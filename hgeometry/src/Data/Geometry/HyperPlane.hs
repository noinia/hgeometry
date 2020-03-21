{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell  #-}
module Data.Geometry.HyperPlane where

import Control.DeepSeq
import Control.Lens
import Data.Geometry.Line
import Data.Geometry.Point
import Data.Geometry.Properties
import Data.Geometry.Transformation
import Data.Geometry.Vector
import GHC.Generics (Generic)
import GHC.TypeLits

--------------------------------------------------------------------------------

-- | Hyperplanes embedded in a \(d\) dimensional space.
data HyperPlane (d :: Nat) (r :: *) = HyperPlane { _inPlane   :: !(Point d r)
                                                 , _normalVec :: !(Vector d r)
                                                 } deriving Generic
makeLenses ''HyperPlane

type instance Dimension (HyperPlane d r) = d
type instance NumType   (HyperPlane d r) = r

deriving instance (Arity d, Show r)   => Show    (HyperPlane d r)
deriving instance (NFData r, Arity d) => NFData  (HyperPlane d r)
deriving instance Arity d => Functor     (HyperPlane d)
deriving instance Arity d => Foldable    (HyperPlane d)
deriving instance Arity d => Traversable (HyperPlane d)

instance (Arity d, Eq r, Fractional r) => Eq (HyperPlane d r) where
  (HyperPlane p u) == h@(HyperPlane _ v) = p `intersects` h && u `isScalarMultipleOf` v


instance (Arity d, Arity (d + 1), Fractional r) => IsTransformable (HyperPlane d r) where
  transformBy t (HyperPlane p v) = HyperPlane (transformBy t p) (transformBy t v)

--------------------------------------------------------------------------------

type instance IntersectionOf (Point d r) (HyperPlane d r) = [NoIntersection, Point d r]

instance (Num r, Eq r, Arity d) => Point d r `IsIntersectableWith` HyperPlane d r where
  nonEmptyIntersection = defaultNonEmptyIntersection
  q `intersects` (HyperPlane p n) = n `dot` (q .-. p) == 0

  q `intersect` h | q `intersects` h = coRec q
                  | otherwise        = coRec NoIntersection




-- -- | Compute a transformation that maps the last dimension (i.e. the d-axis) to
-- -- the normal vector of the plane. The origin of the coordinate system will
-- -- correspond to the inPlane point.
-- changeCoordinateSystem                  :: Floating r => HyperPlane 3 r -> Vector 3 r
--                                         -> Transformation 3 r
-- changeCoordinateSystem (HyperPlane p n) u = rotateTo (Vector3 u v w)
--                                        |.| translation (origin .-. p)
--   where
--     v = undefined
--     w = normalize n

-- toPlaneCoordinates :: HyperPlane d r ->

--------------------------------------------------------------------------------
-- * 3 Dimensional planes

type Plane = HyperPlane 3

pattern Plane     :: Point 3 r -> Vector 3 r -> Plane r
pattern Plane p n = HyperPlane p n
{-# COMPLETE Plane #-}

-- | Produces a plane. If r lies counter clockwise of q w.r.t. p then
-- the normal vector of the resulting plane is pointing "upwards".
--
-- >>> from3Points origin (Point3 1 0 0) (Point3 0 1 0)
-- HyperPlane {_inPlane = Point3 [0,0,0], _normalVec = Vector3 [0,0,1]}
from3Points       :: Num r => Point 3 r -> Point 3 r -> Point 3 r -> HyperPlane 3 r
from3Points p q r = let u = q .-. p
                        v = r .-. p
                    in HyperPlane p (u `cross` v)

instance OnSideUpDownTest (Plane r) where
  -- >>> (Point3 5 5 5) `onSideUpDown` from3Points origin (Point3 1 0 0) (Point3 0 1 0)
  -- Above
  -- >>> (Point3 5 5 (-5)) `onSideUpDown` from3Points origin (Point3 1 0 0) (Point3 0 1 0)
  -- Below
  -- >>> (Point3 5 5 0) `onSideUpDown` from3Points origin (Point3 1 0 0) (Point3 0 1 0)
  -- On
  q `onSideUpDown` (Plane p n) = let v = q .-. p in case (n `dot` v) `compare` 0 of
                                   LT -> Below
                                   EQ -> On
                                   GT -> Above

type instance IntersectionOf (Line 3 r) (Plane r) = [NoIntersection, Point 3 r, Line 3 r]

instance (Eq r, Fractional r) => (Line 3 r) `IsIntersectableWith` (Plane r) where
  nonEmptyIntersection = defaultNonEmptyIntersection
  l@(Line p v) `intersect` (HyperPlane q n)
      | denum == 0 = if num == 0 then coRec l else coRec NoIntersection
      | otherwise  = coRec $ p .+^ (num / denum) *^ v
    where
      num   = (q .-. p) `dot` n
      denum = v `dot` n
    -- see https://en.wikipedia.org/wiki/Line%E2%80%93plane_intersection


-- * Lines

-- | Convert between lines and hyperplanes
_asLine :: Num r => Iso' (HyperPlane 2 r) (Line 2 r)
_asLine = iso hyperplane2line line2hyperplane
  where
    hyperplane2line (HyperPlane p n) = perpendicularTo $ Line p n
    line2hyperplane l = let Line p n = perpendicularTo l in HyperPlane p ((-1) *^ n)

--------------------------------------------------------------------------------
-- * Supporting Planes

-- | Types for which we can compute a supporting hyperplane, i.e. a hyperplane
-- that contains the thing of type t.
class HasSupportingPlane t where
  supportingPlane :: t -> HyperPlane (Dimension t) (NumType t)

instance HasSupportingPlane (HyperPlane d r) where
  supportingPlane = id


-- | Given
-- * a plane,
-- * a unit vector in the plane that will represent the y-axis (i.e. the "view up" vector), and
-- * a point in the plane,
--
-- computes the plane coordinates of the given point, using the
-- inPlane point as the origin, the normal vector of the plane as the
-- unit vector in the "z-direction" and the view up vector as the
-- y-axis.
--
-- >>> planeCoordinatesWith (Plane origin (Vector3 0 0 1)) (Vector3 0 1 0) (Point3 10 10 0)
-- Point2 [10.0,10.0]
planeCoordinatesWith       :: Fractional r => Plane r -> Vector 3 r -> Point 3 r -> Point 2 r
planeCoordinatesWith h vup = projectPoint . transformBy (planeCoordinatesTransform h vup)

planeCoordinatesTransform                    :: Num r => Plane r -> Vector 3 r -> Transformation 3 r
planeCoordinatesTransform (HyperPlane o n) v =   rotateTo (Vector3 (v `cross` n) v n)
                                             |.| translation ((-1) *^ toVec o)
