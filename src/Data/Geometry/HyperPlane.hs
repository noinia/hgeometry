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

data HyperPlane (d :: Nat) (r :: *) = HyperPlane { _inPlane   :: !(Point d r)
                                                 , _normalVec :: !(Vector d r)
                                                 } deriving Generic
makeLenses ''HyperPlane

type instance Dimension (HyperPlane d r) = d
type instance NumType   (HyperPlane d r) = r

deriving instance (Arity d, Show r)   => Show    (HyperPlane d r)
deriving instance (Arity d, Eq r)     => Eq      (HyperPlane d r)
deriving instance (NFData r, Arity d) => NFData  (HyperPlane d r)
deriving instance Arity d => Functor     (HyperPlane d)
deriving instance Arity d => Foldable    (HyperPlane d)
deriving instance Arity d => Traversable (HyperPlane d)

instance (Arity d, Arity (d + 1), Fractional r) => IsTransformable (HyperPlane d r) where
  transformBy t (HyperPlane p v) = HyperPlane (transformBy t p) (transformBy t v)

--------------------------------------------------------------------------------

-- | Test if a point lies on a hyperplane.
onHyperPlane                      :: (Num r, Eq r, Arity d)
                                  => Point d r -> HyperPlane d r -> Bool
q `onHyperPlane` (HyperPlane p n) = n `dot` (q .-. p) == 0


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


from3Points       :: Num r => Point 3 r -> Point 3 r -> Point 3 r -> HyperPlane 3 r
from3Points p q r = let u = q .-. p
                        v = r .-. p
                    in HyperPlane p (u `cross` v)


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


--------------------------------------------------------------------------------
-- * Supporting Planes

-- | Types for which we can compute a supporting hyperplane, i.e. a hyperplane
-- that contains the thing of type t.
class HasSupportingPlane t where
  supportingPlane :: t -> HyperPlane (Dimension t) (NumType t)

instance HasSupportingPlane (HyperPlane d r) where
  supportingPlane = id
