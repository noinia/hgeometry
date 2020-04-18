{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Triangle where

import           Control.DeepSeq
import           Control.Lens
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Either (partitionEithers)
import           Data.Ext
import           Data.Geometry.Ball (Disk, disk)
import           Data.Geometry.Boundary
import           Data.Geometry.HalfSpace
import           Data.Geometry.HyperPlane
import           Data.Geometry.Line
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import qualified Data.Geometry.Vector as V
import qualified Data.List as List
import           Data.Maybe (mapMaybe)
import           Data.Util
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           GHC.Generics (Generic)
import           GHC.TypeLits

--------------------------------------------------------------------------------

-- | Triangles in \(d\)-dimensional space.
data Triangle d p r = Triangle !(Point d r :+ p)
                               !(Point d r :+ p)
                               !(Point d r :+ p)
                      deriving (Generic)

deriving instance (Arity d, Show r, Show p)     => Show   (Triangle d p r)
deriving instance (Arity d, Read r, Read p)     => Read   (Triangle d p r)
deriving instance (Arity d, Eq r, Eq p)         => Eq     (Triangle d p r)

instance (Arity d, NFData r, NFData p) => NFData (Triangle d p r)

instance Arity d => Bifunctor  (Triangle d) where bimap = bimapDefault
instance Arity d => Bifoldable (Triangle d) where bifoldMap = bifoldMapDefault

instance Arity d => Bitraversable (Triangle d) where
  bitraverse f g (Triangle p q r) = let tr = bitraverse (traverse g) f in
    Triangle <$> tr p <*> tr q <*> tr r

-- instance Arity d => Functor (Triangle d p) where
--   fmap f (Triangle p q r) = let f' = first (fmap f) in Triangle (f' p) (f' q) (f' r)

instance Field1 (Triangle d p r) (Triangle d p r) (Point d r :+ p) (Point d r :+ p) where
  _1 = lens (\(Triangle p _ _) -> p) (\(Triangle _ q r) p -> Triangle p q r)
instance Field2 (Triangle d p r) (Triangle d p r) (Point d r :+ p) (Point d r :+ p) where
  _2 = lens (\(Triangle _ q _) -> q) (\(Triangle p _ r) q -> Triangle p q r)
instance Field3 (Triangle d p r) (Triangle d p r) (Point d r :+ p) (Point d r :+ p) where
  _3 = lens (\(Triangle _ _ r) -> r) (\(Triangle p q _) r -> Triangle p q r)

type instance NumType   (Triangle d p r) = r
type instance Dimension (Triangle d p r) = d

_TriangleThreePoints :: Iso' (Triangle d p r) (Three (Point d r :+ p))
_TriangleThreePoints = iso (\(Triangle p q r) -> Three p q r) (\(Three p q r) -> Triangle p q r)

instance PointFunctor (Triangle d p) where
  pmap f (Triangle p q r) = Triangle (p&core %~ f) (q&core %~ f) (r&core %~ f)

instance (Fractional r, Arity d, Arity (d + 1)) => IsTransformable (Triangle d p r) where
  transformBy = transformPointFunctor

-- | convenience function to construct a triangle without associated data.
pattern Triangle' :: Point d r -> Point d r -> Point d r -> Triangle d () r
pattern Triangle' p q r <- Triangle (p :+ ()) (q :+ ()) (r :+ ())
  where
    Triangle' p q r = Triangle (ext p) (ext q) (ext r)


sideSegments                  :: Triangle d p r -> [LineSegment d p r]
sideSegments (Triangle p q r) =
  [ClosedLineSegment p q, ClosedLineSegment q r, ClosedLineSegment r p]

-- | Compute the area of a triangle
area   :: Fractional r => Triangle 2 p r -> r
area t = doubleArea t / 2

-- | 2*the area of a triangle.
doubleArea                  :: Num r => Triangle 2 p r -> r
doubleArea (Triangle a b c) = abs $ ax*by - ax*cy
                                  + bx*cy - bx*ay
                                  + cx*ay - cx*by
                                  -- Based on determinant of a 3x3 matrix (shoelace formula)
  where
    Point2 ax ay = a^.core
    Point2 bx by = b^.core
    Point2 cx cy = c^.core

-- | Checks if the triangle is degenerate, i.e. has zero area.
isDegenerateTriangle :: (Num r, Eq r) => Triangle 2 p r -> Bool
isDegenerateTriangle = (== 0) . doubleArea

-- | get the inscribed disk. Returns Nothing if the triangle is degenerate,
-- i.e. if the points are colinear.
inscribedDisk                  :: (Eq r, Fractional r)
                               => Triangle 2 p r -> Maybe (Disk () r)
inscribedDisk (Triangle p q r) = disk (p^.core) (q^.core) (r^.core)


instance Num r => HasSupportingPlane (Triangle 3 p r) where
  supportingPlane (Triangle p q r) = from3Points (p^.core) (q^.core) (r^.core)


-- | Given a point q and a triangle, q inside the triangle, get the baricentric
-- cordinates of q
toBarricentric                                 :: Fractional r
                                               => Point 2 r -> Triangle 2 p r
                                               -> Vector 3 r
toBarricentric (Point2 qx qy) (Triangle a b c) = Vector3 alpha beta gamma
  where
    Point2 ax ay = a^.core
    Point2 bx by = b^.core
    Point2 cx cy = c^.core

    dett  = (by - cy)*(ax - cx) + (cx - bx)*(ay - cy)

    alpha = ((by - cy)*(qx - cx) + (cx - bx)*(qy - cy)) / dett
    beta  = ((cy - ay)*(qx - cx) + (ax - cx)*(qy - cy)) / dett
    gamma = 1 - alpha - beta
    -- see https://en.wikipedia.org/wiki/Barycentric_coordinate_system#Conversion_between_barycentric_and_Cartesian_coordinates

-- | Given a vector of barricentric coordinates and a triangle, get the
-- corresponding point in the same coordinate sytsem as the vertices of the
-- triangle.
fromBarricentric                                  :: (Arity d, Num r)
                                                  => Vector 3 r -> Triangle d p r
                                                  -> Point d r
fromBarricentric (Vector3 a b c) (Triangle p q r) = let f = view (core.vector) in
    Point $ a *^ f p ^+^ b *^ f q ^+^ c *^ f r


-- | Tests if a point lies inside a triangle, on its boundary, or outside the triangle
inTriangle     :: (Ord r, Fractional r)
                 => Point 2 r -> Triangle 2 p r -> PointLocationResult
inTriangle q t
    | all (`inRange` (OpenRange   0 1)) [a,b,c] = Inside
    | all (`inRange` (ClosedRange 0 1)) [a,b,c] = OnBoundary
    | otherwise                                 = Outside
  where
    Vector3 a b c = toBarricentric q t

-- | Test if a point lies inside or on the boundary of a triangle
onTriangle       :: (Ord r, Fractional r)
                 => Point 2 r -> Triangle 2 p r -> Bool
q `onTriangle` t = let Vector3 a b c = toBarricentric q t
                   in all (`inRange` (ClosedRange 0 1)) [a,b,c]


-- myQ :: Point 2 Rational
-- myQ = read "Point2 [(-5985) % 16,(-14625) % 1]"
-- myTri :: Triangle 2 () Rational
-- myTri = read "Triangle (Point2 [(-15) % 1,0 % 1] :+ ()) (Point2 [225 % 2,0 % 1] :+ ()) (Point2 [135 % 1,0 % 1] :+ ())"

type instance IntersectionOf (Line 2 r) (Triangle 2 p r) =
  [ NoIntersection, Point 2 r, LineSegment 2 () r ]

instance (Fractional r, Ord r) => (Line 2 r) `IsIntersectableWith` (Triangle 2 p r) where
   nonEmptyIntersection = defaultNonEmptyIntersection

   l `intersect` (Triangle p q r) =
     case first List.nub . partitionEithers . mapMaybe collect $ sides of
       ([],[])   -> coRec NoIntersection
       (_, [s])  -> coRec $ first (const ()) s
       ([a],_)   -> coRec a
       ([a,b],_) -> coRec $ ClosedLineSegment (ext a) (ext b)
       (_,_)     -> error "intersecting a line with a triangle. Triangle is degenerate"
     where
       sides = [ClosedLineSegment p q, ClosedLineSegment q r, ClosedLineSegment r p]

       collect   :: LineSegment 2 p r -> Maybe (Either (Point 2 r) (LineSegment 2 p r))
       collect s = match (s `intersect` l) $
                        (H $ \NoIntersection           -> Nothing)
                     :& (H $ \(a :: Point 2 r)         -> Just $ Left a)
                     :& (H $ \(e :: LineSegment 2 p r) -> Just $ Right e)
                     :& RNil



type instance IntersectionOf (Line 3 r) (Triangle 3 p r) =
  [ NoIntersection, Point 3 r, LineSegment 3 () r ]

instance (Fractional r, Ord r) => (Line 3 r) `IsIntersectableWith` (Triangle 3 p r) where
   nonEmptyIntersection = defaultNonEmptyIntersection

   l@(Line a v) `intersect` t@(Triangle (p :+ _) (q :+ _) (r :+ _)) =
       match (l `intersect` h) $
            (H $ \NoIntersection   -> coRec NoIntersection)
         :& (H $ \i@(Point3 _ _ _) -> if onTriangle' i then coRec i else coRec NoIntersection)
         :& (H $ \_                -> intersect2d)
         :& RNil
     where
       h@(Plane _ n) = supportingPlane t

       -- 2d triangle and the line in terms of 2d-coordinates wr.t. of a
       -- coordinate system in the supporting plane of t. The origin of this
       -- coordinate system corresponds to the second vertex of t (q)
       t' = Triangle (ext $ project p) (ext origin) (ext $ project r)
       l' = Line (project a) (project' v)

       -- test if the point in terms of its 2d coords lies in side the projected triangle
       onTriangle'                :: Point 3 r -> Bool
       onTriangle' i = (project i) `onTriangle` t'

       -- FIXME! these vectors may not be unit vectors. How do we deal with
       -- that? (and does that really matter here?)
       transf :: Transformation 3 r
       transf = let u = p .-. q
                in rotateTo (Vector3 u (n `cross` u) n) |.| translation ((-1) *^ (toVec q))
       -- inverse of the transformation above.
       invTrans :: Transformation 3 r
       invTrans = inverseOf transf


       project :: Point 3 r -> Point 2 r
       project = projectPoint . transformBy transf
       project' :: Vector 3 r -> Vector 2 r
       project' = toVec . project . Point

       lift :: Point 2 r -> Point 3 r
       lift = Point . transformBy invTrans . flip V.snoc 0 . toVec
         -- lift a 2d point back into plane coordinates

       intersect2d :: Intersection (Line 3 r) (Triangle 3 p r)
       intersect2d = match (l' `intersect` t') $
            (H $ \NoIntersection    -> coRec NoIntersection)
         :& (H $ \i@(Point2 _ _)    -> coRec $ lift i)
         :& (H $ \(LineSegment s e) -> coRec $ LineSegment (s&unEndPoint.core %~ lift)
                                                           (e&unEndPoint.core %~ lift))
         :& RNil
