{-# LANGUAGE  UndecidableInstances  #-}
module TriangleUnboundedConvexSpec where

import           Control.Lens
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Golden
import           HGeometry.HalfSpace
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Polygon.Convex
import           HGeometry.Properties
import           HGeometry.Triangle
import           HGeometry.Vector
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type UnboundedConvexRegion point = UnboundedConvexRegionF (NumType point) NonEmpty point

-- | An unbounded ConvexRegion
data UnboundedConvexRegionF r nonEmpty point =
  Unbounded (Vector 2 r)
            -- ^ vector indicating the direction of the unbounded edge
            -- incident to the first vertex. Note that this vector
            -- thus points INTO vertex v.
            (nonEmpty point)
            -- ^ the vertices in CCW order,
            (Vector 2 r)
            -- ^ the vector indicating the direction of the unbounded
            -- edge incident to the last vertex. The vector points
            -- away from the vertex (i.e. towards +infty).
  deriving stock (Show,Eq,Functor,Foldable,Traversable)


type instance NumType   (UnboundedConvexRegionF r nonEmpty point) = r
type instance Dimension (UnboundedConvexRegionF r nonEmpty point) = 2

-- | Compute the first and last vertex of the chain. Returns a Left if the first and last
-- are the same.
extremalVertices :: UnboundedConvexRegionF r NonEmpty point -> Either point (Vector 2 point)
extremalVertices = \case
  Unbounded _ (p :| pts) _ -> case NonEmpty.nonEmpty pts of
                                Nothing   -> Left p
                                Just pts' -> Right $ Vector2 p (NonEmpty.last pts')

--
--
-- note: this creates two new vertices; which are "copies" from the extremal vertices.
-- this is to avoid having to introduce yet another level of 'OriginalOrExtra''s
toBoundedFrom :: (Foldable1 nonEmpty, Point_ point 2 r, Point_ point' 2 r, Ord r, Fractional r
                 )
              => nonEmpty point' -> UnboundedConvexRegionF r NonEmpty point
              -> ConvexPolygonF NonEmpty point
toBoundedFrom tri reg@(Unbounded v pts w) = case extremalVertices reg of
    Right (Vector2 p q) -> let l@(LinePV _ u) = lineThrough p q
                               h              = HalfSpace Positive l
                           in compute p q h u
    Left p              -> let bisec = w ^+^ negated v -- note that v is pointing in the wrong dir.
                               u     = rot90 bisec -- perpendicular to bisec
                               h     = HalfSpace Positive (fromPointAndVec p u)
                           in compute p p h u
  where
    -- given the halfpsace h that goes through p and q (the extremal vertices),
    -- and the direction u of its bounding line.
    --
    -- computes two points a and b on the halflines so that tri is contained in the halfspace
    -- defined by a and b (that contains p and q).
    --
    -- it returns a clipped version of the bounded region with a and b as vertices.
    compute p q h u = let s = view asPoint $ maximumOn (`squaredEuclideanDistTo` h) tri
                          -- distance to the point furthest from this halfspace.
                          -- we then create two additional on the halflines
                          -- that are at least that distance away.
                          a = case (fromPointAndVec @(LinePV 2 _) p v) `intersect` (LinePV s u) of
                                Just (Line_x_Line_Point a') -> p&xCoord .~ a'^.xCoord
                                                                &yCoord .~ a'^.yCoord
                                _                           -> error "absurd; a'"
                          b = case (fromPointAndVec @(LinePV 2 _) q w) `intersect` (LinePV s u) of
                                Just (Line_x_Line_Point b') -> q&xCoord .~ b'^.xCoord
                                                                &yCoord .~ b'^.yCoord
                                _                           -> error "absurd; b'"
                      in uncheckedFromCCWPoints (b NonEmpty.<| a NonEmpty.<| pts)


    maximumOn f = maximumBy (comparing f)
    rot90 (Vector2 x y) = Vector2 (-y) x

instance ( HasSquaredEuclideanDistance boundingHyperPlane
         , HasIntersectionWith (Point d r) (HalfSpaceF boundingHyperPlane)
         , d ~ Dimension boundingHyperPlane, r ~ NumType boundingHyperPlane
         )
         => HasSquaredEuclideanDistance (HalfSpaceF boundingHyperPlane) where
  pointClosestTo (view asPoint -> q) h
    | q `intersects` h = q
    | otherwise        = pointClosestTo q (h^.boundingHyperPlane)

--------------------------------------------------------------------------------




type instance Intersection (Triangle corner) (UnboundedConvexRegionF r nonEmpty point)
  = Intersection (Triangle corner) (ConvexPolygonF nonEmpty point)

instance (Point_ point 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `HasIntersectionWith` (UnboundedConvexRegionF r NonEmpty point)

instance ( Point_ point 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `IsIntersectableWith` (UnboundedConvexRegionF r NonEmpty point) where
  tri `intersect` region = tri `intersect` (toBoundedFrom tri region)


--------------------------------------------------------------------------------




type R = RealNumber 5





spec :: Spec
spec = describe "triangle x unbounde convex polygon intersection" $ do
         it "manu al test" $
           (myTriangle `intersects` upperQuadrant) `shouldBe` True


myTriangle :: Triangle (Point 2 R)
myTriangle = Triangle origin (Point2 100 0) (Point2 0 100)


upperQuadrant :: UnboundedConvexRegion (Point 2 R)
upperQuadrant = Unbounded (Vector2 0 (-1)) (origin :| []) (Vector2 1  0)
