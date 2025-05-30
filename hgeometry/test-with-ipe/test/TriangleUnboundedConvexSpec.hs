{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module TriangleUnboundedConvexSpec where

import           Control.Lens as Lens
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Ord (comparing)
import           Golden
import           HGeometry.Ext
import           HGeometry.HalfSpace
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.PolyLine
import           HGeometry.Polygon
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Properties
import           HGeometry.Triangle
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import           Test.QuickCheck.Instances ()

import           Debug.Trace
--------------------------------------------------------------------------------

-- | An unbounded polygonal Convex Region
type UnboundedConvexRegion vertex = UnboundedConvexRegionF (NumType vertex) NonEmpty vertex

-- | An unbounded polygonal ConvexRegion whose vertices are stored in an 'nonEmpty'
data UnboundedConvexRegionF r nonEmpty vertex =
  Unbounded (Vector 2 r)
            -- ^ vector indicating the direction of the unbounded edge
            -- incident to the first vertex. Note that this vector
            -- thus points INTO vertex v.
            (nonEmpty vertex)
            -- ^ the vertices in CCW order,
            (Vector 2 r)
            -- ^ the vector indicating the direction of the unbounded
            -- edge incident to the last vertex. The vector points
            -- away from the vertex (i.e. towards +infty).
  deriving stock (Show,Eq,Functor,Foldable,Traversable)

type instance NumType   (UnboundedConvexRegionF r nonEmpty vertex) = r
type instance Dimension (UnboundedConvexRegionF r nonEmpty vertex) = 2

-- | map a function over the sequence of points
mapChain                       :: (nonEmpty vertex -> nonEmpty' vertex')
                               -> UnboundedConvexRegionF r nonEmpty vertex
                               -> UnboundedConvexRegionF r nonEmpty' vertex'
mapChain f (Unbounded v pts w) = Unbounded v (f pts) w

-- | Compute the first and last vertex of the chain. Returns a Left if the first and last
-- are the same.
extremalVertices                            :: UnboundedConvexRegionF r NonEmpty vertex
                                            -> Either vertex (Vector 2 vertex)
extremalVertices (Unbounded _ (p :| pts) _) = case NonEmpty.nonEmpty pts of
                                                Nothing   -> Left p
                                                Just pts' -> Right $ Vector2 p (NonEmpty.last pts')

-- | convert to a bounded polygon that contains the points given as the first argument.
--
-- note: this creates two new vertices; which are "copies" from the extremal vertices.
-- this is to avoid having to introduce yet another level of 'OriginalOrExtra''s
toBoundedFrom :: (Foldable nonEmpty, Point_ point 2 r, Point_ vertex 2 r, Ord r, Fractional r
                 )
              => nonEmpty point -> UnboundedConvexRegionF r NonEmpty vertex
              -> ConvexPolygonF NonEmpty vertex
toBoundedFrom tri reg@(Unbounded v pts w) = case extremalVertices reg of
    Right (Vector2 p q) -> let l@(LinePV _ u) = lineThrough p q
                               h              = HalfSpace Negative l
                           in compute p q h u
    Left p              -> let bisec = w ^+^ negated v -- note that v is pointing in the wrong dir.
                               u     = rot90 bisec -- perpendicular to bisec
                               h     = HalfSpace Negative (fromPointAndVec p u)
                           in compute p p h u
  where
    -- given the halfpsace h that goes through p and q (the extremal vertices),
    -- and the direction u of its bounding line.
    --
    -- computes two points a and b on the halflines so that tri is contained in the halfspace
    -- defined by a and b (that contains p and q).
    --
    -- it returns a clipped version of the bounded region with a and b as vertices.
    compute p q h u = let s    = view asPoint $ maximumOn (`squaredEuclideanDistTo` h) tri'
                          -- distance to the point furthest from this halfspace.
                          -- we then create two additional on the halflines
                          -- that are at least that distance away.

                          tri' = (q .+^ w)^.asPoint :| ((^.asPoint) <$> F.toList tri)
                          -- make sure that there is at least one point outside h,
                          -- so that s lies stricly outside h as well

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


-- TODO: this would make for a good property test: test if the points all lie inside the reegion


--------------------------------------------------------------------------------

type instance Intersection (Triangle corner) (UnboundedConvexRegionF r nonEmpty vertex)
  = Intersection (Triangle corner) (ConvexPolygonF nonEmpty vertex)

instance (Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `HasIntersectionWith` (UnboundedConvexRegionF r NonEmpty vertex)

instance ( Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `IsIntersectableWith` (UnboundedConvexRegionF r NonEmpty vertex) where
  tri `intersect` region = tri `intersect` (toBoundedFrom tri region)


--------------------------------------------------------------------------------




type R = RealNumber 5




instance (Num r, Ord r
         ) => HasDefaultFromIpe (UnboundedConvexRegionF r NonEmpty (Point 2 r)) where
  type DefaultFromIpe (UnboundedConvexRegionF r NonEmpty (Point 2 r)) = Path
  defaultFromIpe = ipeUnboundedConvexPolygon

-- | Prism to convert between ipe objects (Paths) and UnboundedConvex regions.
ipeUnboundedConvexPolygon :: forall r. (Num r, Ord r)
                          => Prism' (IpeObject r)
                                    (UnboundedConvexRegionF r NonEmpty (Point 2 r)
                                      :+ IpeAttributes Path r
                                    )
ipeUnboundedConvexPolygon = prism' (IpePath . renderChain) parse
  where
    parse      :: IpeObject r -> Maybe (UnboundedConvexRegionF r NonEmpty (Point 2 r)
                                        :+ IpeAttributes Path r
                                       )
    parse obj = do poly :+ ats  <- obj^?_withAttrs _IpePath _asPolyLine
                   _            <- fromPoints @(ConvexPolygon _) $ poly^._PolyLineF
                   -- makes sure we are convex.
                   _            <- lookupAttr SArrow  ats
                   _            <- lookupAttr SRArrow ats
                   (a:pts1,b)   <- Lens.unsnoc $ poly^..vertices
                   pts@(p:|_)   <- NonEmpty.nonEmpty pts1
                   let q = pts^.last1
                   pure $ Unbounded (p .-. a) pts (b .-. q) :+ ats
                     -- should we zero the arrow attrs?
  -- TODO: I think I may have to reverse the chain; so that the interior is indeed to the left



-- ipeUnboundedConvexPolygon' =

instance (Num r, Point_ vertex 2 r, Foldable1 nonEmpty
         ) => HasDefaultIpeOut (UnboundedConvexRegionF r nonEmpty vertex) where
  type DefaultIpeOut (UnboundedConvexRegionF r nonEmpty vertex) = Path
  defIO = renderChain . (:+ mempty)


renderChain :: (Foldable1 nonEmpty, Point_ vertex 2 r, Num r)
            => UnboundedConvexRegionF r nonEmpty vertex :+ IpeAttributes Path r
            -> IpeObject' Path r
renderChain (reg@(Unbounded v pts w) :+ ats) =
    (poly^.re _asPolyLine) :+     attr SArrow  normalArrow
                               <> attr SRArrow normalArrow
                               <> ats
  where
    poly = case extremalVertices (mapChain toNonEmpty reg) of
             Left p              -> f p p
             Right (Vector2 p q) -> f p q
    f p q = polyLineFromPoints . fmap (^.asPoint)
          $ p .-^ v NonEmpty.<| toNonEmpty pts <> NonEmpty.singleton (q .+^ w)

ipeSpec inFp outFP = do (chain,tri) <- runIO go
                        goldenWith dataPath
                                   (ipeContentGolden { name = outFP })
                                   [ case tri `intersect` chain of
                                       Nothing    -> iO $ ipeLabel ("no intersection" :+ origin)
                                                        ! attr SStroke black
                                       Just inter -> case inter of
                                         DegenerateVertex v -> iO $ defIO (v^.asPoint)
                                                                  ! attr SStroke red
                                         DegenerateEdge e   -> iO $ defIO ((^.asPoint) <$> e)
                                                                  ! attr SStroke red
                                         ActualPolygon poly -> iO $ defIO ((^.asPoint) <$> poly)
                                                                  ! attr SFill red
                                   , iO' chain
                                   , iO' tri
                                   ]
  where
    dataPath = [osp|data/test-with-ipe/Triangle/|]
    go = do [chain :+ _] <- readAllFrom $ dataPath <> inFp
            [tri   :+ _] <- readAllFrom $ dataPath <> inFp
            pure ( chain :: UnboundedConvexRegionF R NonEmpty (Point 2 R)
                 , tri   :: Triangle (Point 2 R)
                 )

spec :: Spec
spec = describe "triangle x unbounded convex polygon intersection" $ do
         it "manual test" $
           (myTriangle `intersects` upperQuadrant) `shouldBe` True
         it "manual test" $
           (touchingTriangle `intersects` upperQuadrant) `shouldBe` True
         it "outside test" $
           (outsideTriangle `intersects` upperQuadrant) `shouldBe` False
         ipeSpec [osp|triangle_x_unbounded.ipe|]
                 [osp|triangle_x_unbounded.out|]
         ipeSpec [osp|triangle_x_unbounded1.ipe|]
                 [osp|triangle_x_unbounded1.out|]
         ipeSpec [osp|triangle_x_cone.ipe|]
                 [osp|triangle_x_cone.out|]
         ipeSpec [osp|triangle_x_cone_no.ipe|]
                 [osp|triangle_x_cone_no.out|]

myTriangle :: Triangle (Point 2 R)
myTriangle = Triangle (Point2 1 1) (Point2 100 0) (Point2 0 100)

outsideTriangle :: Triangle (Point 2 R)
outsideTriangle = Triangle (Point2 (-1) (-10)) (Point2 (-100) (-4)) (Point2 (-1) (-100))

touchingTriangle :: Triangle (Point 2 R)
touchingTriangle = Triangle (Point2 (-1) (-1)) (Point2 100 0) (Point2 0 (-1100))


upperQuadrant :: UnboundedConvexRegion (Point 2 R)
upperQuadrant = Unbounded (Vector2 0 (-1)) (origin :| []) (Vector2 1  0)
