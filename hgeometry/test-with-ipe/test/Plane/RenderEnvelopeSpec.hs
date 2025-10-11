{-# LANGUAGE QuasiQuotes #-}
module Plane.RenderEnvelopeSpec
  where

import Data.Foldable1.WithIndex
import Data.Semialign
import Prelude hiding (zipWith)
import Data.Map.NonEmpty qualified as NEMap
import Data.Coerce
import Data.Foldable1
import Control.Lens
import Test.Hspec
import Test.Hspec.QuickCheck
import Ipe
import Ipe.Color
import System.OsPath
import Data.Map qualified as Map
import Test.Hspec.WithTempFile
import Golden
import HGeometry.Plane.LowerEnvelope.Connected.BruteForce qualified as BruteForce
import HGeometry.HyperPlane.NonVertical
import HGeometry.Instances ()
import HGeometry.Ext
import HGeometry.LineSegment
import HGeometry.Polygon
import HGeometry.Polygon.Convex.Instances ()
import HGeometry.Number.Real.Rational
import HGeometry.Plane.LowerEnvelope
import HGeometry.VoronoiDiagram
import HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane)
import HGeometry.Triangle
import Data.Sequence qualified as Seq
import HGeometry.PlaneGraph
import HGeometry.Point
import HGeometry.VoronoiDiagram qualified as VD
import HGeometry.Box as Box
import HGeometry.Map.NonEmpty.Monoidal (MonoidalNEMap)
import HGeometry.Map.NonEmpty.Monoidal qualified as MonoidalNEMap
import HGeometry.Vector
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.Set.NonEmpty (NESet)
import HGeometry.Transformation
import HGeometry.Intersection
import HGeometry.Graphics.Camera
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Ipe.Color
import HGeometry.LineSegment.Intersection.BentleyOttmann
import Hiraffe.PlanarGraph.Connected
import Data.Map.Monoidal qualified as MonoidalMap
import HGeometry.Sequence.NonEmpty (ViewL1(..), asViewL1, singletonL1)

import PlaneGraph.RenderSpec
--------------------------------------------------------------------------------

type R = RealNumber 5

triangles :: [Triangle (Point 3 Double) :+ IpeColor Double]
triangles = -- scaleUniformlyBy 5 <$>
        [ -- ground plane
          Triangle origin (Point3 1 0 0) (Point3 1 1 0) :+ blue
        , Triangle origin (Point3 1 1 0) (Point3 0 1 0) :+ blue
        -- left side
        , Triangle origin (Point3 0 1 0) (Point3 0 1 1) :+ green
        , Triangle origin (Point3 0 1 1) (Point3 0 0 1) :+ green
        -- front plane
        -- , Triangle origin (Point3 1 0 0) (Point3 1 0 1) :+ red
        -- , Triangle origin (Point3 1 0 1) (Point3 0 1 1) :+ red

        -- back plane
        , Triangle (Point3 0 1 0) (Point3 1 1 0) (Point3 1 1 1) :+ orange
        , Triangle (Point3 0 1 0) (Point3 1 1 1) (Point3 0 1 1) :+ orange
        ] <> ((\tri -> tri&extra %~ getColor
                         &vertices.coordinates %~ realToFrac
              ) <$> myTriangles
             )

getColor :: core :+ IpeColor Double -> IpeColor Double
getColor = view extra

myTriangles :: [Triangle (Point 3 R) :+ (Plane R :+ IpeColor Double)]
myTriangles = asTrianglesAbove domain planes

planes :: NonEmpty (Plane R :+ IpeColor Double)
planes = NonEmpty.fromList
           [ Plane 0 0 (0.5)   :+ red
           , Plane 0 (-0.25) 1 :+ gray
           ]
-- planes = points&mapped.core %~ pointToPlane

points :: NonEmpty (Point 2 R :+ IpeColor Double)
points = NonEmpty.fromList
         [ Point2 (1/2) (1/3) :+ red
         ]


myCamera :: Camera Double
myCamera = blenderCamera


-- | fit to something that fits in this rectangle
screenBox :: Rectangle (Point 2 Double)
screenBox = Rectangle origin (Point2 500 500)


-- | The domain in which we want to render the planes
domain :: Rectangle (Point 2 R)
domain = Rectangle origin (Point2 2 1)

-- | Given a rectangular domain, and a set of planes, generates the
-- triangles that represent the planes above the domain.
asTrianglesAbove      :: (Plane_ plane r, Num r, Foldable f)
                      => Rectangle (Point 2 r)
                      -> f plane -> [Triangle (Point 3 r) :+ plane]
asTrianglesAbove rect = foldMap (foldMap (:[]) . asTrianglePairAbove rect)

-- | Given a rectangular domain, and a plane h, generate two triangles
-- that represent the plane above the domain.
asTrianglePairAbove        :: (Plane_ plane r, Num r)
                           => Rectangle (Point 2 r)
                           -> plane -> Vector 2 (Triangle (Point 3 r) :+ plane)
asTrianglePairAbove rect h = Vector2 (Triangle tl tr br :+ h)
                                     (Triangle tl br bl :+ h)
  where
    Corners tl tr br bl = (\p@(Point2 x y) -> Point3 x y (evalAt p h)) <$> Box.corners rect

-- todo; just map to polygons/quads directly

-- theTransform =

-- instance IsBoxable (IpeObject r) where
--   boundingBox = \case
--     IpeGroup g     -> boundingBox g
--     IpeImage i     -> boundingBox i
--     IpeTextLabel l -> boundingBox l
--     IpeMiniPage p  -> boundingBox p
--     IpeUse u       -> boundingBox u
--     IpePath p      -> boundingBox p

spec :: Spec
spec = describe "Plane.RenderEnvelope"  $ do
         goldenWith [osp|data/test-with-ipe/golden/PlaneGraph/|]
              (ipeFileGolden { name = [osp|planeGraphFromIntersectingSegments|]
                             }
              )
              ( let myTriangles2 :: NonEmpty (Triangle (Point 2 R))
                    myTriangles2 = NonEmpty.fromList
                      [ Triangle origin (Point2 100 0) (Point2 100 100)
                      , Triangle (Point2 10 (-5)) (Point2 20 (-5)) (Point2 20 200)
                      ]
                    mySegments = foldMap1 (toNonEmptyOf outerBoundaryEdgeSegments) myTriangles2
                    myPlaneGraph = fromIntersectingSegments mySegments

                    content' = drawGraph myPlaneGraph
                in addStyleSheet opacitiesStyle $ singlePageFromContent content'
              )


         goldenWith [osp|data/test-with-ipe/golden/Plane/|]
           (ipeFileGolden { name = [osp|lowerEnvelopeRender|]
                          }
           )
           ( let content' = let tris = renderToIpe myCamera triangles
                                t    = uniformScaling 1000
                                      --- fitToBoxTransform screenBox  tris -- TODO
                            in transformBy t tris
             in addStyleSheet opacitiesStyle $ singlePageFromContent content'
           )


--------------------------------------------------------------------------------


-- newtype PolygonEdge = PolygonEdge
--   (ClosedLineSegment vertex )


-- in the end this should be a planeGraph rather than a

-- | Construct a connected Plane Graph out of a bunch of polygons
constructCPlaneGraph      :: forall nonEmpty s simplePolygon vertex r.
                               (Foldable1 nonEmpty, Point_ vertex 2 r
                               , SimplePolygon_ simplePolygon vertex r
                               )
                          => nonEmpty simplePolygon
                          -> CPlaneGraph s (Point 2 r :+ [vertex]) () [simplePolygon]
constructCPlaneGraph tris = undefined
  -- where
  --   allEdgeSegments = foldMap1

theIntersections :: forall nonEmpty s simplePolygon vertex r.
                      (Foldable1 nonEmpty, Point_ vertex 2 r
                        , SimplePolygon_ simplePolygon vertex r
                        , Ord r, Fractional r, Ord vertex
                      )
                 => nonEmpty simplePolygon
                 -> Intersections r (ClosedLineSegment vertex)
theIntersections = intersections . foldMap1 (toNonEmptyOf outerBoundaryEdgeSegments)


-- | Assign each element in the map a unique Integer key (in the range \([0,n)\) )
assignIndex :: MonoidalNEMap k v -> MonoidalNEMap k Int
assignIndex = snd . MonoidalNEMap.mapAccumWithKey (\i k _ -> (succ i, i)) 0


(<>>)        :: (Ord k, Semigroup v) => MonoidalNEMap k v -> Map.Map k v -> MonoidalNEMap k v
base <>> new = foldr (uncurry MonoidalNEMap.insert) base $ Map.toAscList new


myTriangles2 :: NonEmpty (Triangle (Point 2 R))
myTriangles2 = NonEmpty.fromList
              [ Triangle origin (Point2 100 0) (Point2 100 100)
              , Triangle (Point2 10 (-5)) (Point2 20 (-5)) (Point2 20 200)
              ]
mySegments = foldMap1 (toNonEmptyOf outerBoundaryEdgeSegments) myTriangles2

foo = MonoidalNEMap.assocs $
  interiorIntersectionsBySegment mySegments (intersections mySegments)

bar = fromIntersectingSegments mySegments


-- | Computes the interior intersections on each segment.
--
-- O((n+k)\log n)
interiorIntersectionsBySegment                    :: ( Ord lineSegment
                                                     , Foldable1 nonEmpty
                                                     )
                                                  => nonEmpty lineSegment
                                                     -- ^ all input segments
                                                  -> Intersections r lineSegment
                                                  -> MonoidalNEMap lineSegment (Seq.Seq (Point 2 r))
interiorIntersectionsBySegment segs intersections =
        foldMap1 (flip MonoidalNEMap.singleton mempty) segs
    <>> coerce (ifoldMap construct intersections)
  where
    construct p assoc = foldMap (\seg -> MonoidalMap.singleton (coerce seg) (Seq.singleton p))
                                (assoc^.interiorTo)


-- |  Construct A connected PlaneGraph from a set of intersecting segments.
--
-- \( O((n+k)\log n) \), where \(n\) is the number of segments, and \(k\) is the number
-- of intersections.
--
-- pre: the segments actually form a connected graph.
fromIntersectingSegments                 :: forall s nonEmpty lineSegment r point planeGraph.
                                       ( Foldable1 nonEmpty, Functor nonEmpty
                                       , LineSegment_ lineSegment point
                                       , Point_ point 2 r, Ord r, Fractional r
                                       , Intersection lineSegment lineSegment
                                         ~ Maybe (LineSegmentLineSegmentIntersection lineSegment)
                                       , IsIntersectableWith lineSegment lineSegment
                                       , Eq lineSegment
                                       , OrdArounds lineSegment
                                       , Ord lineSegment
                                       , HasOnSegment lineSegment 2
                                       , StartPointOf lineSegment ~ EndPointOf lineSegment

                                       , planeGraph ~ CPlaneGraph s (Point 2 r)
                                                                    (ViewL1 lineSegment)
                                                                    ()
                                       )
                              => nonEmpty lineSegment
                              -> planeGraph
fromIntersectingSegments segs = fromIntersections segs (intersections segs)

-- |  Construct A connected PlaneGraph from a set of intersecting segments.
--
-- \( O((n+k)\log n) \), where \(n\) is the number of segments, and \(k\) is the number
-- of intersections.
--
-- pre: the segments actually form a connected graph.
fromIntersections                 :: forall s nonEmpty lineSegment r point planeGraph.
                                       ( Foldable1 nonEmpty
                                       , LineSegment_ lineSegment point
                                       , Point_ point 2 r, Ord r, Num r
                                       , Intersection lineSegment lineSegment
                                         ~ Maybe (LineSegmentLineSegmentIntersection lineSegment)
                                       , IsIntersectableWith lineSegment lineSegment
                                       , Eq lineSegment
                                       , OrdArounds lineSegment
                                       , Ord lineSegment

                                       , planeGraph ~ CPlaneGraph s (Point 2 r)
                                                                    (ViewL1 lineSegment)
                                                                    ()
                                       )
                                  => nonEmpty lineSegment
                                  -> Intersections r lineSegment
                                  -> planeGraph
fromIntersections segs intersects = fromAdjacencyLists adjLists
  where
    -- | Map every Point to its vertexId
    vertexMapping :: MonoidalNEMap (Point 2 r) (VertexIx planeGraph)
    vertexMapping = coerce $ assignIndex vertexLocations

    vertexLocations :: MonoidalNEMap (Point 2 r) (Associated lineSegment)
    vertexLocations = foldMap1 (\seg -> MonoidalNEMap.singleton (seg^.start.asPoint) (mkAroundStart seg)
                                     <> MonoidalNEMap.singleton (seg^.end.asPoint)   (mkAroundEnd seg)
                               ) segs
                    <>> intersects

    -- | Computes the vertices along each segment
    verticesBySegment :: MonoidalNEMap lineSegment (ViewL1 (VertexIx planeGraph))
    verticesBySegment = imap collect $ interiorIntersectionsBySegment segs intersects
      where
        collect seg interiorPts = (vertexMapping MonoidalNEMap.!) <$>
              (seg^.start.asPoint) :<< (interiorPts' Seq.|> seg^.end.asPoint)
          where
            interiorPts' = Seq.sortOn alongSegment interiorPts
            -- | We want to sort the points, which all lie on the segment, along the segment
            -- to this end we essentially compare their distance to the starting point.
            -- however, instead of explicitly computing this distance, it suffices to compare
            -- the x-coordinates or y-coordiantes of the points themselves, depending on the
            -- orientation of the segment. (since a segment is xy-monotone)
            alongSegment :: Point 2 r -> r
            alongSegment = case (seg^.start.xCoord) `compare` (seg^.end.xCoord) of
              LT                                        -> view xCoord
              GT                                        -> negate . view xCoord
              EQ | seg^.start.yCoord <= seg^.end.yCoord -> view yCoord
                 | otherwise                            -> negate . view yCoord

    -- | For every vertex, collect its neighbours
    neighbours :: MonoidalNEMap (VertexIx planeGraph)
                                (MonoidalNEMap (VertexIx planeGraph) (ViewL1 lineSegment))
    neighbours = ifoldMap1 collect verticesBySegment
      where
        collect seg verts@(_ :<< rest') = case asViewL1 rest' of
            Nothing   ->
              error "fromIntersections. absurd. every seg should have at least 2 vertices"
            Just rest -> fold1 $ zipWith f verts rest
          where
            f u v = MonoidalNEMap.singleton u (MonoidalNEMap.singleton v $ singletonL1 seg)
                 <> MonoidalNEMap.singleton v (MonoidalNEMap.singleton u $ singletonL1 seg)

    -- I think this already automatically takes care of colinear semgnets as well, as we
    -- are using a NESet to collect the neighbours of each vertex.


    -- | Construct the final adjacency lists
    adjLists :: MonoidalNEMap _
                  (VertexIx planeGraph, Point 2 r, NonEmpty ( VertexIx planeGraph
                                                            , ViewL1 lineSegment
                                                            )
                  )
    adjLists = imap buildVertex vertexMapping
    buildVertex v vi = (vi, v, MonoidalNEMap.assocs $ neighbours MonoidalNEMap.! vi)


--------------------------------------------------------------------------------

renderToIpe       :: forall point r r' f.
                       (Foldable f, Functor f, Point_ point 3 r, Real r, Real r')
                   => Camera r -> f (Triangle point :+ IpeColor r') -> [IpeObject Double]
renderToIpe camera = foldMap asIpe . render camera
  where
    asIpe (triangle :+ col) = [iO $ defIO triangle ! attr SFill (realToFrac <$> col)]

-- | Render a scene; i..e a set of triangles
render        :: forall point r r' f. (Functor f, Point_ point 3 r, Real r)
              => Camera r
              -> f (Triangle point :+ IpeColor r')
              -> f (Triangle (Point 2 Double) :+ IpeColor r')
render camera = over (mapped.core) $ \triangle ->
    triangle&vertices %~
            projectPoint . transformBy (cameraTransform camera') . f
  where
    camera' = realToFrac <$> camera
    f   :: point -> Point 3 Double
    f p = over coordinates realToFrac (p^.asPoint)
