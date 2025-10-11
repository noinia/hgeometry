{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Plane.RenderEnvelopeSpec
  where

import Data.Ord(comparing)
import Data.Default.Class
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
import HGeometry.Polygon.WithHoles
import HGeometry.Foldable.Util
import Plane.Overlay
import HGeometry.Polygon.Simple
import Data.Functor.Apply as Apply
import HGeometry.Properties
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
--
-- The triangles are in counterclockwise order.
asTrianglePairAbove        :: (Plane_ plane r, Num r)
                           => Rectangle (Point 2 r)
                           -> plane -> Vector 2 (Triangle (Point 3 r) :+ plane)
asTrianglePairAbove rect h = Vector2 (Triangle tl br tr :+ h)
                                     (Triangle tl bl br :+ h)
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







instance Default Props where
  def = Props 0 black black

instance Default (Seq.Seq a) where
  -- this is just for drawing to ipe purposes
  def = mempty




fromTriangle :: Point_ vertex 2 r => Triangle vertex -> SimplePolygon vertex
fromTriangle = uncheckedFromCCWPoints
-- TODO: we should just coerce to a SimplePolygonF (Cyclic Vector3)

data Props = Props { heightVal :: {-#UNPACK#-} !Int
                   , _fillColor   :: IpeColor Double
                   , _strokeColor :: IpeColor Double
                   }
  deriving (Show)




spec :: Spec
spec =
    describe "Plane.RenderEnvelope"  $ do
      goldenWith [osp|data/test-with-ipe/golden/PlaneGraph/|]
            (ipeFileGolden { name = [osp|planeGraphFromIntersectingSegments|]
                           }
            )
            ( let myTriangles2 :: NonEmpty (Triangle (Point 2 R) :+ Props)
                  myTriangles2 = NonEmpty.fromList
                    [ Triangle origin (Point2 100 0) (Point2 100 100)            :+ Props 1 red black
                    , Triangle (Point2 10 (-5)) (Point2 20 (-5)) (Point2 20 200) :+ Props 2 blue green
                    ] -- these triangles are indeed in CCW order....
                  myPolygons = myTriangles2&mapped.core %~ fromTriangle
                  myPlaneGraph = polygonOverlay myPolygons
                  content' = renderGraph (firstColor myPlaneGraph)
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

-- | Function to draw a plane graph whose faces may have some color associated them.
--
-- draws the edges in black as well.
renderGraph    :: ( PlaneGraph_ planeGraph vertex, HasOuterBoundaryOf planeGraph
                  , Point_ vertex 2 r, Real r
                  , Face planeGraph ~ Maybe (IpeColor Double)
                  , Edge planeGraph ~ Maybe (IpeColor Double)
                  , Eq (FaceIx planeGraph)
                  , HasInnerComponents planeGraph
                  -- , IsTransformable vertex
                  -- , ConstructablePoint_ vertex 2 r, Ord r, Real r
                  -- , Fractional r, Show r,
                  -- , Show (Vertex planeGraph), Show (Face planeGraph)
                  -- , Show (EdgeIx planeGraph), Show (Edge planeGraph)
                  ) => planeGraph -> [IpeObject Double]
renderGraph gr = theFaces <> theEdges
  where
    theEdges = ifoldMapOf edgeSegments         drawEdge' gr
    theFaces = ifoldMapOf interiorFacePolygons drawFace' gr
    drawFace' fi pg = case gr^?!faceAt fi of
        Nothing    -> []
        Just color -> [ iO $ ipePolygon pg' ! attr SFill color
                      ]
      where
          pg' :: PolygonalDomain _
          pg' = pg&vertices %~ toPoint

    drawEdge' d s = case gr^?!edgeAt d of
        Nothing    -> []
        Just color -> [ iO $ ipeLineSegment s' ! attr SStroke color ]
      where
        s' :: ClosedLineSegment (Point 2 Double)
        s' = s&vertices %~ toPoint


    toPoint   :: (Point_ point 2 r, Real r) => point -> Point 2 Double
    toPoint p = (p^.asPoint)&coordinates %~ realToFrac

-- | Lables the faces with the first value, according to the 'b'
-- value.
firstColor :: (Foldable f, Foldable e)
           => CPlaneGraph s v (E (e (a :+ Props))) (f (b :+ Props))
           -> CPlaneGraph s v
                            (Maybe (IpeColor Double))
                            (Maybe (IpeColor Double))
firstColor = firstColorFaces . firstColorEdges


firstColorFaces    :: (Foldable f)
                   => CPlaneGraph s v e (f (a :+ Props))
                   -> CPlaneGraph s v e (Maybe (IpeColor Double))
firstColorFaces gr = gr&faces %~ \xs -> (^.extra.to _fillColor)
                                        <$> minimumOn (^.extra.to heightVal) xs


-- | Lables the faces with the first value, according to the 'b'
-- value.
firstColorEdges    :: (Foldable e)
                   => CPlaneGraph s v (e (a :+ Props)) f
                   -> CPlaneGraph s v (Maybe (IpeColor Double)) f
firstColorEdges gr = gr&edges %~ \xs -> (^.extra.to _strokeColor)
                                        <$> minimumOn (^.extra.to heightVal) xs



-- | Compute the minimum using a given function
minimumOn   :: (Ord b, Foldable f) => (a -> b) -> f a -> Maybe a
minimumOn f = minimumByOf folded (comparing f)

--------------------------------------------------------------------------------






--------------------------------------------------------------------------------
{-

myTriangles2 :: NonEmpty (Triangle (Point 2 R))
myTriangles2 = NonEmpty.fromList
              [ Triangle origin (Point2 100 0) (Point2 100 100)
              , Triangle (Point2 10 (-5)) (Point2 20 (-5)) (Point2 20 200)
              ]
mySegments = foldMap1 (toNonEmptyOf outerBoundaryEdgeSegments) myTriangles2

foo = MonoidalNEMap.assocs $
  interiorIntersectionsBySegment mySegments (intersections mySegments)

bar = fromIntersectingSegments mySegments

-}
--------------------------------------------------------------------------------


-- | Given a set of polygons, constructs a plane graph whose vertices,
-- edges, and faces are tagged with the polygons that cover that face.
--
-- The current running time is \(O( (N+k) + \log (N+k) + (N+k)*m )\),
-- where \(N\) is the total complexity of all polygons, \(k\) is the
-- number of intersections between the polygons, and \(m\) is the
-- maximum complexity of a single polygon.
--
-- pre: everything forms a connected graph
polygonOverlay          :: forall nonEmpty s simplePolygon vertex r.
                           ( Foldable1 nonEmpty, Point_ vertex 2 r
                           , SimplePolygon_ simplePolygon vertex r
                           , Fractional r, Ord r
                           , Ord vertex
                           , HasIntersectionWith (Point 2 r) simplePolygon
                           )
                        => nonEmpty simplePolygon
                        -> CPlaneGraph s (V simplePolygon)
                                         (E simplePolygon)
                                         (Seq.Seq simplePolygon)
polygonOverlay polygons = gr2&vertices %~ \(p :+ defs) -> V p defs (polygonsCoveringVertices p)
  where
    gr  = fromIntersectingSegments segs

    gr1 :: CPlaneGraph s (Point 2 r :+ Seq.Seq vertex) (E simplePolygon) ()
    gr1 = gr&edges %@~ polygonsCoveringEdges

    gr2 :: CPlaneGraph s (Point 2 r :+ Seq.Seq vertex)
                         (E simplePolygon)
                         (Seq.Seq simplePolygon)
    gr2 = gr1&faces .@~ polygonsCoveringFaces


    segs :: NonEmpty (ClosedLineSegment vertex)
    segs = foldMap1 (toNonEmptyOf outerBoundaryEdgeSegments) polygons

    outerId = outerFaceId gr

    polygonsCoveringFaces fi
      | fi == outerId = mempty
      | otherwise     = filter' (pointIn fi `intersects`) polygons

    filter' p = foldMap (\x -> if p x then Seq.singleton x else mempty)

    pointIn fi = pointInteriorTo $ gr^?!interiorFacePolygonAt fi

    polygonsCoveringEdges d defs = E defs $ filter' (midPoint d `intersects`) polygons
    midPoint d = let ClosedLineSegment s t = (^.asPoint) <$> gr^?!edgeSegmentAt d
                 in s .+^ ((t .-. s) ^/ 2)

    polygonsCoveringVertices v = filter' (v `intersects`) polygons

-- FIXME: we should remember the polygons defining the edges as well

-- I guess we could build point location structures on the polygons to
-- speed things up, if need be.

--------------------------------------------------------------------------------

-- | A Class for polygon types that support returning a point inside the polygon.
class HasPickPoint polygon r | polygon -> r where
  -- | Returns a point in the interior of the polygon
  pointInteriorTo :: polygon -> Point 2 r

instance ( VertexContainer nonEmpty vertex, HasFromFoldable1 nonEmpty, Point_ vertex 2 r
         , Fractional r
         ) => HasPickPoint (SimplePolygonF nonEmpty vertex) r where
  pointInteriorTo = centroid

instance ( VertexContainer nonEmpty vertex, HasFromFoldable1 nonEmpty, Point_ vertex 2 r
         , Fractional r
         ) => HasPickPoint (PolygonalDomainF h nonEmpty vertex) r where
  pointInteriorTo = pointInteriorTo . view outerBoundaryPolygon
  -- FIXME: this is simply not true

--------------------------------------------------------------------------------

-- newtype PolygonEdge = PolygonEdge
--   (ClosedLineSegment vertex )

-- | Helper data type to implement 'constructCPlanegraph' from a bunch of polygons
data PolygonEdge vertex left =
  PolygonEdge { polygonId   :: {-#UNPACK#-}!Int
              , edgeId      :: {-#UNPACK#-}!Int
              , theSegment  :: ClosedLineSegment vertex
              , leftPolygon :: left
                -- ^ Data on the left side of the edge; this tends to be  the interior
                -- of the polgyon.
              } deriving (Show, Functor)
-- Note that we are *not* deriving EQ, since we use the ID's for EQ and Ord

instance Eq (PolygonEdge vertex left) where
  a == b = polygonId a == polygonId b && edgeId a == edgeId b
instance Ord (PolygonEdge vertex left) where
  a `compare` b = let f x = (polygonId x, edgeId x)
                  in f a `compare` f b




-- in the end this should be a planeGraph rather than a, connected plane graph graph,

-- | Construct a connected Plane Graph out of a bunch of polygons
--
-- every edge is annotated with the polygon(s) immediately to its left.
-- Note that in general this tends to be just one polygon, but if there are
-- shared edges, there could be more than one.
--
-- pre: everything forms a connected graph
constructCPlaneGraph          :: forall nonEmpty s simplePolygon vertex r.
                                   (Foldable1 nonEmpty, Point_ vertex 2 r
                                   , SimplePolygon_ simplePolygon vertex r
                                   )
                              => nonEmpty simplePolygon
                              -> CPlaneGraph s (Point 2 r :+ Seq.Seq vertex)
                                               (ViewL1 simplePolygon)
                                               ()
constructCPlaneGraph polygons = undefined

  -- gr&edges .~ ()
  -- where
  --   gr      = fromIntersectingSegments theSegs

  --   theSegs = ifoldMap1 (\i pg -> foldMap1Of (outerBoundaryEdgeSegments.withIndex)
  --                                            (\(j,seg) -> PolygonEdge i j seg pg
  --                                            ) pg
  --                       ) (toNonEmpty polygons)


-- theIntersections :: forall nonEmpty s simplePolygon vertex r.
--                       (Foldable1 nonEmpty, Point_ vertex 2 r
--                         , SimplePolygon_ simplePolygon vertex r
--                         , Ord r, Fractional r, Ord vertex
--                       )
--                  => nonEmpty simplePolygon
--                  -> Intersections r (ClosedLineSegment vertex)
-- theIntersections = intersections . foldMap1 (toNonEmptyOf outerBoundaryEdgeSegments)



--------------------------------------------------------------------------------

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
interiorIntersectionsBySegment segs inters =
        foldMap1 (flip MonoidalNEMap.singleton mempty) segs
    <>> coerce (ifoldMap construct inters)
  where
    construct p assoc = foldMap (\seg -> MonoidalMap.singleton (coerce seg) (Seq.singleton p))
                                (assoc^.interiorTo)


-- |  Construct A connected PlaneGraph from a set of intersecting segments.
--
-- \( O((n+k)\log n) \), where \(n\) is the number of segments, and \(k\) is the number
-- of intersections.
--
-- pre: the segments actually form a connected graph.
fromIntersectingSegments      :: forall s nonEmpty lineSegment r point.
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
                                 )
                              => nonEmpty lineSegment
                              -> CPlaneGraph s (Point 2 r :+ Seq.Seq point)
                                               (ViewL1 lineSegment)
                                               ()
fromIntersectingSegments segs = fromIntersections segs (intersections segs)

-- | Construct A connected PlaneGraph from a set of intersecting segments. This assumes
-- we are actually given the intersections as well.
--
-- \( O((n+k)\log n) \), where \(n\) is the number of segments, and \(k\) is the number
-- of intersections.
--
-- pre: the segments actually form a connected graph.
fromIntersections             :: forall s nonEmpty lineSegment r point planeGraph.
                                   ( Foldable1 nonEmpty
                                   , LineSegment_ lineSegment point
                                   , Point_ point 2 r, Ord r, Num r
                                   , Intersection lineSegment lineSegment
                                     ~ Maybe (LineSegmentLineSegmentIntersection lineSegment)
                                   , IsIntersectableWith lineSegment lineSegment
                                   , Eq lineSegment
                                   , OrdArounds lineSegment
                                   , Ord lineSegment

                                   , planeGraph ~ CPlaneGraph s (Point 2 r :+ Seq.Seq point)
                                                                (ViewL1 lineSegment)
                                                                ()
                                   )
                              => nonEmpty lineSegment
                              -> Intersections r lineSegment
                              -> planeGraph
fromIntersections segs inters = fromAdjacencyLists adjLists
  where
    -- | Map every Point to its vertexId
    vertexMapping :: MonoidalNEMap (Point 2 r) (VertexIx planeGraph)
    vertexMapping = coerce $ assignIndex vertexLocations

    -- | For each vertex location, collect the segment endpoints
    segmentEndPoints :: MonoidalNEMap (Point 2 r) (ViewL1 point)
    segmentEndPoints = foldMap1 (\(LineSegment_ s t) ->
                                   MonoidalNEMap.singleton (s^.asPoint) (singletonL1 s)
                                <> MonoidalNEMap.singleton (t^.asPoint) (singletonL1 t)
                                ) segs

    vertexLocations :: MonoidalNEMap (Point 2 r) (Associated lineSegment)
    vertexLocations = foldMap1 (\seg -> MonoidalNEMap.singleton (seg^.start.asPoint) (mkAroundStart seg)
                                     <> MonoidalNEMap.singleton (seg^.end.asPoint)   (mkAroundEnd seg)
                               ) segs
                    <>> inters

    -- | Computes the vertices along each segment
    verticesBySegment :: MonoidalNEMap lineSegment (ViewL1 (VertexIx planeGraph))
    verticesBySegment = imap collect $ interiorIntersectionsBySegment segs inters
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
                  (VertexIx planeGraph, Point 2 r :+ Seq.Seq point
                  , NonEmpty ( VertexIx planeGraph
                             , ViewL1 lineSegment
                             )
                  )
    adjLists = imap buildVertex vertexMapping
    buildVertex v vi = (vi, v :+ segEndpts, MonoidalNEMap.assocs $ neighbours MonoidalNEMap.! vi)
      where
        segEndpts = case segmentEndPoints MonoidalNEMap.!? v of
                      Nothing -> mempty
                      Just (x :<< rest) -> x Seq.<| rest


-- | Assign each element in the map a unique Integer key (in the range \([0,n)\) )
assignIndex :: MonoidalNEMap k v -> MonoidalNEMap k Int
assignIndex = snd . MonoidalNEMap.mapAccumWithKey (\i _ _ -> (succ i, i)) 0


-- | Helper to combine a nonempty monoidal map with an additional map
(<>>)        :: (Ord k, Semigroup v) => MonoidalNEMap k v -> Map.Map k v -> MonoidalNEMap k v
base <>> new = foldr (uncurry MonoidalNEMap.insert) base $ Map.toAscList new

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
