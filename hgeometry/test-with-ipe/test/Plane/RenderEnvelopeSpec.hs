{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Plane.RenderEnvelopeSpec
  where

import Data.Proxy
import Data.Maybe
import Data.Foldable
import Wavefront qualified
import HGeometry.ByIndex
import Control.Lens
import Control.Monad
import Data.Coerce
import Data.Default.Class
import Data.Foldable1
import Data.Foldable1.WithIndex
import Data.Functor.Apply as Apply
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Map.NonEmpty qualified as NEMap
import Data.Ord (comparing)
import Data.Semialign
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Golden
import HGeometry.Box as Box
import HGeometry.Ext
import HGeometry.Foldable.Util
import HGeometry.Graphics.Camera
import HGeometry.HyperPlane.NonVertical
import HGeometry.Instances ()
import HGeometry.Intersection
import HGeometry.LineSegment
import HGeometry.LineSegment.Intersection.BentleyOttmann
import HGeometry.Map.NonEmpty.Monoidal (MonoidalNEMap)
import HGeometry.Map.NonEmpty.Monoidal qualified as MonoidalNEMap
import HGeometry.Number.Real.Rational
import HGeometry.Plane.LowerEnvelope
import HGeometry.Plane.LowerEnvelope.Connected.BruteForce qualified as BruteForce
import HGeometry.PlaneGraph
import HGeometry.Point
import HGeometry.Polygon
import HGeometry.Polygon.Convex.Instances ()
import HGeometry.Polygon.Simple
import HGeometry.Polygon.WithHoles
import HGeometry.Properties
import HGeometry.Sequence.NonEmpty (ViewL1(..), asViewL1, singletonL1)
import HGeometry.Transformation
import HGeometry.Triangle
import HGeometry.Vector
import HGeometry.VoronoiDiagram
import HGeometry.VoronoiDiagram qualified as VD
import HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane)
import Hiraffe.PlanarGraph.Connected
import Ipe
import Ipe.Color
import Plane.Overlay
import Plane.RenderProps
import PlaneGraph.RenderSpec
import Prelude hiding (zipWith)
import System.OsPath
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.WithTempFile

--------------------------------------------------------------------------------

type R = RealNumber 5

triangles :: NonEmpty (Triangle (Point 3 Double) :+ RenderProps)
triangles = -- scaleUniformlyBy 5 <$>
            NonEmpty.fromList $
        [ -- ground plane
          Triangle origin (Point3 1 0 0) (Point3 1 1 0) :+ props blue
        , Triangle origin (Point3 1 1 0) (Point3 0 1 0) :+ props blue
        -- left side
        , Triangle origin (Point3 0 1 0) (Point3 0 1 1) :+ props green
        , Triangle origin (Point3 0 1 1) (Point3 0 0 1) :+ props green
        -- front plane
        -- , Triangle origin (Point3 1 0 0) (Point3 1 0 1) :+ red
        -- , Triangle origin (Point3 1 0 1) (Point3 0 1 1) :+ red


        -- back plane
        , Triangle (Point3 0 1 0) (Point3 1 1 0) (Point3 1 1 1) :+ props orange
        , Triangle (Point3 0 1 0) (Point3 1 1 1) (Point3 0 1 1) :+ props orange
        ] <> ((\tri -> tri&extra %~ getColor
                         &vertices.coordinates %~ realToFrac
              ) <$> myTriangles
             )

props c = RenderProps Nothing (Just $ attr SFill c)

getColor :: core :+ RenderProps -> RenderProps
getColor = view extra

myTriangles :: [Triangle (Point 3 R) :+ (Plane R :+ RenderProps)]
myTriangles = asTrianglesAbove domain planes

planes :: NonEmpty (Plane R :+ RenderProps)
planes = NonEmpty.fromList
           [ Plane 0 0 (0.5)   :+ props red
           , Plane 0 (-0.25) 1 :+ props gray
           ]

-- planes = points&mapped.core %~ pointToPlane

points :: NonEmpty (Point 2 R :+ IpeColor R)
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







-- instance Default Props where
--   def = Props 0 black black

instance Default (Seq.Seq a) where
  -- this is just for drawing to ipe purposes
  def = mempty


fromTriangle :: (Point_ vertex 2 r, Ord r, Num r) => Triangle vertex -> SimplePolygon vertex
fromTriangle = fromMaybe (error "fromTriangle: absurd") . fromPoints

{-
fromTriangle :: Point_ vertex 2 r => Triangle vertex -> SimplePolygon vertex
fromTriangle = uncheckedFromCCWPoints
-- TODO: we should just coerce to a SimplePolygonF (Cyclic Vector3)
-}
--------------------------------------------------------------------------------
-- TODO: Move to a PlaneGraph.RenderOverlaySpec module

overlaySpec :: Spec
overlaySpec = describe "OverlaySpec" $ do
      goldenWith [osp|data/test-with-ipe/golden/PlaneGraph/|]
            (ipeFileGolden { name = [osp|planeGraphFromIntersectingSegments|]
                           }
            )
            ( let myPlaneGraph = polygonOverlay myPolygons
                  content'     = renderGraph (assignRenderingAttributes myPlaneGraph)
              in addStyleSheet opacitiesStyle $ singlePageFromContent content'
            )
  where
    myTriangles2 :: NonEmpty (Triangle (Point 2 R) :+ (Int, RenderProps))
    myTriangles2 = NonEmpty.fromList
      [ Triangle origin (Point2 100 0) (Point2 100 100)            :+ props 1 black  red
      , Triangle (Point2 10 (-5)) (Point2 20 (-5)) (Point2 20 200) :+ props 2 green blue
      ] -- these triangles are indeed in CCW order....
    myPolygons = myTriangles2&mapped.core %~ fromTriangle
    props z s f = (z, RenderProps (Just $ attr SStroke s) (Just $ attr SFill f))

--------------------------------------------------------------------------------


spec :: Spec
spec =
    describe "Plane.RenderEnvelope"  $ do
      overlaySpec

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

-- | Function to draw a plane graph whose faces may have some
-- attributes associated them.
renderGraph    :: forall planeGraph vertex r r'.
                  ( PlaneGraph_ planeGraph vertex, HasOuterBoundaryOf planeGraph
                  , Point_ vertex 2 r
                  , Ord r, Fractional r
                  , Real r'
                  , Face planeGraph ~ Maybe (IpeAttributes Path r')
                  , Edge planeGraph ~ Maybe (IpeAttributes Path r')
                  , Eq (FaceIx planeGraph)
                  , HasInnerComponents planeGraph
                  -- , IsTransformable vertex
                  -- , ConstructablePoint_ vertex 2 r, Ord r, Real r
                  -- , Fractional r, Show r,
                  -- , Show (Vertex planeGraph), Show (Face planeGraph)
                  -- , Show (EdgeIx planeGraph), Show (Edge planeGraph)
                  ) => planeGraph -> [IpeObject r]
renderGraph gr = theFaces <> theEdges
  where
    theEdges = ifoldMapOf edgeSegments         drawEdge' gr
    theFaces = ifoldMapOf interiorFacePolygons drawFace' gr
    drawFace' fi pg = case attrToR <$> gr^?!faceAt fi of
        Nothing  -> []
        Just ats -> [ iO $ ipePolygon pg' ! ats ]
      where
          pg' :: PolygonalDomain _
          pg' = pg&vertices %~ toPoint

    drawEdge' d s = case attrToR <$> gr^?!edgeAt d of
        Nothing  -> []
        Just ats -> [ iO $ ipeLineSegment s' ! ats ]
      where
        s' :: ClosedLineSegment (Point 2 r)
        s' = s&vertices %~ toPoint

    toPoint :: Point_ point 2 r => point -> Point 2 r
    toPoint = view asPoint

    attrToR = mapIpeAttrs (Proxy @Path) realToFrac

renderSkeleton    :: forall s v polygon.
                     CPlaneGraph s v
                                   (E (polygon :+ RenderProps))
                                   (Seq.Seq (polygon :+ RenderProps))
                  -> CPlaneGraph s v
                                   (Maybe (IpeAttributes Path Double))
                                   (Maybe (IpeAttributes Path Double))
renderSkeleton gr = gr1&faces .~ Nothing
  where
    gr1 :: CPlaneGraph s v
                         (Maybe (IpeAttributes Path Double))
                         (Seq.Seq (polygon :+ RenderProps))
    gr1 = gr&edges .~ Just def
      -- \(E defs covering) -> do
      --                     _ :+ (_ :+ (z,props)) <- minimumOn (^.extra.extra._1) defs
      --                     _ :+ (zCovering,_)    <- minimumOn (^.extra._1) covering
      --                     if z <= zCovering then props^.edgeAttrs
      --                                       else Nothing



-- | Lables the faces and edges with the attributes to use in rendering
assignRenderingAttributes    :: forall s v polygon z.
                                Ord z
                             => CPlaneGraph s v
                                              (E (polygon :+ (z, RenderProps)))
                                              (Seq.Seq (polygon :+ (z, RenderProps)))
                             -> CPlaneGraph s v
                                              (Maybe (IpeAttributes Path Double))
                                              (Maybe (IpeAttributes Path Double))
assignRenderingAttributes gr = gr1&faces %~ \xs -> do x <- minimumOn (^.extra._1) xs
                                                      x^.extra._2.faceAttrs
  where
    gr1 :: CPlaneGraph s v
                         (Maybe (IpeAttributes Path Double))
                         (Seq.Seq (polygon :+ (z, RenderProps)))
    gr1 = gr&edges %~ \(E defs covering) -> do
                          _ :+ (_ :+ (z,props)) <- minimumOn (^.extra.extra._1) defs
                          _ :+ (zCovering,_)    <- minimumOn (^.extra._1) covering
                          if z <= zCovering then props^.edgeAttrs
                                            else Nothing

  -- We compute the first defining polygon and its z-value, and the
  -- props corresponding to this edge. Similarly, we compute the
  -- minimum zvalue of the covering regions. If the covering z-value
  -- is smaller than the one among the definers, then we show Nothing;
  -- as the edge is hidden. Otherwise we show the definer's value

-- TODO: maybe we still want to store the z-value, and still draw them in ipe in
-- back to front order.


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
    segs :: NonEmpty (ClosedLineSegment vertex :+ simplePolygon)
    segs = foldMap1 (\poly -> (:+ poly) <$> toNonEmptyOf outerBoundaryEdgeSegments poly) polygons

    gr  = fromIntersectingSegments segs

    gr1 :: CPlaneGraph s (Point 2 r :+ Seq.Seq vertex) (E simplePolygon) ()
    gr1 = gr&edges %@~ polygonsCoveringEdges

    gr2 :: CPlaneGraph s (Point 2 r :+ Seq.Seq vertex)
                         (E simplePolygon)
                         (Seq.Seq simplePolygon)
    gr2 = gr1&faces .@~ polygonsCoveringFaces


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
-- *  The stuff in this section is unsused
{-

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

-}

--------------------------------------------------------------------------------
-- * Computing a Connected Plane graph from a set of intersecting line segments

-- |  Construct A connected PlaneGraph from a set of intersecting segments.
--
-- \( O((n+k)\log n) \), where \(n\) is the number of segments, and \(k\) is the number
-- of intersections.
--
-- pre: the segments actually form a connected graph.
fromIntersectingSegments      :: forall s nonEmpty ix lineSegment segment r point.
                                 ( Foldable1 nonEmpty
                                 , FunctorWithIndex ix nonEmpty

                                 , LineSegment_ lineSegment point
                                 , Point_ point 2 r, Ord r, Fractional r
                                 , Intersection lineSegment lineSegment
                                   ~ Maybe (LineSegmentLineSegmentIntersection segment)
                                 , LineSegment_ segment point
                                 , IsIntersectableWith lineSegment lineSegment
                                 , Ord ix
                                 , HasOnSegment lineSegment 2
                                 , StartPointOf lineSegment ~ EndPointOf lineSegment
                                 )
                              => nonEmpty lineSegment
                              -> CPlaneGraph s (Point 2 r :+ Seq.Seq point)
                                               (ViewL1 lineSegment)
                                               ()
fromIntersectingSegments segs = gr&edges.mapped %~ view theValue
  where
    segs' = imap ByIndex segs
    gr    :: CPlaneGraph s _ _ _
    gr    = fromIntersections segs' (intersections segs')

----------------------------------------

-- | Construct A connected PlaneGraph from a set of intersecting
-- segments. This assumes we are actually given the intersections as
-- well.
--
-- \( O((n+k)\log n) \), where \(n\) is the number of segments, and \(k\) is the number
-- of intersections.
--
-- pre: the segments actually form a connected graph.
--
-- note: this implementation uses that the lineSegment type is Orderable. Consider using
-- 'ByIndex Int segment' to order by some Identifier, rather than ordering by the raw segments.
fromIntersections             :: forall s nonEmpty lineSegment r point planeGraph.
                                   ( Foldable1 nonEmpty
                                   , LineSegment_ lineSegment point
                                   , Point_ point 2 r, Ord r, Num r
                                   , IsIntersectableWith lineSegment lineSegment
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

    -- I think this already automatically takes care of colinear semgents as well, as we
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


-- | Computes the interior intersections on each segment.
--
-- O((n+k)\log n)
interiorIntersectionsBySegment             :: ( Ord lineSegment, Foldable1 nonEmpty)
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


-- | Assign each element in the map a unique Integer key (in the range \([0,n)\) )
assignIndex :: MonoidalNEMap k v -> MonoidalNEMap k Int
assignIndex = snd . MonoidalNEMap.mapAccumWithKey (\i _ _ -> (succ i, i)) 0


-- | Helper to combine a nonempty monoidal map with an additional map
(<>>)        :: (Ord k, Semigroup v) => MonoidalNEMap k v -> Map.Map k v -> MonoidalNEMap k v
base <>> new = foldr (uncurry MonoidalNEMap.insert) base $ Map.toAscList new

--------------------------------------------------------------------------------

-- | Given a camera and a set of colored triangles in R^3, renders the
-- triangles as they are visible from the camera
renderToIpe       :: forall set point r.
                     ( Foldable1 set, Functor set
                     , Point_ point 3 r, Real r, Fractional r
                     )
                   => Camera Double -> set (Triangle point :+ RenderProps) -> [IpeObject R]
renderToIpe camera scene =
    [ -- iO $ ipeGroup (renderGraph (assignRenderingAttributes subdiv)) ! attr SLayer "render"
     iO $ ipeGroup (renderGraph (renderSkeleton subdiv))            ! attr SLayer "skeleton"
    ]
  where
    triangles = render camera scene
    subdiv    = polygonOverlay $ triangles&mapped.core %~ fromTriangle
    -- sudbdiv    = traceShow (numEdges subdiv') subdiv'

-- | Render a scene; i..e a set of triangles
--
-- this intermediately uses doubles to apply the camera transform
render        :: forall point r set extra. (Functor set, Point_ point 3 r, Real r, Fractional r)
              => Camera Double
              -> set (Triangle point :+ extra)
              -> set (Triangle (Point 2 R) :+ extra)
render camera = over (mapped.core) $ \triangle ->
    triangle&vertices %~
            f2 . projectPoint . transformBy (cameraTransform camera') . f3
  where
    camera' = realToFrac <$> camera

    f2 :: Point 2 Double -> Point 2 R
    f2 = over coordinates realToFrac

    f3   :: point -> Point 3 Double
    f3 p = over coordinates realToFrac (p^.asPoint)


-- next thing to do is somehow make it so that 'render' actually computes
-- the overlay, and renders it. This probably requires some rewiring making sure
-- we can compute the minimum zValue on the fly


loadObj = Wavefront.fromFile [osp|data/test-with-ipe/golden/Render/cube/cube.obj|] >>= \case
            Left err  -> print err
            Right obj -> traverse_ print $ Wavefront.allTriangles obj
