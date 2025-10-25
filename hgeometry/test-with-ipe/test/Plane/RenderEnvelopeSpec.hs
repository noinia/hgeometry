{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Plane.RenderEnvelopeSpec
  where

import Data.Colour.SRGB (RGB(..))
import HGeometry.HyperPlane
import HGeometry.Unbounded
import Data.Proxy
import Data.Maybe
import Data.Foldable
import Wavefront qualified
import Wavefront (elValue, elMtl)
import Codec.Wavefront.Element qualified as Element
import Codec.Wavefront.Material.Type qualified as Material
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
import HGeometry.Box (Box(..), Rectangle, Corners(..))
import HGeometry.Box qualified as Box
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
import HGeometry.Number.Radical
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
import HGeometry.Interval.Class
import HGeometry.Vector
import HGeometry.VoronoiDiagram
import HGeometry.VoronoiDiagram qualified as VD
import HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane)
import Hiraffe.PlanarGraph.Connected
import Ipe
import Ipe.Color
import Plane.RenderProps
import PlaneGraph.RenderSpec
import Prelude hiding (zipWith)
import System.OsPath
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.WithTempFile
import PlaneGraph.PolygonOverlaySpec (assignRenderingAttributes, renderGraph)
import Debug.Trace
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
        ]
        <> ((\tri -> tri&extra %~ getColor
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
           -- , Plane 0 (-0.25) 1 :+ props gray
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

-- instance HasRenderProps (triangle :+ RenderProps) where
--   renderProps = extra

-- ‘HasRenderProps
--                           (SimplePolygon (Point 2 R) :+ triangle)
instance HasRenderProps extra => HasRenderProps (polygon, extra) where
  renderProps = _2.renderProps

instance HasRenderProps extra => HasRenderProps (polygon :+ extra) where
  renderProps = extra.renderProps


spec :: Spec
spec =
    describe "Plane.RenderEnvelope"  $ do

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
      testRenderObj [osp|data/test-with-ipe/golden/Render/cube/|]
                    [osp|cube.obj|]
      testRenderObj [osp|data/test-with-ipe/golden/Render/cornellbox/|]
                    [osp|CornellBox-Mirror.obj|]


testRenderObj                :: OsPath -> OsPath -> Spec
testRenderObj objDir objFile = do
    triangles <- runIO $ Wavefront.fromFile (objDir </> objFile) >>= \case
                           Left err  -> error err
                           Right obj -> case NonEmpty.nonEmpty (toList $ Wavefront.allTriangles obj) of
                             Nothing  -> error "no triangles?"
                             Just res -> pure res

    goldenWith [osp|data/test-with-ipe/golden/Plane/|]
        (ipeFileGolden { name = objFile
                       }
        )
        ( let triangles'= triangles <&> \el -> el^.elValue :+ fromMaterial (el^.elMtl)
              cornellCamera = blenderCamera
              -- cornellCamera = blenderCameraAt (Point3 6.82743 (-15.8002) 5.22068)
              --                                 (Vector3 73.1268 0.000008 23.3696)

              cameras    = NonEmpty.fromList [cornellCamera]
              -- cameras = NonEmpty.fromList . take 10
              --         $ iterate (\cam -> -- let transform = transformBy (rotateX (toRadians 1))
              --                            let transform = transformBy
              --                                            (translation $ Vector3 1 0 0)
              --                            in cam&cameraNormal %~ transform
              --                                  &viewUp       %~ transform
                      --           )
                      -- cornellCamera
              pages = renderPage triangles' <$> cameras
          in addStyleSheet opacitiesStyle $ ipeFile pages
        )

  where
    renderPage triangles camera = fromContent content'
      where
        content'  = let tris = renderToIpe camera triangles
                        t    = uniformScaling 1000
                               --- fitToBoxTransform screenBox  tris -- TODO
                    in transformBy t tris

--------------------------------------------------------------------------------


renderSkeleton    :: forall s v polygon.
                     CPlaneGraph s v
                                   (E polygon)
                                   (F polygon)
                  -> CPlaneGraph s v
                                   (Maybe (IpeAttributes Path Double))
                                   (Maybe (IpeAttributes Path Double))
renderSkeleton gr = gr1&faces .~ Nothing
  where
    gr1 :: CPlaneGraph s v
                         (Maybe (IpeAttributes Path Double))
                         (F polygon)
    gr1 = gr&edges ?~ def
      -- \(E defs covering) -> do
      --                     _ :+ (_ :+ (z,props)) <- minimumOn (^.extra.extra._1) defs
      --                     _ :+ (zCovering,_)    <- minimumOn (^.extra._1) covering
      --                     if z <= zCovering then props^.edgeAttrs
      --                                       else Nothing






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

{-
newtype SegmentWith vertex extra = SegmentWith (ClosedLineSegment vertex :+ extra)
  deriving newtype (Show)

_unSegmentWith :: Iso' (SegmentWith vertex extra) (ClosedLineSegment vertex :+ extra)
_unSegmentWith = coerced

type instance NumType      (SegmentWith vertex extra) = NumType vertex
type instance Dimension    (SegmentWith vertex extra) = Dimension vertex
type instance StartPointOf (SegmentWith vertex extra) = StartPointOf (ClosedLineSegment vertex)
type instance EndPointOf   (SegmentWith vertex extra) = EndPointOf   (ClosedLineSegment vertex)

instance HasStart (SegmentWith vertex extra) vertex where
  start = _unSegmentWith.start
instance HasEnd (SegmentWith vertex extra) vertex where
  end = _unSegmentWith.end
instance HasStartPoint (SegmentWith vertex extra) (EndPoint Closed vertex) where
  startPoint = _unSegmentWith.startPoint
instance HasEndPoint (SegmentWith vertex extra) (EndPoint Closed vertex) where
  endPoint = _unSegmentWith.endPoint

instance Point_ vertex d r => IntervalLike_      (SegmentWith vertex extra) vertex
instance Point_ vertex d r => LineSegment_       (SegmentWith vertex extra) vertex
--instance Point_ vertex d r => ClosedLineSegment_ (SegmentWith vertex extra) vertex
instance (Point_ vertex 2 r, Num r) => HasOnSegment       (SegmentWith vertex extra) 2 where
  onSegment q seg = onSegment q (seg^._unSegmentWith)

instance HasIntersectionWith (ClosedLineSegment vertex :+ simplePolygon)
                             (ClosedLineSegment vertex :+ simplePolygon)
         => HasIntersectionWith (SegmentWith vertex simplePolygon)
                             (SegmentWith vertex simplePolygon) where
  segA `intersects` segB = (segA^._unSegmentWith) `intersects` (segA^._unSegmentWith)

instance IsIntersectableWith (ClosedLineSegment vertex :+ simplePolygon)
                             (ClosedLineSegment vertex :+ simplePolygon)
         => IsIntersectableWith (SegmentWith vertex simplePolygon)
                                (SegmentWith vertex simplePolygon) where
  segA `intersect` segB = (segA^._unSegmentWith) `intersect` (segA^._unSegmentWith)

type instance Intersection (SegmentWith vertex extra) (SegmentWith vertex extra) =
  Intersection (ClosedLineSegment vertex :+ extra) (ClosedLineSegment vertex :+ extra)

instance Eq vertex => Eq (SegmentWith vertex extra) where
  (SegmentWith (segA :+ _)) == (SegmentWith (segB :+ _)) = segA == segB

instance Ord vertex => Ord (SegmentWith vertex extra) where
  (SegmentWith (segA :+ _)) `compare` (SegmentWith (segB :+ _)) = segA `compare` segB
-}





--------------------------------------------------------------------------------



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
-- *


--------------------------------------------------------------------------------


instance Default (Triangle (Point 3 Float)) where
  def = undefined
instance Default (Triangle (Point 3 Double)) where
  def = undefined
instance Default ProjectedTriangle where
  def = undefined -- this is nonense
instance Default Material.Material where
  def = Material.defaultMaterial "default"

instance (Default material, Default a) => Default (Element.ElementF material a) where
  def = Element.Element { Element.elObject         = Nothing
                        , Element.elGroups         = []
                        , Element.elMtl            = def
                        , Element.elSmoothingGroup = 0
                        , Element.elValue          = def
                        }

fromMaterial    :: Maybe Material.Material -> RenderProps
fromMaterial mm = case mRefl of
    Nothing   -> def
    Just refl -> case refl of
      Material.ReflexicityRGB rgb -> let c = IpeColor (Valued $ convert rgb)
                                     in RenderProps def (Just $ attr SFill c)
  where
    mRefl = do m <- mm
               -- guard (traceShowId (Material.materialName m) `elem` [ "leftWall"
               --                                                     , "rightWall"
               --                                       , "floor"
               --                                       , "ceiling"
               --                                       , "tallBox"
               --                                       , "shortBox"
               --                                       ])
               Material.ambientReflexivity m



convert                      :: Material.RGB -> RGB Double
convert (Material.RGB r g b) = realToFrac <$> RGB r g b

-- | Given a camera and a set of colored triangles in R^3, renders the
-- triangles as they are visible from the camera
renderToIpe              :: forall set triangle point r.
                            ( Foldable1 set, Functor set
                            , Triangle_ triangle point
                            , Point_ point 3 r, Real r, Fractional r
                            , HasRenderProps triangle

                            , Show triangle, Eq triangle

                            -- , r ~ R
                            , Default triangle -- TODO: remove this
                            )
                          => Camera Double
                         -> set triangle
                         -> [IpeObject R]
renderToIpe camera scene =
    -- replicate k (iO $ defIO (Point2 5 5))
    -- <>
    [ iO $ ipeGroup (renderGraph (renderSkeleton subdiv))                 ! attr SLayer "skeleton"
    , iO $ ipeGroup (renderGraph (assignRenderingAttributes getZ subdiv)) ! attr SLayer "render"
    ]
    -- drawGraphWithDarts subdiv
  where
    triangles = render camera scene
    subdiv    = polygonOverlay $ triangles&mapped %~ \(pt :+ orig) ->
                                   let poly = fromTriangle $ toTriangle2 pt
                                   in poly :+ (pt, orig)

    -- | Compute the z-coordinate at the given location. We render the closest triangle
    getZ         :: Point 2 R -> triangle2d :+ (ProjectedTriangle, triangle) -> R
    getZ q poly = case supportingPlane (poly^.extra._1.triangle3) of
      Nothing -> error "getZ: unhandled degeneracy; we hit the side of the triangle"
      Just h  -> evalAt q h

-- | Get the supporting plane of the triangle (as a non-vertical plane)
supportingPlane   :: forall triangle point r.
                     (Triangle_ triangle point, Point_ point 3 r, Fractional r, Eq r)
                  => triangle -> Maybe (Plane r)
supportingPlane t = asNonVerticalHyperPlane @(HyperPlane 3 r) $ hyperPlaneThrough (t^.corners)




-- | Represent the projection of a 3D triangle in 2D space.  i.e. this
-- triangle acts as a triangle in R^2, but also has the information
-- from where it came from.
newtype ProjectedTriangle = ProjectedTriangle {_triangle3 :: Triangle (Point 3 R) }
  deriving stock (Show,Eq)

-- | Access the 3D triagnle
triangle3 :: Iso' ProjectedTriangle (Triangle (Point 3 R))
triangle3 = coerced

type instance NumType   ProjectedTriangle = R
type instance Dimension ProjectedTriangle = 2

-- | Renders a Projected Triangle as a 2D Triangle
toTriangle2 :: ProjectedTriangle -> Triangle (Point 2 R)
toTriangle2 = fmap projectPoint . coerce



-- | Render a scene; i..e a set of triangles
--
-- this intermediately uses doubles to apply the camera transform
render        :: forall triangle point r set.
                 ( Functor set
                 , Triangle_ triangle point, Point_ point 3 r
                 , Real r, Fractional r
                 )
              => Camera Double
              -> set triangle
              -> set (ProjectedTriangle :+ triangle)
render camera = fmap $ \orig@(Triangle_ a b c) ->
                         ProjectedTriangle (Triangle (f a) (f b) (f c)) :+ orig
  where
    f = f2 . transformBy (cameraTransform camera) . f3

    f2 :: Point 3 Double -> Point 3 R
    f2 = over coordinates realToFrac

    f3   :: point -> Point 3 Double
    f3 p = over coordinates realToFrac (p^.asPoint)
    -- TODO: clean up


-- next thing to do is somehow make it so that 'render' actually computes
-- the overlay, and renders it. This probably requires some rewiring making sure
-- we can compute the minimum zValue on the fly


loadObj = Wavefront.fromFile [osp|data/test-with-ipe/golden/Render/cube/cube.obj|] >>= \case
            Left err  -> print err
            Right obj -> traverse_ print $ Wavefront.allTriangles obj
