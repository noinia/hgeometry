{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Polygon.VisibilitySpec where

import           Control.Lens hiding (views)
import           Data.Maybe
import           Data.Ord (comparing)
-- import qualified Data.Set as Set
import           Golden
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.Foldable.Sort
import           HGeometry.Polygon
import           HGeometry.Properties
import           HGeometry.Polygon.Instances ()
-- import           HGeometry.Polygon.Visibility
import qualified HGeometry.Polygon.Visibility.Naive as Naive
import           HGeometry.Vector
import           HGeometry.HalfLine
import qualified Data.Vector as Vector
import           HGeometry.Intersection
import           Ipe
import           Ipe.Color
import           System.OsPath
import           Test.Hspec
-- import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck hiding (vector)
import           Test.QuickCheck.Instances ()
-- import qualified Data.Text as Text

import           Data.Semigroup

import           Data.Default.Class
import qualified Data.Text as Text
import           Debug.Trace
import           HGeometry.Box
import           HGeometry.Graphics.Camera
import qualified HGeometry.Matrix as Matrix
import           HGeometry.Number.Radical
import           HGeometry.Transformation
import           HGeometry.Triangle

--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "visibility graph / visibility polygon" $ do
         -- prop "naive visibility graph and fast one the same " $
         --   \(pg :: SimplePolygon (Point 2 R)) ->
         --     Set.fromList (visibilityGraph pg) === Set.fromList (Naive.visibilityGraph pg)
         polygonPages <- runIO $ readInputPolygons [osp|data/test-with-ipe/golden/Polygon/visibilityInputPolygons.ipe|]


         goldenWith [osp|data/test-with-ipe/golden/Polygon/|]
           (ipeFileGolden { name = [osp|visibility|] })
             (ipeFile $ perPage <$> polygonPages)

         goldenWith [osp|data/test-with-ipe/golden/Polygon/|]
           (ipeFileGolden { name = [osp|visibilityPolygons|] })
             (ipeFile $ perPage' <$> polygonPages)

         manualPages <- runIO $ readInputPolygons [osp|data/test-with-ipe/golden/Polygon/manual.ipe|]


         goldenWith [osp|data/test-with-ipe/golden/Polygon/|]
           (ipeFileGolden { name = [osp|manualVisibilityPolygons|] })
             (ipeFile $ perPage' <$> manualPages)



readInputPolygons    :: OsPath -> IO (NonEmpty (IpePage R))
readInputPolygons fp = do
  Right file <- readIpeFile fp
  pure $ file^.pages


perPage      :: IpePage R -> IpePage R
perPage page = fromContent . foldMap (drawSingle . (^.core)) $ readAll page

drawSingle    :: SimplePolygon (Point 2 R) -> [IpeObject R]
drawSingle pg = concat
  [ [ iO $ defIO (drawVisibilityEdge e pg) ! attr SStroke red
                                           ! attr SLayer "naive"

    | e <- Naive.visibilityGraph pg
    ]
             -- , [ iO $ defIO (drawVisibilityEdge e pg) ! attr SStroke red
             --                                          ! attr SLayer "visibilityGraph"

             --   | e <- visibilityGraph pg
             --   ]
  , [iO $ defIO pg ! attr SLayer "polygon" ]
  ]





drawVisibilityEdge :: Vector 2 Int -> SimplePolygon (Point 2 R) -> ClosedLineSegment (Point 2 R)
drawVisibilityEdge (Vector2 u v) pg = ClosedLineSegment (pg^?!vertexAt u) (pg^?!vertexAt v)

--------------------------------------------------------------------------------

myPolygon :: SimplePolygon (Point 2 R)
myPolygon = fromJust
          $ fromPoints [ origin
                       , Point2 100 100
                       , Point2 0 100
                       , Point2 (-20) 80
                       , Point2 (-20) 200
                       , Point2 (-30) 200
                       , Point2 (-30) (-20)
                       , Point2 (-25) 0
                       ]

-- out = pg

-- | Generates some input files
createFile :: IO ()
createFile = do
  pgs <- sample' arbitrary
  writeIpeFile [osp|visibilityInputPolygons.ipe|] $
                 ipeFile $ fmap (\pg -> fromContent [ iO $ defIO pg ]) (myPolygon :| pgs)



--------------------------------------------------------------------------------
-- * Visibility Polygons


perPage'      :: IpePage R -> IpePage R
perPage' page = fromContent . concat $
                [ drawSingle' p poly
                | poly :+ _ <- readAll page
                , p :+ _    <- readAll page
                ]

drawSingle'      :: Point 2 R -> SimplePolygon (Point 2 R) -> [IpeObject R]
drawSingle' q pg = [ iO $ defIO visPoly ! attr SStroke blue
                                        ! attr SFill   lightcyan
                                        ! attr SLayer "output"
                   , iO $ defIO q  ! attr SLayer "orig"
                   , iO $ defIO pg ! attr SLayer "orig"
                   ]
  where
    visPoly = (^.asPoint) <$> visibilityPolygon q pg


--------------------------------------------------------------------------------

-- | The second parameter of DefinerF; the edge, actually stores the index of its starting
-- vertex.
type Definer polygon =
  DefinerF (NumType polygon) (VertexIx polygon) (Vertex polygon :+ VertexIx polygon)

data DefinerF r edge orig = OriginalVertex orig
                          | NewVertex (Point 2 r) edge orig
                            -- ^ the intersection point on the edge, defined by the edge
                            -- and the original vertex
                          deriving (Show,Eq,Functor)

instance Bifunctor (DefinerF r) where
  bimap f g = \case
    OriginalVertex p -> OriginalVertex (g p)
    NewVertex p e v  -> NewVertex p (f e) (g v)

position :: Point_ orig 2 r => Lens' (DefinerF r edge orig) (Point 2 r)
position = lens (\case
                    OriginalVertex v -> v^.asPoint
                    NewVertex p _ _  -> p
                ) undefined

type instance Dimension (DefinerF r edge orig) = 2
type instance NumType (DefinerF r edge orig)   = r

instance ( HasSquaredEuclideanDistance orig, Point_ orig 2 r
         ) => HasSquaredEuclideanDistance (DefinerF r edge orig) where
  pointClosestTo _ = view asPoint

instance Point_ orig 2 r => HasVector (DefinerF r edge orig) (DefinerF r edge orig) where
  vector = position.vector
instance Point_ orig 2 r => Affine_ (DefinerF r edge orig) 2 r
instance Point_ orig 2 r => Point_ (DefinerF r edge orig) 2 r
instance Point_ orig 2 r => HasCoordinates (DefinerF r edge orig) (DefinerF r edge orig)

-- | Naive O(n^2) time implementation.
visibilityPolygon :: forall point vertex polygon r.
                     ( Point_ point 2 r, Point_ vertex 2 r
                     , HasSquaredEuclideanDistance vertex
                     , SimplePolygon_ polygon vertex r
                     , Ord r, Fractional r
                     )
                  => point -> polygon -> SimplePolygon (Definer polygon)
visibilityPolygon (view asPoint -> q) poly = fromMaybe err . fromPoints $ theVertices
  where
    theVertices      :: Vector.Vector (DefinerF r (VertexIx polygon) (vertex :+ VertexIx polygon))
    theVertices      = fmap dropIx . sortBy alongBoundary $ originalVertices <> newVertices


    visibleVertices  :: [vertex :+ VertexIx polygon]
    visibleVertices  = poly^..vertices.asIndexedExt.filtered isVisible

    originalVertices
      , newVertices  :: [DefinerF r  (ClosedLineSegment vertex :+ VertexIx polygon)
                                     (vertex :+ VertexIx polygon)
                        ]
    originalVertices = OriginalVertex <$> visibleVertices
    newVertices      = [ w | v <- reflexVertices, w <- maybeToList $ rayThrough v ]

    obstacleEdges :: [ClosedLineSegment vertex :+ VertexIx polygon]
    obstacleEdges = poly^..(reindexed fst outerBoundaryEdgeSegments).asIndexedExt

    -- test if the vertex is strictly visible; i.e. the open line segment between p and q lies
    -- strictly in the interior of the polygon.
    isVisible   :: vertex :+ ix -> Bool
    isVisible p =
      all (not . (intersects (OpenLineSegment (p^.asPoint) q)) . view core) obstacleEdges
    -- TODO: this does not apprporiately account for colinear vertices ; i.e.

    -- we p should be visible from q if the open segment is contained in the closure of P.
    -- i.e. it may coincide with edges

    -- however, for the reflex visibility test below, we want to report the closest
    -- vertices; so we need strictly visible.
    --
    -- moreover, we don't want colinear vertices in the output.


    -- isVisible   :: vertex :+ ix -> Bool
    -- isVisible p =
    --   all (not . (intersects (OpenLineSegment (p^.asPoint) q)) . view core) obstacleEdges


    reflexVertices :: [vertex :+ VertexIx polygon]
    reflexVertices = filter isInterior visibleVertices

    -- test whether the two neighbours of v are on the same side of the line through q and v
    -- if so; then v is a reflex vertex that defines a new visible vertex.
    --
    -- if either the predecessor or successor is colinear with v, then v counts as a reflex
    -- vertex as well (since v was visible)
    isInterior          :: vertex :+ VertexIx polygon -> Bool
    isInterior (v :+ i) = case ( ccw q (poly^.ccwPredecessorOf i.asPoint) (v^.asPoint)
                               , ccw q (poly^.ccwSuccessorOf i.asPoint) (v^.asPoint)
                               ) of
                            (CoLinear, _)       -> True
                            (_       ,CoLinear) -> True
                            (sideP,   sideS)    -> sideP == sideS

    intersectionPoint     :: vertex :+ ix -> ClosedLineSegment vertex :+ ix
                          -> Maybe (DefinerF r (ClosedLineSegment vertex :+ ix) (vertex :+ ix))
    intersectionPoint v e = case HalfLine (v^.asPoint) ((v^.asPoint) .-. q)
                                 `intersect` (asOpenEdge e) of
      Just (HalfLine_x_LineSegment_Point p) -> Just (NewVertex p e v)
      _                                     -> Nothing

    -- treat e as an open line segment; i.e. we don't want duplicate vertices at endpoints
    -- anyway.
    asOpenEdge e = OpenLineSegment (e^.start.asPoint) (e^.end.asPoint)

    rayThrough   :: vertex :+ VertexIx polygon
                 -> Maybe (DefinerF r (ClosedLineSegment vertex :+ VertexIx polygon)
                                     (vertex :+ VertexIx polygon))
    rayThrough v = minimumOn (squaredEuclideanDistTo q)
                 $ mapMaybe (intersectionPoint v) obstacleEdges


    err = error "visibilityPolygon: absurd"

dropIx :: DefinerF r (edge :+ x) vertex  -> DefinerF r x vertex
dropIx = bimap (^.extra) id

alongBoundary :: ( Ord ix, Num r, Ord r, HasStart edge vertex, Point_ vertex 2 r
                 , HasSquaredEuclideanDistance vertex
                 )
              => DefinerF r (edge :+ ix) (vertex :+ ix)
              -> DefinerF r (edge :+ ix) (vertex :+ ix) -> Ordering
alongBoundary = comparing getIx
  where
    getIx = \case
      OriginalVertex (_ :+ i) -> (i,0)
      NewVertex p (e :+ i) _  -> (i,squaredEuclideanDistTo p (e^.start))
  -- if the polygon has holes this is not correct

minimumOn   :: (Ord b, Foldable f) => (a -> b) -> f a -> Maybe a
minimumOn f = fmap (\(Min (Arg _ x)) -> x) . foldMap (\x -> Just $ Min $ Arg (f x) x)


    -- rayThrough (_,v) = case

    -- (pg^..outerBoundaryEdgeSegments)

  -- fromPoints' . snd $ foldl' handle initialStatus events'








-- withInitialRay q events = case findNonColinear q events of
--   (prefix, e1, e2, rest) -> HalfLine q (midPoint (e1.core) (e2^.core) .-. q)
--   where
--     findNonColinear =

--       NonEmpty.zip edges (NonEmpty.tail edges)

-- newtype ByDist e = ByDist e

-- fromPoints' = uncheckedFromPoints . undefined -- remove possibly colinear vertices

-- visibilityPolygon        :: ( Point_ point 2 r, Point_ vertex 2 r
--                             , Polygon_ polygon vertex
--                             , Ord r, Fractional r
--                             )
--                          => point -> polygon -> SimplePolygon (VPVertex r (Edge polygon) orig)
-- visibilityPolygon q poly = fromPoints' . snd $ foldl' handle initialStatus events'
--   where
--     events = sort cmp $ foldMapOf outerBoundaryEdgeSegments mkEvents poly
--     mkEvents seg = (seg^.start :+ seg) :| [seg^.end :+ seg]

--     (initialRay, events') = withInitialRay q events

--     initialStatus = ( foldMapOf outerBoundaryEdgeSegments intersectionPtOf poly
--                     , []
--                     )
--     intersectionPtOf e = case initialRay `intersects` e of
--       Just (HalfLine_x_LineSegment_Point p) -> Set.singleton (ByDist e)
--       _                                     -> mempty

--     cmp = ccwCmpAround q
--     -- are we sorting them in the right order for safe use of unchedkedFromPolygon?

--     handle event (status, res) = case eventRay event `intersect` (event^.extra) of
--         Just (HalfLine_x_LineSegment_Point _) -> (status',res')
--         Just _                                -> (status, res) -- ignore colinear edges
--         Nothing                               -> error "absurd"
--       where
--         status' = toggle (ByDist $ event^.extra) status
--                   -- if the edge is the status structure; delete it, otherwise insert it
--         res'    = undefined


--     eventRay event = HalfLine q (event^.core .-. q)



--------------------------------------------------------------------------------

-- renderPillar    :: Num r => [(Int, SimplePolygon (Point 2 r))] ->
-- renderPillar pg =


-- | Renders a slice
renderSliceWith           :: (Fractional r, Eq r, Functor f, Foldable f)
                          => Camera r -> f (Point 3 r) -> [IpeObject r]
renderSliceWith camera vs = case fromPoints (render <$> vs) of
    Just (pg :: SimplePolygon (Point 2 r)) -> [iO $ defIO pg ! attr SStroke black]
    Nothing                                -> [] -- error "???"
  where
    render = projectPoint . transformBy (cameraTransform camera)
    projectPoint (Point3 x y _) = Point2 x y

-- | Construct a 3d pillar
mkPillar :: (Num r, Real r) => [(r, SimplePolygon (Point 2 r))] -> [[Point 3 R']]
mkPillar = map (\(t,pg) -> extend t <$> pg^..vertices)
  where
    extend z (Point2 x y) = Point3 (realToFrac x) (realToFrac y) (realToFrac z)


-- generatePages seg inputPolygon = NonEmpty.fromList [ myScene camera seg inputPolygon
--                                                    , visibilityPolgins
--                                                    ]
--   where
--     seg       = ClosedLineSegment origin (Point2 0.1 0.2 :: Point 2 R')
-- inputPolygon = fromJust
--                  $ fromPoints
--                    [Point2 (-0.5) (-0.8), Point2 0.5 0.5, Point2 1 0.75, Point2 (-1) 0.75
--                    , Point2 (-1) (-0.8)
--                    ]





visibilityPolygons        :: [(R, SimplePolygon (Point 2 R))]
                          -> ([Ipe.View], [IpeObject R])
visibilityPolygons pillar = (views',obs)
  where
    obs = (\(t,vis) -> iO $ defIO vis ! attr SLayer   (mkLayer t)
                                      ! attr SFill    lightcyan
                                      ! attr SOpacity "10%"
          ) <$> pillar
    views' = (\(t,_) -> View [mkLayer t,"alpha"] "alpha") <$> pillar


mkLayer t = LayerName $ "time" <> Text.pack (show t)

myScene                         :: Camera R' -> ClosedLineSegment (Point 2 R)
                                -> SimplePolygon (Point 2 R) -> [IpeObject R']
myScene camera seg inputPolygon = foldMap (renderSliceWith camera)
                                $ mkPillar (thePillar <> [(0,inputPolygon)
                                                         ,(0,basePlane)])
  where
    thePillar = visibilityPillarWith 10 seg inputPolygon
    basePlane    = fromJust
                 $ fromPoints [Point2 (-1) (-1), Point2 (-1) 1, Point2 1 1, Point2 1 (-1)]



visibilityPillarWith          :: ( ConstructablePoint_ point 2 r, Point_ vertex 2 r
                                 , LineSegment_ lineSegment point
                                 , HasSquaredEuclideanDistance vertex
                                 , SimplePolygon_ polygon vertex r
                                 , Ord r, Fractional r
                                 )
                              => Int
                              -> lineSegment -> polygon -> [(r, SimplePolygon (Point 2 r))]
visibilityPillarWith m s poly = [ let t = fromIntegral i / fromIntegral m
                                  in ( t
                                     , visibilityPolygon (interpolate t s) poly
                                         & vertices %~ (^.asPoint)
                                     )
                                | i <- [0..m]
                                ]



resolution = 12

constructPillar = constructPillar' [osp|data/test-with-ipe/golden/Polygon/pillarIn2.ipe|]

constructPillar'    :: OsPath -> IO ()
constructPillar' fp = do page <- readSinglePageFileThrow @R fp
                         let (polies :: [SimplePolygon (Point 2 R) :+ _]) = readAll page
                             [seg  :: ClosedLineSegment (Point 2 R) :+ _] = readAll page
                             poly = head polies
                         print polies
                         print seg
                         let
                             pillar = [ [iO $ defIO vis ! attr SLayer   "pillar"
                                                        ! attr SFill    lightcyan
                                                        ! attr SLayer (mkLayer t)
                                        , iO $ defIO (interpolate t seg)
                                                     ! attr SLayer (mkLayer t)

                                        ]
                                      | (t, vis) <- visibilityPillarWith resolution seg poly
                                      ]
                             (views',pillar') = visibilityPolygons
                                              $ visibilityPillarWith resolution seg poly
                             out    = concat
                                      [ concat pillar
                                      , [iO $ defIO poly ! attr SLayer "polygon" ]
                                      , [iO $ defIO seg  ! attr SLayer "seg" ]
                                      ]
                             page :: IpePage R
                             page = fromContent out
                                        & views %~ (<> views')
                         writeIpeFile [osp|pillar.ipe|] . addStyleSheet opacitiesStyle $
                                        ipeFile (page :| [])





--------------------------------------------------------------------------------

type R' = Double


-- -- | This is the default camera position used in Blender
-- blenderCamera :: forall r. Floating r => Camera r
-- blenderCamera = def&cameraPosition     .~ Point3 7.35 (-6.92) (4.95) -- Point3 7.35 (-34) (4.95)
--                    &cameraNormal       .~ fromRotate (Vector3 0 0 (-1))
--                    &viewUp             .~ fromRotate (Vector3 0 1 0)
--                    &viewportDimensions .~ fromAspectRatio (Vector2 1920 1080)
--                    &nearDist           .~ 0.1
--                    &focalDepth         .~ 0.05
--   where
--     -- in degrees with respect to?
--     -- x=0 means looking towards z=-\infty, so along Vector3 0 0 (-1)
--     -- z=0 means looking towards y=\infy, so along Vecto3 0 1 0 (-- the default view dir)

--     rotationAngles = Vector3 (-63.559) 0 46.692

--     fromRotate :: Vector 3 r -> Vector 3 r
--     fromRotate = transformBy (rotateXYZ $ toRadians <$> rotationAngles)

--     -- camera width in real world space is 36mm
--     lensWidth                     = 0.036
--     fromAspectRatio (Vector2 w h) = Vector2 lensWidth (lensWidth * (h/w))




-- -- myCamera :: Camera R'
-- -- myCamera = Camera (Point3 (-30) (-20) 20)
-- --                   (Vector3 0 0 (-1))
-- --                   (Vector3 0 1 0)
-- --                   10
-- --                   15
-- --                   55
-- --                   (Vector2 980 800)


toRadians deg = pi * (deg / 180.0)


blenderCube :: [Triangle (Point 3 R') :+ IpeColor R']
blenderCube = transformBy (translation (Vector3 (-1) (-1) (-1)) |.| uniformScaling 2) <$> scene

scene :: [Triangle (Point 3 R') :+ IpeColor R']
scene = [ -- ground plane
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
        ]















renderScene :: IO ()
renderScene = do
    -- page <- readSinglePageFileThrow @R [osp|data/test-with-ipe/golden/Polygon/pillarIn1.ipe|]
    page <- readSinglePageFileThrow @R [osp|data/test-with-ipe/golden/Polygon/pillarIn2.ipe|]
    let [poly' :: SimplePolygon (Point 2 R) :+ _]     = readAll page
        [seg'  :: ClosedLineSegment (Point 2 R) :+ _] = readAll page
        box = Rectangle (Point2 (-0.9) 0) (Point2 0.9 0.75)
        transform = fitToBoxTransform box poly'
        poly = transformBy transform (poly'^.core)
        seg  = transformBy transform (seg'^.core)
    writeIpeFile [osp|scene.ipe|] $ ipeFile ((\cam -> renderPage cam seg poly) <$> cams)
  where
    cams = NonEmpty.fromList [blenderCamera]
    -- cams = NonEmpty.fromList . take 180
    --      $ iterate (\cam -> let transform = transformBy (rotateX (toRadians 1))
    --                         in cam&cameraNormal %~ transform
    --                               &viewUp       %~ transform
    --                )
    --                blenderCamera
    --                -- (blenderCamera&cameraNormal %~ transformBy (rotateZ (toRadians (-90))))


renderPage         :: Camera R' -> _ -> _ -> IpePage R'
renderPage camera seg inputPolygon = fromContent $
                    traceShowWith ("scen",) $
                    scaleUniformlyBy 10000 $
                      map render blenderCube
                      <> myScene camera seg inputPolygon
                      <> renderSliceWith camera (viewportInWorld camera)
  where
    render (triang :+ col) = iO $ defIO triang' ! attr SFill col
      where
        triang' :: Triangle (Point 2 R')
        triang' = triang&vertices %~ projectPoint . transformBy (cameraTransform camera)
        projectPoint (Point3 x y _) = Point2 x y
