{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module PlaneGraph.FromPolygonsSpec where

import           Control.Lens
import           Data.Coerce
import           Data.Foldable1
import           Data.Functor.Apply (WrappedApplicative(..))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NonEmptyV
import           GHC.Generics (Generic)
import           Golden
import           HGeometry.Boundary
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.LineSegment
import           HGeometry.Map.NonEmpty.Monoidal (MonoidalNEMap)
import qualified HGeometry.Map.NonEmpty.Monoidal as MonoidalNEMap
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Properties
import           HGeometry.Vector.NonEmpty.Util ()
import           Hiraffe.PlanarGraph
import           Hiraffe.PlanarGraph.Component
import qualified Hiraffe.PlanarGraph.Dart as Dart
import           Ipe
import           PlaneGraph.RenderSpec
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.WithTempFile
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder

-- import HGeometry.Plane.LowerEnvelope.Connected.MonoidalMap

import Debug.Trace
--------------------------------------------------------------------------------

type R = RealNumber 5


-- deriving stock Show (PlaneGraph s v e f)

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

-- data VTXData point = VTXData point edge


-- | Given a set of line segments that may intersect only in endpoints, construct
-- the planeGraph these segments represent.
fromDisjointSegments      :: ( Foldable1 nonEmpty, Ord r, Num r
                             , LineSegment_ lineSegment point
                             , Point_ point 2 r
                             )
                          => nonEmpty lineSegment
                          -> PlaneGraph s (NonEmpty.NonEmpty point) lineSegment ()
fromDisjointSegments segs = undefined

--   fromAdjacencyLists adjs
--   where
--     adjs = undefined

--     theDarts' = toNonEmpty . ifoldMap1 f $ toNonEmpty segs

--     -- Creates two vertices with one edge each ; combines them into a single Map
--     f       :: Int -> lineSegment
--             -> MonoidalNEMap (Point 2 r)
--                              (VtxData (NonEmpty point) r (Dart.Dart s, lineSegment))
--     f i seg = let u = seg^.start
--                   v = seg^.end
--                   d = Dart.Dart (Dart.Arc i) Dart.Positive
--               in    singleton (u^.asPoint) (vtx (d          ,seg) u v)
--                  <> singleton (v^.asPoint) (vtx (Dart.twin d,seg) v u)

--     singleton k v = MonoidalNEMap $ NEMap.singleton k v


-- -- | Helper type to represent the vertex data of a vertex. The NEMap
-- -- represents the edges ordered cyclically around the vertex
-- type VtxData v r e = (v, NEMap.NEMap (E r) e)


-- | Construct planar graphs, one for each component, given the darts in cyclic order
-- around each vertex.
planarGraphs :: Foldable1 nonEmpty
             => nonEmpty (v, NonEmpty (DartId s, e))
             -> NonEmpty (PlanarGraph Primal s v e ())
planarGraphs = undefined








-- | Construct a PlaneGraph from a single polygon (which may have holes).
fromPolygon      :: ()
                 => polygon
                 -> PlaneGraph s (vertex :+ VertexIx polygon)
                                 (EdgeIx polygon)
                                 PointLocationResult
fromPolygon poly = undefined
-- TODO: tag the holes ?


fromPolygons :: ( Polygon_ polygon vertex r
                , Foldable1 nonEmpty
                ) => nonEmpty polygon -> PlaneGraph s vertex () [polygon]
fromPolygons = undefined


--------------------------------------------------------------------------------

-- | Given a bunch of components, each one with a given outer face,
-- construct a PlaneGraph from them that unifies all their outer faces; i.e.
-- embeds all components in a single outer face.
fromDisjointComponents :: forall set s v e f r.
                          (Foldable1 set, Functor set, Point_ v 2 r, Num r, Ord r
                          )
                       => (set (FaceId (Wrap s), f) -> f)
                          -- ^ How to combine the data from the "outer faces"
                       -> set (CPlaneGraph (Wrap s) v e f)
                       -> PlaneGraph s v e f
fromDisjointComponents combineOuterFace graphs = review _PlanarGraph $
  fromDisjointComponents1 combineOuterFace
                          (fmap (\gr -> ( gr^._CPlanarGraph
                                        , gr^.outerFace.asIndex)
                                ) graphs)

-- | Given a bunch of components, each one with a given outer face,
-- construct a PlaneGraph from them that unifies all their outer faces; i.e.
-- embeds all components in a single outer face.
fromDisjointComponents' :: forall set s v e f. (Foldable1 set, Functor set)
                       => (set (FaceId (Wrap s), f) -> f)
                          -- ^ How to combine the data from the "outer faces"
                       -> set ( CPlaneGraph (Wrap s) v e f
                              , FaceId (Wrap s)
                              )
                       -> PlaneGraph s v e f
fromDisjointComponents' combineOuterFace graphs = review _PlanarGraph $
  fromDisjointComponents1 combineOuterFace ((\(gr,fi) -> (gr^._CPlanarGraph,fi))
                                            <$> graphs
                                           )


-- | Given a bunch of components, each one with a given outer face,
-- construct a PlanarGraph from them that unifies all their outer faces; i.e.
-- embeds all components in a single outer face.
fromDisjointComponents1 :: forall set s v e f. (Foldable1 set, Functor set)
                       => (set (FaceId (Wrap s), f) -> f)
                          -- ^ How to combine the data from the "outer faces"
                       -> set ( CPlanarGraph Primal (Wrap s) v e f
                              , FaceId (Wrap s)
                              )
                       -> PlanarGraph Primal s v e f
fromDisjointComponents1 combineOuterFace graphs =
    PlanarGraph (fst <$> components)
                (build   vData)
                (build   dData)
                (build $ Builder.singleton outerFaceData <> fData)
  where
    -- Collect the outerfaces and their data.
    outerFaces :: set (FaceId (Wrap s), f)
    outerFaces = (\(gr,fi) -> (fi, gr^?!faceAt fi)) <$> graphs

    -- each component still has its outer face;
    components :: NonEmptyVector ( Component Primal s, FaceId (Wrap s) )
    -- this is where we do the main work we replace the data of each vertex, dart, and face
    -- in each component with a reference to their global Ids.
    -- Along the way we create builders (vvData, dData, and fData) that can actually
    -- construct the final 'rawVertexData', 'rawDartData', and 'rawFaceData'
    -- vectors.
    ( Comps _ _ _ vData dData fData, components) =
        imapAccumL go (Comps 0 0 0 mempty mempty mempty) (fromFoldable1 graphs)

    globalOuterFaceId :: FaceId s
    globalOuterFaceId = coerce @Int 0

    go i (Comps nv nd nf vB dB fB) (gr, localOuterFaceId) =
        ( Comps nv' nd' nf' (vB <> vB') (dB <> dB') (fB <> fB')
        , (gr3, localOuterFaceId)
        )
      where
        ((nv',vB'), gr1) = goVertices (Raw (coerce i))     nv gr
        ((nd',dB'), gr2) = goDarts    (Raw (coerce i))     nd gr1
        ((nf',fB'), gr3) = goFaces globalOuterFaceId
                                   localOuterFaceId
                                   (rawFace (coerce i))
                                   nf gr2
        rawFace ci fi x = RawFace (Just (ci, fi)) (FaceData mempty x)

    -- | Given a non-empty builder, construct the NonEmpty vector out of it
    -- pre: the builder is indeed non-empty.
    --
    -- in our case the use is indeed safe as we are guaranteed to have non-empty builders of
    -- vectors, faces etc.
    build  :: Builder.Builder a -> NonEmptyVector a
    build  = NonEmptyV.unsafeFromVector . Builder.build

    outerFaceData =
      RawFace Nothing $ FaceData (foldMap (Seq.singleton . computeHole) components)
                                 (combineOuterFace outerFaces)

    -- we compute the data of the a component, which in this case is a (global) Dart
    computeHole (gr,outerFaceId) = gr^.boundaryDartOf outerFaceId


data Comps v e f =
  Comps { _numVertices' :: {-#UNPACK#-}!Int -- ^ num vertices in earlier components
        , _numDarts'    :: {-#UNPACK#-}!Int -- ^ number of darts in earlier components
        , _numFaces'    :: {-#UNPACK#-}!Int -- ^ number of faces in earlier components
        , _vData'       :: v -- ^ information about vertices we've already seen
        , _eData'       :: e -- ^ information about the edges we've already seen
        , _fData'       :: f -- ^ information about the darts we've already seen
        }

-- | Computes, for each local vertex its global vertexId
goVertices        ::(VertexId (Wrap s) -> v -> v')
                  -> Int -- ^ vertex offset
                  -> CPlanarGraph Primal (Wrap s) v e f
                  -> ((Int, Builder.Builder v'), CPlanarGraph Primal (Wrap s) (VertexId s) e f)
goVertices raw nv = imapAccumLOf vertices
                                 (\vi (offSet,res) x -> let vi' = vi `shiftR` offSet
                                                        in ((offSet+1, res <> raw' vi x), vi')
                                 ) (nv, mempty)
  where
    shiftR i offSet = coerce $ (coerce i) + offSet
    raw' a b = Builder.singleton $ raw a b

-- | Computes for each ddart the global dart
goDarts        :: ( Dart.Dart (Wrap s) -> e -> e') -- ^ compute the new dart data
               -> Int -- ^ initial offset
               -> CPlanarGraph Primal (Wrap s) v e f
               -> ((Int, Builder.Builder e'), CPlanarGraph Primal (Wrap s) v (Dart.Dart s) f)
goDarts raw nd = imapAccumLOf darts
                                 (\d (offSet,res) x -> let d' = d `shiftR` offSet
                                                       in ((offSet+1, res <> raw' d x), d')
                                 ) (nd, mempty)
  where
    shiftR (Dart.Dart a dir) offSet = Dart.Dart (coerce $ (coerce a) + offSet) dir
    raw' a b = Builder.singleton $ raw a b

-- | Process the faces. replaces the face data in the component by references to the
-- global face Id's. In addition, for every internal face, we create a new
-- entry in the builder storing its actual data.
--
-- note that this function essentially throws away the data of the (local) outerface.
-- so it's important to retain that somewhere else first.
goFaces        :: FaceId s
               -- ^ The global outerFaceId
               -> FaceId (Wrap s)
               -- ^ The outer face in this component
               -> ( FaceId (Wrap s) -> f -> f')
               -- ^ returns the new face data; left means we have to create a new face
               -- right means we keep the current face.
               -> Int
                  -- ^ initial offset
               -> CPlanarGraph Primal (Wrap s) v e f
               -> ( (Int, Builder.Builder f')
                  , CPlanarGraph Primal (Wrap s) v e (FaceId s)
                  )
goFaces globalOuterFaceId localOuterFaceId raw nf = imapAccumLOf faces go (nf, mempty)
  where
    shiftR i offSet = coerce $ (coerce i) + offSet
    raw' a b = Builder.singleton $ raw a b
    go fi (offSet,res) x
      | fi == localOuterFaceId = ((offSet, res), globalOuterFaceId)
      | otherwise              = let fi' = fi `shiftR` offSet
                                 in ((offSet+1, res <> raw' fi x), fi')
      -- if this face is its local outerFaceId, we just refer to the
      -- global outerFaceId. Otherwise we will have to create a new
      -- global faceId (fi') that corresponds to this face, and store its
      -- data.


--------------------------------------------------------------------------------

instance PlanarGraph_ (PlaneGraph s vertex e f) where
  type DualGraphOf (PlaneGraph s vertex e f) = CPlanarGraph Dual s f e vertex
  type WorldOf     (PlaneGraph s vertex e f) = Primal

  dualGraph = dualGraph . view _PlanarGraph
  _DualFaceIx     _ = coerced
  _DualVertexIx   _ = coerced
  incidentFaceOf  d = _PlanarGraph .> incidentFaceOf d

  prevDartOf      d = _PlanarGraph .> prevDartOf d
  nextDartOf      d = _PlanarGraph .> nextDartOf d

  boundaryDartOf  f = _PlanarGraph .> boundaryDartOf f
  boundaryDarts   f g = undefined
                        -- FIXME!!!!!!

-- boundaryDarts :: FaceIx planarGraph -> planarGraph -> NonEmptyVector (DartIx planarGraph)

-- The darts bounding this face. The darts are reported in order along the face. This means that for internal faces the darts are reported in *counter clockwise* order along the boundary, whereas for the outer face the darts are reported in clockwise order.

-- running time: 𝑂(𝑘)
-- , where 𝑘 is the output size.


instance ( Point_ vertex 2 r, Ord r, Num r
         ) => PlaneGraph_ (PlaneGraph s vertex e f) vertex
instance ( Point_ vertex 2 r, Ord r, Num r
         ) => ConstructablePlaneGraph_ (PlaneGraph s vertex e f) vertex where
  fromEmbedding = undefined




--------------------------------------------------------------------------------

-- instance ConstructableGraph_ (PlanarGraph s Primal v e f) where
--   fromAdjacencyLists xs = PlanarGraph components
--                                       vtxData dartData faceData
--     where


--       gr :: Graph v e
--       gr = fromAdjacencyLists xs

--       components' = dff gr



    -- PlanarGraph comps






--------------------------------------------------------------------------------
-- * Move to PlaneGraph

--------------------------------------------------------------------------------

-- instance ( Point_ v 2 (NumType v)
--          , Ord (NumType v), Num (NumType v)

--          ) => PlanarGraph_ (PlaneGraph s v e f) where
--   type DualGraphOf (PlaneGraph s v e f) = PlanarGraph Dual s f e v


--------------------------------------------------------------------------------



{-
-}



instance ( -- PlanarGraph_ (Component s)
         -- , IsComponent s
         -- , EdgeIx   (Component s) ~ Dart.Dart (Wrap s)
         -- , Edge     (Component s) ~ Dart.Dart s
         ) => PlanarGraph_ (PlanarGraph w s v e f) where
  -- dualGraph, (incidentFaceOf | leftFaceOf), prevDartOf, nextDartOf, boundaryDartOf, boundaryDartOf, boundaryDarts
  type DualGraphOf (PlanarGraph w s v e f) = CPlanarGraph (DualOf w) s f e v
  type WorldOf     (PlanarGraph w s v e f) = w

  dualGraph =  computeDualGraph
  _DualFaceIx     _ = coerced
  _DualVertexIx   _ = coerced

  incidentFaceOf  d = \pF gr -> let (_,d', c) = asLocalD d gr
                                    fi        = c^.incidentFaceOf d'
                                in singular (faceAt fi) pF gr


  prevDartOf      d = \pF gr -> let (_,d', c) = asLocalD d gr
                                    prevD     = c^.prevDartOf d'
                                in singular (dartAt prevD) pF gr

  nextDartOf      d = \pF gr -> let (_,d', c) = asLocalD d gr
                                    nextD     = c^.nextDartOf d'
                                in singular (dartAt nextD) pF gr

  boundaryDartOf _ = undefined

  -- boundaryDartOf  f = \pF gr -> let (_,f', c) = asLocalF f gr
  --                                   d         = c^.boundaryDartOf f'
  --                               in singular (dartAt d) pF gr

  boundaryDarts   f g = undefined



computeDualGraph :: PlanarGraph w s v e f -> CPlanarGraph (DualOf w) s f e v
computeDualGraph = undefined



testPoly :: SimplePolygon (Point 2 Int :+ Int)
testPoly = uncheckedFromCCWPoints $ NonEmpty.fromList
           [ origin :+ 0
           , Point2 10 0 :+ 1
           , Point2 10 10 :+ 2
           , Point2 0 10 :+ 3
           ]


spec :: Spec
spec = describe "Constructing a PlaneGraph from overlapping Polygons" $ do
         it "vertices testPoly ok" $
           let g = fromSimplePolygonWith testPoly "In" "Out"
           in (g^..vertices) `shouldBe` (testPoly^..vertices.asIndexedExt)
         it "vertices testPoly ok indexed" $
           let g = fromSimplePolygonWith testPoly "In" "Out"
           in allOf (vertices.withIndex) (\(i,_ :+ j) -> coerce i == j) g `shouldBe` True
         -- it "all inside" $
         --   let g = fromSimplePolygonWith testPoly "In" "Out"
         --   in allOf (vertices.withIndex) (\v _ -> v&outNeighboursOfByDart )
         testIpe [osp|components.ipe|]
                 [osp|components.out|]

--------------------------------------------------------------------------------

readInput      :: OsPath -> IO (NonEmpty (ClosedLineSegment (Point 2 R) :+ IpeAttributes Path R))
readInput inFP = NonEmpty.fromList <$>
                 readAllFrom ([osp|data/test-with-ipe/PlaneGraph|] </> inFP)


testIpe inFP outFP = describe ("Constructing PlaneGraph from " <> show inFP) $ do
    segs <- runIO $ readInput inFP
    let (gr :: PlaneGraph () (NonEmpty (Point 2 R))
                             (ClosedLineSegment (Point 2 R) :+ _)
                             ()
          )  = fromDisjointSegments segs


        segsbyColor :: MonoidalNEMap (IpeColor R) (NonEmpty (ClosedLineSegment (Point 2 R) :+ _))
        segsbyColor = foldMap1 (\seg -> MonoidalNEMap.singleton (seg^?!extra._Attr SStroke)
                                                                (NonEmpty.singleton seg)
                               ) segs


        graphs0 :: MonoidalNEMap _ (CPlaneGraph (Wrap ()) (NonEmpty (Point 2 R)) _ _)
        graphs0 = fromConnectedSegments <$> segsbyColor

        graphs :: MonoidalNEMap _
                                (CPlaneGraph (Wrap ())
                                      (Point 2 R)
                                      (ClosedLineSegment (Point 2 R) :+ IpeAttributes Path R)
                                      ()
                                )
        graphs = over vertices NonEmpty.head <$> graphs0

        grr :: PlaneGraph ()
                          (Point 2 R)
                          (ClosedLineSegment (Point 2 R) :+ IpeAttributes Path R)
                          ()
        grr = fromDisjointComponents (const ()) graphs



    xit "fromDisjointSegments" $ do
      show gr `shouldBe` ""
    xit "fromDisjointComponents" $ do
      show grr `shouldBe` ""

    it "fromDisjointComponents components" $ do
      numConnectedComponents grr `shouldBe` length graphs0
    it "fromDisjointComponents faces" $ do
      numFaces grr `shouldBe` 5

-- TODO: reenable this
--    xdescribe "golden" $ do
--      goldenWith [osp|data/test-with-ipe/PlaneGraph|]
--        (ipeContentGolden { name = outFP })
--        (drawGraph grr)




    -- goldenWith [osp|data/test-with-ipe/PlaneGraph/|]
    --            (ipeContentGolden { name = inFP})
    --            (drawGraph gr)


  -- tests: all edges of a component have the same color
