{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuasiQuotes #-}
module PlaneGraph.FromPolygonsSpec where

import           Control.Lens hiding (holes)
import           Data.Coerce
import           Data.Foldable
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import           Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import qualified Data.Sequence as Seq
import qualified Data.Vector as Vector
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NonEmptyV
import           Golden
import           HGeometry.Boundary
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.LineSegment
import           HGeometry.Sequence.NonEmpty
import           HGeometry.Map.NonEmpty.Monoidal (MonoidalNEMap)
import qualified HGeometry.Map.NonEmpty.Monoidal as MonoidalNEMap
import           R
import           HGeometry.PlaneGraph
import           HGeometry.Point
import           HGeometry.Polygon hiding (holes)
import           HGeometry.Properties
import           HGeometry.Vector.NonEmpty.Util ()
import           Hiraffe.PlanarGraph
import           Hiraffe.PlanarGraph.Component
import qualified Hiraffe.PlanarGraph.Dart as Dart
import           Ipe
import           PlaneGraph.RenderSpec
import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.Hspec.WithTempFile
import           Test.QuickCheck
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder
import qualified Hiraffe.AdjacencyListRep.Map as MapRep
import qualified Data.Vector.Mutable as MV
import           Control.Monad.ST
import Data.Functor.Bind.Class


import           Debug.Trace
--------------------------------------------------------------------------------

-- TODO:: this should probably go into Semigroups?

instance Apply (ST s) where
  (<.>) = (<*>)
  (<.) = (<*)
  (.>) = (*>)

--------------------------------------------------------------------------------



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
fromDisjointComponents :: forall s v e f r.
                          (Point_ v 2 r, Num r, Ord r
                          )
                       => (ViewL1 (Int,FaceId (Wrap s), f) -> f)
                          -- ^ How to combine the data from the "outer faces"
                       -> NonEmptyVector (CPlaneGraph (Wrap s) v e f)
                       -> PlaneGraph s v e f
fromDisjointComponents combineOuterFace graphs = review _PlanarGraph $
  fromDisjointComponents1 combineOuterFace
                          (fmap (\gr -> ( gr^._CPlanarGraph
                                        , gr^.outerFace.asIndex)
                                ) graphs)

-- | Given a bunch of components, each one with a given outer face,
-- construct a PlaneGraph from them that unifies all their outer faces; i.e.
-- embeds all components in a single outer face.
fromDisjointComponents' :: (ViewL1 (Int, FaceId (Wrap s), f) -> f)
                        -- ^ How to combine the data from the "outer faces"
                        -> NonEmptyVector ( CPlaneGraph (Wrap s) v e f
                                          , FaceId (Wrap s)
                                          )
                        -> PlaneGraph s v e f
fromDisjointComponents' combineOuterFace graphs = review _PlanarGraph $
  fromDisjointComponents1 combineOuterFace ((\(gr,fi) -> (gr^._CPlanarGraph,fi))
                                            <$> graphs
                                           )


type FaceDataMerger s f = Maybe (FaceId s, f)
                       -- ^ The faceId and data corresponding to the face we are merging into
                       -> ViewL1 (Int, FaceId (Wrap s), f)
                       -- ^ The data we are merging into the new face
                       -> f

-- | The data that we collect during mering
type FaceMergeData s f  = ( Maybe (ComponentId s, FaceId (Wrap s))
                          , LazyFaceData (Dart.Dart s,(Int, FaceId (Wrap s), f)) f
                          )

-- | Given a bunch of components, each one with a given outer face,
-- construct a PlanarGraph from them that unifies all their outer faces; i.e.
-- embeds all components in a single outer face.
fromDisjointComponents1               :: (ViewL1 (Int, FaceId (Wrap s), f) -> f)
                                        -- ^ How to combine the data from the "outer faces"
                                      -> NonEmptyVector ( CPlanarGraph Primal (Wrap s) v e f
                                                        , FaceId (Wrap s)
                                                        )
                                      -> PlanarGraph Primal s v e f
fromDisjointComponents1 merger graphs =
  mergeComponentsInto (\_nothing extras -> merger extras)
                      ((\(gr,outer) -> (gr,outer,Nothing)) <$> graphs)


-- we want the f to be lazy
data LazyFaceData h f = LazyFaceData (Seq.Seq h) f
  deriving (Show)


mergeComponentsInto'               :: forall s v e f.
                                     FaceDataMerger s f
                                  -> NonEmptyVector ( CPlaneGraph (Wrap s) v e f
                                                    , FaceId (Wrap s)
                                                    , Maybe (Int, FaceId (Wrap s))
                                                    )
                                     -- ^ All components are supposed to be given, in the
                                     -- proper componentId order.
                                  -> PlaneGraph s v e f
mergeComponentsInto' merger graphs =
  review _PlanarGraph . mergeComponentsInto merger $ over (traverse._1) (^._CPlanarGraph) graphs





-- | Merge a bunch of components into a single graph. For each
-- component we specify the component and the FaceId of its outer
-- face. In addition, we specify the componentId and the faceId that
-- we should identify this outer face with. If this is Nothing, it
-- should be the global outerface.
--
-- pre: at least one component has a 'Nothing' as the face containing it (i.e.)
-- there is at least one face that appears in the outer face
mergeComponentsInto               :: forall s v e f.
                                     FaceDataMerger s f
                                  -> NonEmptyVector ( CPlanarGraph Primal (Wrap s) v e f
                                                    , FaceId (Wrap s)
                                                    , Maybe (Int, FaceId (Wrap s))
                                                    )
                                     -- ^ All components are supposed to be given, in the
                                     -- proper componentId order.
                                  -> PlanarGraph Primal s v e f
mergeComponentsInto merger graphs = PlanarGraph theComponents
                                                (build   vData)
                                                (build   dData)
                                                fData'
  where
    -- Computes the connected components, and, for each face, all the
    -- data we need to apply merger on obtain the final face data.
    faceMergeData :: NonEmptyVector (FaceMergeData s f)
    (theComponents, faceMergeData) = runST $ do
          fDataVec  <- MV.replicate nf (Nothing,LazyFaceData mempty undefined)
          comps     <- constructComponentsAndData fDataVec
          fDataVec' <- Vector.unsafeFreeze fDataVec
          pure (comps, NonEmptyV.unsafeFromVector fDataVec')
    -- The components' array contains the CPlanarGraphs representing
    -- the invididual component.  The data values stored in this graph
    -- are all references to globalId's, except for the data stored at
    -- the outer face of each component. Replace those by proper
    -- references as well.
    constructComponentsAndData          :: forall s'. MV.MVector s' (FaceMergeData s f)
                                        -> ST s' (NonEmptyVector (Component Primal s))
    constructComponentsAndData fDataVec = ifor components' $ \ci comp ->
                                            comp&faces %%@~ handle ci
      where
        modifyMV i f = MV.modify fDataVec f (coerce i)

        handle                           :: Int
                                         -> FaceId (Wrap s)
                                         -> (Either (Dart.Dart s, Maybe (Int, FaceId (Wrap s)))
                                                    (FaceId s)
                                            , f
                                            )
                                         -> ST s' (FaceId s)
        handle ci fi (eGlobalId,theData) = case eGlobalId of
          Right globalFi   -> do modifyMV globalFi $ \(_,LazyFaceData hs _) ->
                                                        ( Just (coerce ci, fi)
                                                        , LazyFaceData hs theData
                                                        )
                                 pure globalFi
                              -- we are an inner face, so the face in the component
                              -- should just store the globalFaceId. Similarly, in our
                              -- global faceData array, we will store the (ComponentId, FaceId)
                              -- value, and initialize the initial face value at the given fData.

          Left (d, parent) -> do let globalFi = computeGlobalFi eGlobalId
                                 modifyMV globalFi $ \(orig, LazyFaceData hs x) ->
                                   ( orig
                                   , LazyFaceData (hs Seq.|> (d,(coerce ci, fi, theData))) x
                                   )
                                 pure globalFi

        -- TODO: we have some code duplication here.

        -- compute the globalFaceIndex of the given temporary faceIndex
        computeGlobalFi :: Either (_, Maybe (Int,FaceId (Wrap s))) (FaceId s) -> FaceId s
        computeGlobalFi = \case
          Right globalFi  -> globalFi
          Left (_,parent) -> case parent of
            Nothing                  -> globalOuterFaceId
            Just (parentCi,parentFi) -> computeGlobalFi . fst
                                          $ components'^?!ix parentCi.faceAt parentFi

    -- apply the merger function to merge the face data into its final data
    fData' :: NonEmptyVector (RawFace Primal s f)
    fData' = flip imap faceMergeData $ \fi (mFaceIdx, LazyFaceData extras orig) ->
                RawFace mFaceIdx $ case fmap snd <$> asViewL1 extras of
                  Nothing      -> FaceData mempty orig
                  Just extras' -> let orig' = (coerce fi,orig) <$ mFaceIdx
                                  in FaceData (fst <$> extras) (merger orig' extras')


    -- this is where we do the main work we replace the data of each
    -- vertex, dart, and face in each component with a reference to
    -- their global Ids. For the outer faces of each local component,
    -- we don't quite know the proper outerfaceId yet. So for the time
    -- being keep using our "Maybe (Int, FaceId)" 'ids'. In addition, for those
    -- outer faces we compute a dart on the boundary (which we will later need)
    -- for the face data of its parent face
    --
    -- Along the way we create builders (vvData, dData, and fData) that can actually
    -- construct the final 'rawVertexData', 'rawDartData', and 'rawFaceData'
    -- vectors.
    components' :: NonEmptyVector (CPlanarGraph Primal (Wrap s) (VertexId s) (Dart.Dart s)
                                                       (Either ( Dart.Dart s
                                                               , Maybe (Int,FaceId (Wrap s))
                                                               )
                                                               (FaceId s)
                                                       , f
                                                       )
                                                )
    ( Comps _ _ nf vData dData, components') =
        imapAccumL go (Comps 0 0 1 mempty mempty) graphs

    globalOuterFaceId :: FaceId s
    globalOuterFaceId = coerce @Int 0

    go i (Comps nv' nd' nf' vB dB) (gr, grOuterFaceId, containingFace) =
        ( Comps (nv' + numVertices gr)
                (nd' + (numDarts gr `div` 2)) -- number of arc's is just numberOf darts / 2
                (nf' + numFaces gr - 1) -- note the -1 since we don't count the outerFace of each component
                (vB <> vB') (dB <> dB')
        , gr3
        )
      where
        (vB', gr1) = goVertices (Raw (coerce i))     nv' gr
        (dB', gr2) = goDarts    (Raw (coerce i))     nd' gr1
        gr3        = goFaces (computeHole gr2 grOuterFaceId, containingFace)
                             grOuterFaceId
                             nf'
                             gr2

    -- | Given a non-empty builder, construct the NonEmpty vector out of it
    -- pre: the builder is indeed non-empty.
    --
    -- in our case the use is indeed safe as we are guaranteed to have non-empty builders of
    -- vectors, faces etc.
    build  :: Builder.Builder a -> NonEmptyVector a
    build  = NonEmptyV.unsafeFromVector . Builder.build

    -- we compute the data of the a component, which in this case is a (global) Dart
    computeHole gr outerFaceId' = gr^.boundaryDartOf outerFaceId'


data Comps v e f =
  Comps { _vertexOffset :: {-#UNPACK#-}!Int -- ^ num vertices in earlier components
        , _dartOffset   :: {-#UNPACK#-}!Int -- ^ number of darts in earlier components
        , _faceOffset   :: {-#UNPACK#-}!Int -- ^ number of faces in earlier components
        , _vData'       :: v -- ^ information about vertices we've already seen
        , _eData'       :: e -- ^ information about the edges we've already seen
        }

-- | Computes, for each local vertex its global vertexId
goVertices            :: (VertexId (Wrap s) -> v -> v')
                      -> Int -- ^ vertex offset
                      -> CPlanarGraph Primal (Wrap s) v e f
                      -> ( Builder.Builder v'
                         , CPlanarGraph Primal (Wrap s) (VertexId s) e f
                         )
goVertices raw offSet = imapAccumLOf vertices f mempty
  where
    f vi res x = let vi' = shiftR vi in (res <> raw' vi x, vi')
    shiftR i = coerce $ (coerce i) + offSet
    raw' a b = Builder.singleton $ raw a b

-- | Computes for each ddart the global dart
goDarts        :: ( Dart.Dart (Wrap s) -> e -> e') -- ^ compute the new dart data
               -> Int -- ^ initial offset
               -> CPlanarGraph Primal (Wrap s) v e f
               -> (Builder.Builder e', CPlanarGraph Primal (Wrap s) v (Dart.Dart s) f)
goDarts raw offSet = imapAccumLOf darts f mempty
  where
    f d res x = let d' = shiftR d
                in (res <> raw' d x, d')
    shiftR (Dart.Dart a dir) = Dart.Dart (coerce $ (coerce a) + offSet) dir
    raw' a b = Builder.singleton $ raw a b


-- | Process the faces. replaces the face data in the component by references to the
-- global face Id's. In addition, for every internal face, we create a new
-- entry in the builder storing its actual data.
--
-- note that this function essentially throws away the data of the (local) outerface.
-- so it's important to retain that somewhere else first.
goFaces        :: globalFaceId
               -- ^ The global outerFaceId
               -> FaceId (Wrap s)
               -- ^ The outer face in this component
               -> Int
                  -- ^ initial offset
               -> CPlanarGraph Primal (Wrap s) v e f
               -> CPlanarGraph Primal (Wrap s) v e (Either globalFaceId (FaceId s), f)
goFaces globalOuterFaceId localOuterFaceId offSet = snd . imapAccumLOf faces go offSet
  where
    go fi newId  x
      | fi == localOuterFaceId = (newId,     (Left globalOuterFaceId, x))
      | otherwise              = (newId + 1, (Right $ coerce newId, x))
      -- if this face is its local outerFaceId, we just refer to the
      -- global outerFaceId. Otherwise we will have to create a new
      -- global faceId (newId) that corresponds to this face, and store its
      -- data.



--------------------------------------------------------------------------------


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



--------------------------------------------------------------------------------





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
                 [osp|components.out.ipe|]
         testIpe1 [osp|components1.ipe|]
                  [osp|components1.out.ipe|]

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
        grr = fromDisjointComponents (const ()) (fromFoldable1 graphs)

    xit "fromDisjointSegments" $ do
      show gr `shouldBe` ""
    it "fromDisjointComponents" $ do
      show grr `shouldSatisfy` (startsWith
        "PlaneGraph (PlanarGraph {_components = [CPlanarGraph embedding = Permutation {_orbits = [[Dart (Arc 3) -1,Dart (Arc 4) +1,Dart (Arc 0) +1],[Dart (Arc 3) +1,Dart (Arc 6) -1,Dart (Arc 2) -1],[Dart (Arc 0) -1,Dart (Arc 1) +1,Dart (Arc 5) +1],[Dart (Arc 1) -1,Dart (Arc 4) -1,Dart (Arc 2) +1],[Dart (Arc 6) +1,Dart (Arc 7) -1],[Dart (Arc 5) -1,Dart (Arc 7) +1]],")


      -- goldenWith [osp|data/test-with-ipe/PlaneGraph/|]
      --            (ipeContentGolden { name = inFP})
      --            (drawGraph grr)


    it "fromDisjointComponents components" $ do
      numConnectedComponents grr `shouldBe` length graphs0
    it "fromDisjointComponents faces" $ do
      numFaces grr `shouldBe` 5

    describe "all edges ok" $ do
      for_ (grr^..darts.withIndex) $ \(d, x) ->
        it ("dart " <> show (d, x)) $  --  grr^.headOf d)) $
          show (grr^.headOf d) `shouldSatisfy` (/= "") -- somewhat silly test
   -- The head of Dart 5 is undefined for some reason?

{-
    describe "golden" $ do
      goldenWith [osp|data/test-with-ipe/PlaneGraph|]
                 (ipeContentGolden { name = outFP })
                 (drawGraph grr)
-}



    -- goldenWith [osp|data/test-with-ipe/PlaneGraph/|]
    --            (ipeContentGolden { name = inFP})
    --            (drawGraph gr)


  -- tests: all edges of a component have the same color

startsWith pref s = pref `List.isPrefixOf` s

--------------------------------------------------------------------------------

testIpe1 inFP outFP = describe ("Merging PlaneGraph from " <> show inFP) $ do
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
        grr = mergeComponentsInto' (\_orig _extras -> ())
                                  (NonEmptyV.unsafeFromList
                                     [ (black, outerFaceId black, Nothing)
                                     , (green, outerFaceId green, Nothing)
                                     , (red,   outerFaceId red, Just (0, fi))
                                     ]
                                  )
          where
            [_,fi,_,_] = black^..faces.asIndex
        black :: CPlaneGraph (Wrap ())
                                      (Point 2 R)
                                      (ClosedLineSegment (Point 2 R) :+ IpeAttributes Path R)
                                      ()
        (black:|[red,green]) = toNonEmpty graphs


    -- runIO $ do
      -- print "==============================="
      -- mapM_ print segs
      -- mapM_ print $ MonoidalNEMap.assocs segsbyColor
      -- mapM_ print $ MonoidalNEMap.assocs graphs
      -- mapM_ print $ grr^..interiorFacePolygons.withIndex

      -- mapM_ print $ black^..faces.withIndex

      -- let [_,_,_,(fi,_)] = grr^..interiorFaces.withIndex
      -- print "outerboundaryDarts"
      -- print $ outerBoundaryDarts fi grr -- grr^?!interiorFacePolygonAt fi
      -- print "outerboundaryvertices"
      -- print $ outerBoundaryVertices fi grr -- grr^?!interiorFacePolygonAt fi
      -- print "interiorfacepolygonat"
      -- print $ grr^?!interiorFacePolygonAt fi


      -- writeIpeFile outFP . singlePageFromContent  $ drawGraph grr

    pure ()
    -- it "combineinto" $ do
    --   show grr `shouldBe` ""
