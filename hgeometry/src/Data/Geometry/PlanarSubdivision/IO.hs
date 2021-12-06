{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PlanarSubdivision.IO
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
--
-- Converting from/to Adjacency Representation of the Planar subdivision
--
--------------------------------------------------------------------------------
module Data.Geometry.PlanarSubdivision.IO
  (
    --  * Reading and Writing the Plane Graph to a file
    readPlanarSubdivision
  , writePlanarSubdivision



  -- * Converting to and from Adjacency list representions
  , toTreeRep
  , toAdjRep
  , fromAdjRep, fromAdjRep'

  ) where

-- import Data.PlanarGraph.Dart(Arc(..))
import           Control.Lens hiding (holesOf)
import           Control.Monad.State.Strict
import           Control.Monad.Writer
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.DList as DList
import qualified Data.Foldable as F
import           Data.Geometry.PlanarSubdivision.Basic
import           Data.Geometry.PlanarSubdivision.Raw
import           Data.Geometry.PlanarSubdivision.TreeRep
import qualified Data.PlaneGraph as PG
import qualified Data.PlaneGraph.AdjRep as PG
import qualified Data.PlaneGraph.IO as PGIO
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Yaml (ParseException)
import           Data.Yaml.Util
-- import           VectorBuilder.Builder (vector, Bilder)
-- import           VectorBuilder.Vector (build)

--------------------------------------------------------------------------------
-- * Reading and Writing the Plane Graph

-- | Reads a plane graph from a bytestring
readPlanarSubdivision :: forall s v e f r. (FromJSON v, FromJSON e, FromJSON f, FromJSON r)
                      => B.ByteString
                      -> Either ParseException (PlanarSubdivision s v e f r)
readPlanarSubdivision = decodeYaml

-- | Writes a plane graph to a bytestring
writePlanarSubdivision :: (ToJSON v, ToJSON e, ToJSON f, ToJSON r)
                       => PlanarSubdivision s v e f r -> B.ByteString
writePlanarSubdivision = encodeYaml

----------------------------------------

instance (ToJSON v, ToJSON e, ToJSON f, ToJSON r) => ToJSON (PlanarSubdivision s v e f r) where
  toEncoding = toEncoding . toTreeRep
  toJSON     = toJSON     . toTreeRep

instance (FromJSON v, FromJSON e, FromJSON f, FromJSON r)
         => FromJSON (PlanarSubdivision s v e f r) where
  parseJSON v = fromTreeRep @s <$> parseJSON v

--------------------------------------------------------------------------------

-- | Convert to a Tree based representation.
--
-- The adjacencies of vertices are given in counter clockwise order.
--
-- running time: \(O(n)\)
toTreeRep :: PlanarSubdivision s v e f r -> PlanarSD v e f r
toTreeRep psd = let f0 = outerFaceId psd
                in PlanarSD (psd^.dataOf f0) (toInners psd f0)

-- | Creeat the innersd for each hole of the given face
toInners        :: PlanarSubdivision s v e f r -> FaceId' s -> [InnerSD v e f r]
toInners psd f0 = map (toInner psd) . F.toList $ holesOf f0 psd

-- | Compute the InnerSD corresponding to the component indicated by
-- the given dart. The given dart lies on the outside of the hole.
toInner       :: PlanarSubdivision s v e f r -> Dart s -> InnerSD v e f r
toInner psd d = Gr (mkAdj psd c <$> as) (mkFace psd c <$> fs)
  where
    (_,_,c)  = asLocalD d psd
    (Gr as fs) = PGIO.toAdjRep c

-- | Convert the 'Vtx' we created by locally converting the component
-- into a global one; i.e. collect all global information.
mkAdj                       :: forall s v e f r. PlanarSubdivision s v e f r
                            -> Component s r
                            -> Vtx (VertexId' s) (Dart s) r
                            -> Vtx v e r
mkAdj psd c (Vtx _ p ns vi) = Vtx (fromEnum vi) p (map makeGlobal ns) (psd^.dataOf vi)
  where
    makeGlobal (j,d) = let vj = c^.PG.dataOf (toEnum j :: VertexId' (Wrap s))
                       in (fromEnum vj, psd^.dataOf d)

-- | Make Face information.
mkFace                          :: forall s v e f r.
                                   PlanarSubdivision s v e f r
                                -> Component s r
                                -> PG.Face (FaceId' s)
                                -> Face v e f r
mkFace psd c (PG.Face (u,v) fi) = Face (toG u, toG v) (psd^.dataOf fi) (toInners psd fi)
  where
    toG i = fromEnum $ c^.PG.dataOf (toEnum i :: VertexId' (Wrap s))

--------------------------------------------------------------------------------
-- * From TreeRep

-- | Reads a planar subdivision from the given Tree-Rep representation.
fromTreeRep :: forall s v e f r. PlanarSD v e f r -> PlanarSubdivision s v e f r
fromTreeRep (PlanarSD ofD inners) =
    PlanarSubdivision pgs'' rawVtxData' rawDartData' rawFaceData'
  where
    (vs,cs) = bimap mkVec mkVec . runFromTreeRep $ mapM_ handleInner inners
    mkVec = fromAssocs . DList.toList

    -- rebuild the global vs vector by replacing the Ints  by the local vertexId's.
    rawVtxData' = rebuildRawVtxData pgs vs

    -- componetns with only vertex data
    pgs :: V.Vector (PlaneGraph (Wrap s) (VertexId' s) e (f, [ComponentId s]) r)
    pgs = fmap (fromInner vs) cs
    -- with also the dart data
    pgs' :: V.Vector (PlaneGraph (Wrap s) (VertexId' s) (Dart s) (f, [ComponentId s]) r)
    pgs' = withGlobalDarts rawDartData' pgs
    -- and finally also with face data
    pgs'' :: V.Vector (PlaneGraph (Wrap s) (VertexId' s) (Dart s) (FaceId' s) r)
    pgs'' = undefined pgs'

    -- builds the rawDart data vector. I.e. for every positive dart in
    -- the plane graphs representing the components we collect the
    -- dart together with its componentId (and the plane graph
    -- representing the component) for each positive dart we then thus
    -- create it, and its twin ; i.e. we make sure these appear
    -- consecutively and at the right position in the rawDart data vector.
    rawDartData' :: V.Vector (Raw s (Dart (Wrap s)) e)
    rawDartData' = V.concatMap (\(d, ci, pg) -> let d' = twin d in
                                                  V.fromList [ Raw ci d  (pg^.PG.dataOf d)
                                                             , Raw ci d' (pg^.PG.dataOf d')
                                                             ]
                               )
                 . V.concatMap Prelude.id
                 . V.imap (\ci pg -> (,ComponentId @s ci, pg) <$> PG.edges' pg) $ pgs



    rawFaceData' = V.cons (RawFace Nothing ofData) rawInnerFaces
    -- data of the outer face
    ofData = FaceData (fromVector . fmap (\pg -> pg^.PG.dataOf (PG.outerFaceDart pg)) $ pgs') ofD

    rawInnerFaces = undefined

fromVector v = Seq.fromFunction (V.length v) (v V.!)


-- Makes sure that the darts in all components accurately point to
-- their corresponding global dart
withGlobalDarts         :: forall s e f r.
                           V.Vector (Raw s (Dart (Wrap s)) e) -- ^ global raw dart data
                        -> V.Vector (PlaneGraph (Wrap s) (VertexId' s) e        (f, [ComponentId s]) r)
                        -> V.Vector (PlaneGraph (Wrap s) (VertexId' s) (Dart s) (f, [ComponentId s]) r)
withGlobalDarts gds pgs = V.zipWith writeTo distributed pgs
  where
    -- distribute the global darts to the right component
    distributed = V.create $ do
      v <- MV.replicate (V.length pgs) []
      iforM_ gds $ \di (Raw (ComponentId i) ld _) ->
        MV.modify v ((fromEnum ld, toEnum @(Dart s) di):) i
      pure v
    -- assing the raw dart data from the assocs
    writeTo assocs pg = pg&PG.rawDartData .~ fromAssocs assocs



-- | rebuild the global rawVertexData vector by replacing the Ints by the local
-- vertexId's.
--
-- It does this by traversing the vertices in the components. They
-- already store the global vertexId, so we can use it to update the
-- global data.
rebuildRawVtxData         :: V.Vector (PlaneGraph (Wrap s) (VertexId' s) e f r)
                          -> V.Vector (Raw s Int v)
                          -> V.Vector (Raw s (VertexId' (Wrap s)) v)
rebuildRawVtxData pgs gvs = V.create $ do
    v <- MV.new (V.length gvs)
    forM_ pgs $ \pg -> forM_ (PG.vertices pg) $ \(lv,VertexData _ (VertexId i)) ->
      MV.write v i $ (gvs V.! i) { _idxVal  = lv }
    pure v
  -- maybe we can use unsafeFreeze . modify . unsafeThaw here to avoid allocating another vector.


----------------------------------------


type InnerSD' s v e f r = Gr (Vtx v e r) (PG.Face (f, [ComponentId s]))

runFromTreeRep :: FromTreeRep s v e f r a -> ( DList.DList (Int, Raw s Int v)
                                             , DList.DList (Int, InnerSD' s v e f r)
                                             )
runFromTreeRep = flip evalState (ComponentId 0) . runWriterT . execWriterT


type FromTreeRep s v e f r =
  WriterT (DList.DList (Int, Raw s Int v))
          (WriterT (DList.DList (Int, InnerSD' s v e f r))
                   (State (ComponentId s))
          )
  -- we can output a new vertex (i, rawVtx) where is is the globalId and rawVtx
  -- is the raw vertex information associated with this vertex.
  --
  -- we can also output a new component; i.e. the componentId we
  -- assign to it and the component itself.
  --
  -- state mainstins the first free ci


-- | handles the particluar component, i.e. creates a new componentId
-- (which is returned in the end), and outputs the component, all its
-- decendant components, and the vertices in these components.
handleInner            :: InnerSD v e f r -> FromTreeRep s v e f r (ComponentId s)
handleInner (Gr as fs) = do ci <- nextCI
                            zipWithM_ (report ci) [0..] as
                            fs' <-  mapM go fs
                            tellC (ci, Gr as fs')
                            pure ci
  where
    -- re-assign the vertices a local index that we can use to construct a graph out of it.
    report ci li (Vtx i _ _ v) = tellV (i, Raw ci li v)
    go (Face e f hs) = (\cis -> PG.Face e (f,cis)) <$> mapM handleInner hs


-- | outputs a new vertex
tellV :: (Int, Raw s Int v) -> FromTreeRep s v e f r ()
tellV = tell . DList.singleton

-- | outputs a new component
tellC                    :: (ComponentId s, InnerSD' s v e f r) -> FromTreeRep s v e f r ()
tellC (ComponentId i, x) = lift $ tell (DList.singleton (i,x))

-- | gets the next available CI.
nextCI :: FromTreeRep s v e f r (ComponentId s)
nextCI = do ci@(ComponentId i) <- get
            put $ ComponentId (i+1)
            pure ci

----------------------------------------

-- | build a vector out of an association list
fromAssocs    :: [(Int,a)] -> V.Vector a
fromAssocs xs = V.create $ do v <- MV.new (length xs)
                              forM_ xs $ uncurry (MV.write v)
                              pure v


------------------------------------------

-- | creates a planegraph for this component, taking care of the vertex mapping as well.
fromInner                :: V.Vector (Raw s Int v) -- ^ provides the mapping to local ints'
                         -> InnerSD' s v e f r
                         -> PlaneGraph (Wrap s) (VertexId' s) e (f, [ComponentId s]) r
fromInner vs (Gr ajs fs) = fromAdjRep $ Gr ajs' fs'
  where
    ajs' = map makeLocal ajs
    fs'  = map (\(PG.Face (i,j) (f,hs)) -> PG.Face (idxOf i, idxOf j) (f,hs)) fs


    makeLocal (Vtx i p ns _) = Vtx (idxOf i) p (map (first idxOf) ns) (VertexId i)
    idxOf i = vs^?!ix i.to _idxVal




--------------------------------------------------------------------------------

-- -- | Transforms the PlanarSubdivision into adjacency lists. For every
-- -- vertex, the adjacent vertices are given in counter clockwise order.
-- --
-- -- See 'toAdjacencyLists' for notes on how we handle self-loops.
-- --
-- -- running time: \(O(n)\)
-- toAdjRep :: PlanarSubdivision s v e f r -> Gr (Vtx v e r) (Face f)
-- toAdjRep = first (\(PGA.Vtx v aj (VertexData p x)) -> Vtx v p aj x) . PGIO.toAdjRep
--          .  view graph

toAdjRep = undefined

fromAdjRep = undefined

fromAdjRep'= undefined
