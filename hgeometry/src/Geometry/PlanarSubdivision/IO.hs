{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.PlanarSubdivision.IO
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
--
-- Converting from/to Adjacency Representation of the Planar subdivision
--
--------------------------------------------------------------------------------
module Geometry.PlanarSubdivision.IO
  (
    --  * Reading and Writing the Plane Graph to a file
    readPlanarSubdivision
  , writePlanarSubdivision

  -- * Converting to and from Adjacency list representions
  , toTreeRep, fromTreeRep
  , toAdjRep, fromAdjRep
  ) where

-- import Data.PlanarGraph.Dart(Arc(..))
import           Data.Ord (comparing)
import           Control.Lens hiding (holesOf)
import           Control.Monad.State.Strict
import           Control.Monad.Writer
import           Data.Aeson
import           Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.DList as DList
import qualified Data.Foldable as F
import           Geometry.PlanarSubdivision.Basic
import           Geometry.PlanarSubdivision.Raw
import           Geometry.PlanarSubdivision.TreeRep
import qualified Data.PlaneGraph as PG
import qualified Data.PlaneGraph.AdjRep as PG
import qualified Data.PlaneGraph.IO as PGIO
import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Yaml (ParseException)
import           Data.Yaml.Util
import           Data.Ext
import           Geometry.Point
import           Geometry.Vector
import           Data.RealNumber.Rational
import           Debug.Trace

--------------------------------------------------------------------------------
-- * Reading and Writing the Plane Graph

-- | Reads a plane graph from a bytestring
readPlanarSubdivision :: forall s v e f r.
                         (FromJSON v, FromJSON e, FromJSON f, FromJSON r, Num r, Ord r)
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

instance (FromJSON v, FromJSON e, FromJSON f, FromJSON r, Num r, Ord r)
         => FromJSON (PlanarSubdivision s v e f r) where
  parseJSON v = undefined -- fromTreeRep @s <$> parseJSON v

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
--
-- running time: \(O(n)\)
fromTreeRep                       :: forall s v e f r. (Num r, Ord r, Show r, Show v, Show e, Show f)
                                  => PlanarSD v e f r -> PlanarSubdivision s v e f r
fromTreeRep (PlanarSD ofD inners) =
    PlanarSubdivision pgs'' rawVtxData' rawDartData' rawFaceData'
  where
    (vs,cs) = bimap mkVec mkVec . runFromTreeRep $ mapM_ handleInner inners
    mkVec = fromAssocs . DList.toList

    -- rebuild the global vs vector by replacing the Ints  by the local vertexId's.
    rawVtxData' = rebuildRawVtxData pgs vs

    -- componetns with only vertex data
    pgs :: V.Vector (PlaneGraph (Wrap s) (VertexId' s) e (f, [ComponentId s]) r)
    pgs = traceShow "woei" $ fmap (fromInner vs) cs
    -- with also the dart data
    pgs' :: V.Vector (PlaneGraph (Wrap s) (VertexId' s) (Dart s) (f, [ComponentId s]) r)
    pgs' = traceShow "woei2" $ withGlobalDarts rawDartData' pgs
    -- and finally also with face data
    pgs'' :: V.Vector (PlaneGraph (Wrap s) (VertexId' s) (Dart s) (FaceId' s) r)
    pgs'' = traceShow "woei3" $ withGlobalFaceIds rawFaceData' pgs'

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

    rawFaceData' :: V.Vector (RawFace s f)
    rawFaceData' = V.cons (RawFace Nothing ofData) (tr "rawInnerFaces" rawInnerFaces)
    -- data of the outer face
    ofData = FaceData mempty ofD
      -- FIXME!!     (fromVector . fmap outerBoundaryDart' . tr "???" $ pgs') ofD

    rawInnerFaces :: V.Vector (RawFace s f)
    rawInnerFaces = V.concatMap (\(ci, pg) -> -- tr "per coimponent " $
                                    (\(lfi, (fd,hcs)) -> RawFace (Just (ci,lfi))
                                                                 (FaceData (toHoles hcs) fd)
                                    ) <$> PG.internalFaces (tr ("the pg: " <> show (PG.faces' pg)) pg)
                                )
                   . V.imap (\ci pg -> tr "woeix" (ComponentId @s ci,pg)) . tr "goz"
                   $ pgs'

    -- for each of the holes, get a dart on their outer face;
    -- i.e. since the outer face of the component is actually the inner face of
    -- the face under consideration we can construct the hole data this way.
    toHoles ::  [ComponentId s] -> Seq.Seq (Dart s)
    toHoles = Seq.fromList . map (\(ComponentId ci) -> outerBoundaryDart' (pgs' V.! ci))


-- | given a component, get a dart --using its global dart id-- on the outer boundary.
outerBoundaryDart'    :: (Num r, Ord r) => PlaneGraph s' v (Dart s) f r -> Dart s
outerBoundaryDart' pg | trace "called" False = undefined
                      | otherwise   = pg^.PG.dataOf (PG.outerFaceDart pg)

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

-- | Makes sure that the darts in all components accurately point to
-- their corresponding global dart
withGlobalDarts         :: forall s v e f r. (Show r, Show v, Show e, Show f) =>
                           V.Vector (Raw s (Dart (Wrap s)) e) -- ^ global raw dart data
                        -> V.Vector (PlaneGraph (Wrap s) v e        f r)
                        -> V.Vector (PlaneGraph (Wrap s) v (Dart s) f r)
withGlobalDarts gds pgs = tr "ret" $ V.zipWith writeTo distributed pgs
  where
    -- distribute the global darts to the right component
    distributed = V.create $ do
      v <- MV.replicate (V.length pgs) []
      iforM_ gds $ \di (Raw (ComponentId i) ld _) ->
        MV.modify v ((fromEnum ld, toEnum @(Dart s) di):) i
      pure v
    -- assing the raw dart data from the assocs
    writeTo assocs pg = pg&PG.rawDartData .~ fromAssocs assocs

-- | Use the global face id's to assign the local faceIds, similar to how we update the darts
-- from their global information.
withGlobalFaceIds         :: forall s v e f g r. V.Vector (RawFace s f) -- ^ global faceId's
                          -> V.Vector (PlaneGraph (Wrap s) v e g           r)
                          -> V.Vector (PlaneGraph (Wrap s) v e (FaceId' s) r)
withGlobalFaceIds gfs pgs = V.zipWith writeTo distributed pgs
  where
    -- distribute the global faceId's to the right component
    distributed = V.create $ do
      v <- MV.replicate (V.length pgs) []
      iforM_ gfs $ \fi (RawFace mc _) -> case mc of
          Nothing                    -> pure ()
          Just ((ComponentId i), lf) -> MV.modify v ((fromEnum lf, toEnum @(FaceId' s) fi):) i
      pure v
    -- assing the raw dart data from the assocs
    writeTo assocs pg = pg&PG.faceData .~ tr "go" (fromAssocs assocs)

----------------------------------------


type InnerSD' s v e f r = Gr (Vtx v e r) (PG.Face (f, [ComponentId s]))

-- | run the fromTreeRep Monad
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

------------------------------------------

-- | creates a planegraph for this component, taking care of the vertex mapping as well.
fromInner                :: (Ord r, Num r, Show r, Show v, Show e, Show f) =>
  V.Vector (Raw s Int v) -- ^ provides the mapping to local ints'
                         -> InnerSD' s v e f r
                         -> PlaneGraph (Wrap s) (VertexId' s) e (f, [ComponentId s]) r
fromInner vs (Gr ajs fs) = PG.fromAdjRep $ Gr ajs' fs'
  where
    ajs' = map makeLocal ajs
    d = bimap idxOf idxOf $ localOuterFaceDart ajs

    err = error "no face data set"
    PG.Face _ (dummy,_) = Prelude.head fs -- FIXME, we should not really do this.

    fs'  = PG.Face d (dummy, [])
         : map (\(PG.Face (i,j) (f,hs)) -> PG.Face (idxOf i, idxOf j) (f,hs)) fs
    -- FIXME: I hope we are indeed getting rid of the undefines

    -- so far the best option seems to be to use PG.fromAdjRep', (or actually preferrably a version that also returns the edge oracle), so that we can set the face data ourselves *after* we've determined the outerFaceId.

    makeLocal (Vtx i p ns _) = Vtx (idxOf i) p (map (first idxOf) ns) (VertexId i)
    idxOf i = vs^?!ix i.to _idxVal


-- | computes a dart that has the "outer face" (local to this
-- component) to its left.
localOuterFaceDart    :: (Ord r, Num r) => [Vtx v e r] -> (Int,Int)
localOuterFaceDart es = (ui,vi)
  where
    (Vtx ui u ajs _) = F.minimumBy (comparing PG.loc) es

    (_ :+ vi) = F.minimumBy (cwCmpAroundWith' (Vector2 (-1) 0) (u :+ ()))
              . map (\(i,_) -> (vs V.! i) :+ i)
              $ ajs

    vs  = fromAssocs [(i,p) | Vtx i p _ _ <- es]


-- I think the idea was to construct a PG for the outerface as well. That basically means we need to find an edge incident to the outerface.

--------------------------------------------------------------------------------
-- * Generic helpers

-- | build a vector out of an association list
fromAssocs    :: [(Int,a)] -> V.Vector a
fromAssocs xs = V.create $ do v <- MV.new (length xs)
                              forM_ xs $ uncurry (MV.write v)
                              pure v


-- | helper function to convert vectors into Seqs.
fromVector   :: V.Vector a -> Seq.Seq a
fromVector v = Seq.fromFunction (V.length v) (v V.!)

--------------------------------------------------------------------------------

-- |  Alias for 'toTreeRep'
toAdjRep :: PlanarSubdivision s v e f r -> PlanarSD v e f r
toAdjRep = toTreeRep

-- | Alias for 'fromTreeRep'
fromAdjRep :: forall s v e f r. (Num r, Ord r) => PlanarSD v e f r -> PlanarSubdivision s v e f r
fromAdjRep = undefined -- fromTreeRep

--------------------------------------------------------------------------------

data Dummy

test :: PlanarSubdivision Dummy Int () String (RealNumber 3)
test = fromTreeRep myTriangle

test2 :: PlanarSubdivision Dummy Int () String (RealNumber 3)
test2 = fromTreeRep myTreeRep


tr :: Show b => String -> b -> b
tr s b = traceShow (s <> " : " <> show b) b



-- myGr = Gr {adjacencies = [Vtx {id = 0, loc = Point2 0 0, adj = [(1,()),(2,())], vData = VertexId 0},Vtx {id = 1, loc = Point2 10 0, adj = [(2,()),(0,())], vData = VertexId 1},Vtx {id = 2, loc = Point2 0 10, adj = [(0,()),(1,())], vData = VertexId 2}]
--           , faces = [Face {incidentEdge = (0,1), fData = "interior"}]
--           }
-- -- we are not passing any otherface data here....
