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
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           Data.Yaml (ParseException)
import           Data.Yaml.Util

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


-- FIXME: InnerSD is more or less a Gr as well. So maybe use that
-- instead.  the only difference is that apparently a PG.Face refers
-- to an incident edge. My guess is that we need this as well to
-- reconstruct the PSD.

--------------------------------------------------------------------------------
-- * From TreeRep

-- data Status s v e = Status !Int
--                        [(VertexId' s, ComponentId s, v) ]
--                        [(Dart s, ComponentId s, e)]
--               deriving (Show)

-- | Reads a planar subdivision from the given Tree-Rep representation.
fromTreeRep :: forall s v e f r. PlanarSD v e f r -> PlanarSubdivision s v e f r
fromTreeRep (PlanarSD ofD inners) = undefined
  where
    (vs,cs) = bimap (fromAssocs . DList.toList) ()
            . runFromTreeRep $ mapM_ handleInner inners







----------------------------------------


runFromTreeRep :: FromTreeRep s v e f r a -> ( DList.DList (Int, Raw s Int v)
                                             , DList.DList (ComponentId s, InnerSD v e f r)
                                             )
runFromTreeRep = flip evalState (ComponentId 0) . runWriterT . execWriterT


type FromTreeRep s v e f r =
  WriterT (DList.DList (Int, Raw s Int v))
          (WriterT (DList.DList (ComponentId s, InnerSD v e f r))
                   (State (ComponentId s))
          )

handleInner            :: InnerSD v e f r -> FromTreeRep s v e f r ()
handleInner (Gr as fs) = do ci <- nextCI
                            zipWithM_ (report ci) [0..] as
                            mapM_ go fs
  where
    -- re-assign the vertices a local index that we can use to construct a graph out of it.
    report ci li (Vtx i _ _ v) = tellV $ (i, Raw ci li v)
    go (Face _ _ hs) = mapM_ handleInner hs

tellV :: (Int, Raw s Int v) -> FromTreeRep s v e f r ()
tellV = tell . DList.singleton

tellC   :: (ComponentId s, InnerSD v e f r) -> FromTreeRep s v e f r ()
tellC x = lift $ tell (DList.singleton x)

nextCI :: FromTreeRep s v e f r (ComponentId s)
nextCI = do ci@(ComponentId i) <- get
            put $ ComponentId (i+1)
            pure ci

----------------------------------------

-- | build a vector out of an association list
fromAssocs    :: [(Int,a)] -> V.Vector a
fromAssocs xs = V.create $ do v <- MV.new (length xs)
                              forM_ xs $ \(i,x) -> MV.write v i x
                              pure v


-- ----------------------------------------

-- type OutputDarts s e = WriterT (DList.DList (Dart s, Raw s (Dart (Wrap s)) e))
--                                (State (Arc s))

-- tellE :: (Dart s, Raw s (Dart (Wrap s)) e) -> OutputDarts s e ()
-- tellE = tell . DList.singleton

-- nextArc :: OutputDarts s e (Arc s)
-- nextArc = do a@(Arc i) <- get
--              put $ Arc (i+1)
--              pure a


-- outputDarts :: PlaneGraph (Wrap s) (VertexId' s) e (f, [InnerSD v e f r]) r
--             -> OutputDarts s e (PlaneGraph (Wrap s) (VertexId' s) (Dart s) (f, [InnerSD v e f r]) r)
-- outputDarts pg = mapM_

--   edges' pg




-- fromInnerM                :: V.Vector (Raw s Int v) -- ^ provides the mapping to local ints'
--                          -> PlaneGraph (Wrap s) (VertexId' s) e (f, [InnerSD v e f r]) r
--                          -> OutputDarts s e
--   (PlaneGraph (Wrap s) (VertexId' s) e (f, [InnerSD v e f r]) r)

-- | creates a planegraph for this component, taking care of the vertex mapping as well.
fromInner                :: V.Vector (Raw s Int v) -- ^ provides the mapping to local ints'
                         -> InnerSD v e f r
                         -> PlaneGraph (Wrap s) (VertexId' s) e (f, [InnerSD v e f r]) r
fromInner vs (Gr ajs fs) = fromAdjRep $ Gr ajs' fs'
  where
    ajs' = map makeLocal ajs
    fs'  = map (\(Face (i,j) f hs) -> PG.Face (idxOf i, idxOf j) (f,hs)) fs

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
