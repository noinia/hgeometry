{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PlnarSubdivision.Build
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Functions for Constructing a PlanarSubdivision
--
--------------------------------------------------------------------------------
module Data.Geometry.PlanarSubdivision.Build where


import           Control.Lens hiding (holes, holesOf, (.=))
import           Control.Monad.State.Strict
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.PlanarSubdivision.Basic
import           Data.Geometry.Polygon
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.PlaneGraph as PG
import           Data.PlaneGraph( PlaneGraph, PlanarGraph, dual
                                , Dart, VertexId(..), FaceId(..), twin
                                , World(..)
                                , VertexId', FaceId'
                                , VertexData, location, vData
                                , HasDataOf(..)
                                )
import           Data.Proxy
import qualified Data.Sequence as Seq
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------

-- | Helper type to make the mapping between the Darts in the full subdivision and the local
-- dart labels
data Label s v f = Label { _nextVertexId :: !Int
                         , _nextDartId   :: !Int
                         , _vRaws        :: [(VertexId' s, Raw s (VertexId' (Wrap s)) v)]
                         , _eRaws        :: [(Dart s,      Raw s (Dart      (Wrap s)) ())]
                         , _fRaws        :: [(FaceId' s,   Raw s (FaceId'   (Wrap s)) f)]
                         }
makeLenses ''Label


--------------------------------------------------------------------------------
-- * Constructing a planar subdivision

-- | Given a list of *disjoint* polygons that all live in the outer face
-- construct a planarsubdivsion representing them.
--
-- running time: \(O(n)\)
fromPolygons          :: forall proxy s p r f. (Ord r, Fractional r)
                      => proxy s
                      -> NonEmpty (SimplePolygon p r :+ f)
                      -> f -- ^ data outside the polygons
                      -> PlanarSubdivision s p () f r
fromPolygons _ pgs oD = PlanarSubdivision (V.fromList cs) rvData rdData rfData
  where
    rvData = buildVec $ ls^.vRaws
    rdData = buildVec $ ls^.eRaws
    rfData = buildVec $ (oId,Raw _ _ oD) : ls^.fRaws
    -- hmm; something is weird here; since for the outer face there is no uniue faceId

    -- lazyness makes us compute the dart label :)
    oD' = FaceData (Seq.fromList $ map getOuterFaceDartData cs) oId
    oId = FaceId $ VertexId 0


    computeLabels :: State (Label s p f) [Component s r]
    computeLabels = sequence $ zipWith (mkComponent oD')  [0..] (F.toList pgs)

    (cs,ls) = runState computeLabels (Label 0 0 [] [] [])


    getOuterFaceDartData g = g^.dataOf (PG.outerFaceDart g)


--------------------------------------------------------------------------------


getNext   :: Lens' (Label s v f) Int -> State (Label s v f) Int
getNext f = do i <- gets (^.f)
               f %= succ
               pure i





tellF x = fRaws %= (x:)
tellV x = vRaws %= (x:)
tellE x = eRaws %= (x:)


mkComponent                :: forall s p r f. _ -> Int -> SimplePolygon p r :+ f
                           -> State (Label s _ f) (Component s r)
mkComponent oD i (pg :+ f) = do tellF $ (fi,Raw c (mkFaceId 1) f)
                                assignVertices c g >>= assignEdges c
  where
   c = toEnum i -- create a new component
   mkFaceId = FaceId . VertexId
   fi = mkFaceId (i+1)
   iD = FaceData mempty fi
   g = PG.fromSimplePolygon wp pg iD oD
   wp = Proxy :: Proxy (Wrap s)






assignVtx        :: ComponentId s -> VertexId' (Wrap s) -> v
                 -> State (Label s v f) (VertexId' s)
assignVtx c vi v = do i <- getNext nextVertexId
                      let ui = VertexId i
                      tellV $ (ui,Raw c vi v)
                      pure ui

assignDart        :: ComponentId s -> Dart (Wrap s) -> ()
                  -> State (Label s v f) (Dart s)
assignDart c di x = do i <- getNext nextDartId
                       let d = toEnum i
                       tellE $ (d,Raw c di x)
                       pure d
  -- FIXME; this is very fragile!!!!!!!!!!!




assignVertices c = PG.traverseVertices (assignVtx c)

assignEdges   :: ComponentId s
              -> PlaneGraph (Wrap s) _ () _ r
              -> State _ (Component s r)
assignEdges c = PG.traverseDarts (assignDart c)


--------------------------------------------------------------------------------
-- * Generic Helper stuff


buildVec    :: Enum k => [(k,a)] -> V.Vector a
buildVec xs = V.create $ do
                v <- MV.new (length xs)
                forM_ xs $ \(k,x) ->
                  MV.write v (fromEnum k) x
                pure v
