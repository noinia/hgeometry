{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

-- | Given a list of *disjoint* polygons that all live in the outer face
-- construct a planarsubdivsion representing them.
--
-- running time: \(O(n)\)
fromPolygons           :: forall proxy s p r f. (Ord r, Fractional r)
                       => proxy s
                       -> NonEmpty (SimplePolygon p r :+ f)
                       -> f -- ^ data outside the polygons
                       -> PlanarSubdivision s p () f r
fromPolygons px pgs oD = PlanarSubdivision (V.fromList cs) rvData rdData rfData
  where
    rvData = buildVec $ ls^.vRaws
    rdData = buildVec $ ls^.eRaws
    rfData = buildVec $ ls^.fRaws


    -- lazyness makes us compute the dart label :)
    oD = FaceData (Seq.fromList $ map getOuterFaceDartData cs) (FaceId $ VertexId 0)

    computeLabels :: State (Label s p f) [Component s r]
    computeLabels = sequence $ zipWith (mkComponent oD)  [0..] (F.toList pgs)

    (cs,ls) = runState computeLabels (Label 0 0 [] [] [])


    getOuterFaceDartData g = g^.dataOf (PG.outerFaceDart g)




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




getNext   :: Lens' (Label s v f) Int -> State (Label s v f) Int
getNext f = do i <- gets (^.f)
               f %= succ
               pure i


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

-- assignDart c d = do i <- getNext nextDartId
--                     let ui = Dart i




-- assignVertices     :: _ -> PG.PlaneGraph s v e f r -> State _ (PG.PlaneGraph s v' e f r)
-- assignVertices c g = let vs = vertices g
--                          vs' = traverse (assignVtx c)
--                      in


   -- (pgs',SP _ rvData') = runState (labelAll pgs) (SP 0 [])


    -- -- holes in the outer face
    -- hs = undefined

    --

    -- oD' = FaceData hs (mkFaceId 0)
    -- -- rfData = F.fromList $ Raw c0 _ oD

    -- wp = Proxy :: Proxy (Wrap s)

    -- -- mkComponent      :: Int -> SimplePolygon p r
    -- --                  -> PG.PlaneGraph (Wrap s) p () (FaceData (Dart s) (FaceId' s)) r


-- labelGraph :: Applicative m => (v -> m v')
--            -> PG.PlaneGraph s v e f r
--            -> PG.PlaneGraph s v'

-- type Label p = SP Int [(Int,p)]

-- assignLabel     :: (Int -> l) -> p -> State (Label p) l
-- assignLabel f p = do i <- gets (^._1)
--                      modify $ \(SP _ xs) -> SP (succ i) ((i,p):xs)
--                      pure $ f i


-- assignVertices = PG.traverseVertices (assignLabel VertexId

-- assignVtx    :: Applicative m => ((VertexId' s,v) -> m v')
--              -> PG.PlaneGraph s v e f r -> m (PG.PlaneGraph s v' e f r)
-- assignVtx f g = let vs = vertices g
--                 in


-- assignAll :: PG.PlaneGraph ws p () fd r -> State (TwoLabels s p ())

-- mkComponent :: Int -> SimplePolygon p r :+ f-> State (TwoLabels ()
--                                                  )
--                ()



-- labelPG :: SimplePolygon p r -> LabelState p (SimplePolygon (VertexId' s) r)
-- labelPG = bitraverse (assignLabel VertexId) pure

-- labelAll :: NonEmpty (SimplePolygon p r :+ f)
--          -> LabelState p (NonEmpty (SimplePolygon (VertexId' s) r))
-- labelAll = traverse (\(pg :+ _) -> labelPG pg)

-- labelVertices    :: proxy s -> NonEmpty (SimplePolygon p r :+ f)
--                  -> SP (NonEmpty (SimplePolygon (VertexId' s) r :+ f), V.Vector p)
-- labelVertices px = foldr label (SP [] 0)
--   where
--     label =

-- labelWith        :: (i -> b) -> i -> SimplePolygon p r -> SP (SimplePolygon b r) i
-- labelWith f i pg = bitraverseVertices


--------------------------------------------------------------------------------
-- * Generic Helper stuff


buildVec    :: Enum k => [(k,a)] -> V.Vector a
buildVec xs = V.create $ do
                v <- MV.new (length xs)
                forM_ xs $ \(k,x) ->
                  MV.write v (fromEnum k) x
                pure v
