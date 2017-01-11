module Data.PlanarGraphSpec where


import           Data.Util
import           Data.PlanarGraph
import           Data.Permutation(toCycleRep)
import           Test.Hspec
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Map.Strict as SM
import           Data.Semigroup



data TestG

type Vertex = VertexId TestG Primal_

-- | Report all adjacnecies from g missing in h
missingAdjacencies     :: PlanarGraph s w v e f -> PlanarGraph s w v e f
                    -> [(VertexId s w, VertexId s w)]
missingAdjacencies g h = concatMap f . vertices' $ g
  where
    f u = let adjUh = S.fromList . F.toList $ neighboursOf u h
          in F.toList . fmap (u,) . V.filter (`S.notMember` adjUh) $ neighboursOf u g


sameGraphs s g h = do
    describe ("Same Adjacencies " <> s) $ do
      it "Missing edges from g in h" $
          (missingAdjacencies g h) `shouldBe` []
      it "Missing edges from h in g" $
          (missingAdjacencies h g) `shouldBe` []




spec :: Spec
spec = sameGraphs "testEdges" (fromAdjacencyLists testEdges) (fromAdjacencyListsOld testEdges)




testEdges :: [(Vertex,[Vertex])]
testEdges = map (\(i,vs) -> (VertexId i, map VertexId vs))
            [ (0, [1])
            , (1, [0,2,4])
            , (2, [1,3,4])
            , (3, [2,5])
            , (4, [1,2,5])
            , (5, [3,4])
            ]

--------------------------------------------------------------------------------


-- - m: a Map, mapping edges, represented by a pair of vertexId's (u,v) with
--            u < v, to arcId's.
-- - a: the next available unused arcID
-- - x: the data value we are interested in computing
type STR' s b = STR (SM.Map (VertexId s Primal_,VertexId s Primal_) Int) Int b

-- | Construct a planar graph from a adjacency matrix. For every vertex, all
-- vertices should be given in counter clockwise order.
--
-- running time: $O(n \log n)$.
fromAdjacencyListsOld      :: forall s f.(Foldable f, Functor f)
                        => [(VertexId s Primal_, f (VertexId s Primal_))]
                        -> PlanarGraph s Primal_ () () ()
fromAdjacencyListsOld adjM = planarGraph' . toCycleRep n $ perm
  where
    n    = sum . fmap length $ perm
    perm = trd' . foldr toOrbit (STR mempty 0 mempty) $ adjM


    -- | Given a vertex with its adjacent vertices (u,vs) (in CCW order) convert this
    -- vertex with its adjacent vertices into an Orbit
    toOrbit                     :: Foldable f
                                => (VertexId s Primal_, f (VertexId s Primal_))
                                -> STR' s [[Dart s]]
                                -> STR' s [[Dart s]]
    toOrbit (u,vs) (STR m a dss) =
      let (STR m' a' ds') = foldr (toDart . (u,)) (STR m a mempty) . F.toList $ vs
      in STR m' a' (ds':dss)


    -- | Given an edge (u,v) and a triplet (m,a,ds) we construct a new dart
    -- representing this edge.
    toDart                    :: (VertexId s Primal_,VertexId s Primal_)
                              -> STR' s [Dart s]
                              -> STR' s [Dart s]
    toDart (u,v) (STR m a ds) = let dir = if u < v then Positive else Negative
                                    t'  = (min u v, max u v)
                               in case SM.lookup t' m of
      Just a' -> STR m                  a     (Dart (Arc a') dir : ds)
      Nothing -> STR (SM.insert t' a m) (a+1) (Dart (Arc a)  dir : ds)
