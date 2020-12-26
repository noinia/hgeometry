{-# LANGUAGE ScopedTypeVariables #-}
module Data.PlanarGraphSpec where

import Control.Lens(view,_3)
import           Data.Bifunctor
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import qualified Data.Map.Strict as SM
import           Data.Permutation (toCycleRep)
import           Data.PlanarGraph
import qualified Data.PlanarGraph as PlanarGraph
import           Data.PlanarGraph.IO
import qualified Data.Set as S
import           Data.Util
import qualified Data.Vector as V
import           Data.Yaml (prettyPrintParseException)
import           Data.Yaml.Util
import           Test.Hspec
import           Test.QuickCheck

--------------------------------------------------------------------------------
data TestG

type Vertex = VertexId TestG Primal

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
spec = do
    describe "PlanarGraph spec" $ do
      sameGraphs "testEdges" (fromAdjacencyLists testEdges) (fromAdjacencyListsOld testEdges)
    it "quickheck Dart:  (toEnum (fromEnum d)) = d" $
      property $ \(d :: Dart TestG) -> toEnum (fromEnum d) `shouldBe` d
    it "quickheck Dart: fromEnum (toEnum i) = i" $
      property $ \(NonNegative i) -> fromEnum ((toEnum i) :: Dart TestG) `shouldBe` i
    it "encode yaml test" $ do
      b <- B.readFile "test/Data/PlanarGraph/myGraph.yaml"
      encodeYaml (fromAdjacencyLists testEdges) `shouldBe` b
    it "decode yaml test" $ do
      (first prettyPrintParseException <$> decodeYamlFile "test/Data/PlanarGraph/myGraph.yaml")
      `shouldReturn`
      (Right $ fromAdjacencyLists testEdges)


testEdges :: [(Vertex,[Vertex])]
testEdges = map (\(i,vs) -> (VertexId i, map VertexId vs))
            [ (0, [1])
            , (1, [0,2,4])
            , (2, [1,3,4])
            , (3, [2,5])
            , (4, [1,2,5])
            , (5, [3,4])
            ]

-- testGraph = fromAdjacencyLists testEdges

-- enccode = let g =
--           in encodeYamlFile

--------------------------------------------------------------------------------


-- - m: a Map, mapping edges, represented by a pair of vertexId's (u,v) with
--            u < v, to arcId's.
-- - a: the next available unused arcID
-- - x: the data value we are interested in computing
type STR' s b = STR (SM.Map (VertexId s Primal,VertexId s Primal) Int) Int b

-- | Construct a planar graph from a adjacency matrix. For every vertex, all
-- vertices should be given in counter clockwise order.
--
-- running time: $O(n \log n)$.
fromAdjacencyListsOld      :: forall s f.(Foldable f, Functor f)
                        => [(VertexId s Primal, f (VertexId s Primal))]
                        -> PlanarGraph s Primal () () ()
fromAdjacencyListsOld adjM = planarGraph' . toCycleRep n $ perm
  where
    n    = sum . fmap length $ perm
    perm = view (_3) . foldr toOrbit (STR mempty 0 mempty) $ adjM


    -- | Given a vertex with its adjacent vertices (u,vs) (in CCW order) convert this
    -- vertex with its adjacent vertices into an Orbit
    toOrbit                     :: Foldable f
                                => (VertexId s Primal, f (VertexId s Primal))
                                -> STR' s [[Dart s]]
                                -> STR' s [[Dart s]]
    toOrbit (u,vs) (STR m a dss) =
      let (STR m' a' ds') = foldr (toDart . (u,)) (STR m a mempty) . F.toList $ vs
      in STR m' a' (ds':dss)


    -- | Given an edge (u,v) and a triplet (m,a,ds) we construct a new dart
    -- representing this edge.
    toDart                    :: (VertexId s Primal,VertexId s Primal)
                              -> STR' s [Dart s]
                              -> STR' s [Dart s]
    toDart (u,v) (STR m a ds) = let dir = if u < v then PlanarGraph.Positive else PlanarGraph.Negative
                                    t'  = (min u v, max u v)
                               in case SM.lookup t' m of
      Just a' -> STR m                  a     (Dart (Arc a') dir : ds)
      Nothing -> STR (SM.insert t' a m) (a+1) (Dart (Arc a)  dir : ds)
