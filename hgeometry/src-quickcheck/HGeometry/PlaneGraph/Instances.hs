--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PlaneGraph.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instance for a plane graph
--
--------------------------------------------------------------------------------
module HGeometry.PlaneGraph.Instances
  ( arbitraryPlaneGraph
  , QuickCheckWorld
  ) where

import Control.Lens
import Control.Monad.State
import           Control.Subcategory.Functor
import Data.Coerce
import Data.Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty(NonEmpty(..))
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import Data.Ord(comparing)
import Data.Proxy
import qualified Data.Set as Set
import Data.Set(Set)
import Data.Tree
import qualified Data.Vector.NonEmpty as Vector
import HGeometry.Ext
import HGeometry.Foldable.Util
import HGeometry.HyperPlane.NonVertical
import HGeometry.Instances ()
import HGeometry.Plane.LowerEnvelope.Connected(MinimizationDiagram(..), mapVertices)
import HGeometry.Plane.LowerEnvelope.Connected.Graph
import HGeometry.PlaneGraph
import HGeometry.Point
import HGeometry.Properties
import HGeometry.VoronoiDiagram
import HGeometry.VoronoiDiagram.ViaLowerEnvelope
import Hiraffe.AdjacencyListRep.Map
import Hiraffe.BFS.Pure
import Hiraffe.PlanarGraph(cPlanarGraph, vertexData)
import Hiraffe.PlanarGraph.Dart(Arc(..), Dart(..), Direction(..))
import Prelude hiding (filter)
import Test.QuickCheck hiding (Positive, Negative)
import Test.QuickCheck.Instances ()
import Witherable


import Debug.Trace
--------------------------------------------------------------------------------

data QuickCheckWorld

instance ( Arbitrary r
         , Ord r, Fractional r
         , Show r
         ) => Arbitrary (CPlaneGraph QuickCheckWorld (Point 2 r) () ()) where
  arbitrary = arbitraryPlaneGraph Proxy

-- general strategy:
-- 1) generate a bunch of random points uniformly at random.
-- 2) Construct the Voronoi diagram
-- 3) turn it into a triangulated graph (on its bounded vertices)
-- 4) sample a subset of its edges
-- 5) return the largest connected component


-- | Generate an arbitrary Plane Graph
arbitraryPlaneGraph       :: forall proxy s r.
                             ( Ord r, Fractional r, Arbitrary r
                             , Show r
                             )
                          => proxy s -> Gen (CPlaneGraph s (Point 2 r) () ())
arbitraryPlaneGraph proxy = do
    n                             <- scale (*2) arbitrary
    (pts :: NonEmpty (Point 2 r)) <- genNDistinct (max 10 n) arbitrary
    -- need at least a few vertices so that we generate at least a triangle in the planar graph
    case voronoiDiagram pts of
      AllColinear _  -> arbitraryPlaneGraph proxy -- retry
      ConnectedVD vd -> do let vd'   = vd&coerced %~ mapVertices (^.asPoint)
                               triGr = toTriangulatedPlaneGraph' . asMD $ vd'
                           gr <- markWitherableEdges (mapNeighbourOrder NEMap.toMap triGr)
                           case traverseNeighbourOrder NEMap.nonEmptyMap $ largestComponent gr of
                             Nothing  -> arbitraryPlaneGraph proxy -- retry
                             Just gr' -> pure $ toPlaneGraph proxy gr'


mapNeighbourOrder f = runIdentity . traverseNeighbourOrder (Identity . f)

--------------------------------------------------------------------------------

 -- | Returns the largest connected component in the graph; i.e. it shrinks
-- the graph to contain only the vertices/edges in this connected component.
largestComponent    :: (Ord i, Witherable f)
                    => GGraph f i v (Bool, e) -> GGraph f i v e
largestComponent gr = witherGraphTo tr gr
  where
    tr = maximumBy (comparing length) $ bff gr


-- | Convert a Vornooi diagram into a minimization diagram again
asMD :: ( Point_ point 2 r
        , Num r, Ord r
        ) => VoronoiDiagram' vertex point -> MinimizationDiagram r vertex (Plane r)
asMD = MinimizationDiagram . NEMap.mapKeys pointToPlane . coerce


-- | select a random subset of edges. I.e. it marks the edges we want to retain.
markWitherableEdges    :: Ord i => GGraph f i v e -> Gen (GGraph f i v (Bool,e))
markWitherableEdges gr = gr&edges %%~ \x -> (,x) <$> arbitrary'
  where
    arbitrary' = frequency [ (19, pure  True)
                           , (1,  pure False)
                           ]

-- | Retain only the selected subset of the vertices, and the edges marked
witherGraphTo               :: ( Foldable1 g, Witherable f, Ord i
                               ) => g i -> GGraph f i v (Bool, e) -> GGraph f i v e
witherGraphTo vs (Graph gr) = Graph $ fmap removeEdges m
  where
    -- retain only the vertices from vs
    m = foldMap1 (\u -> NEMap.singleton u (gr NEMap.! u)) vs
    -- remove edges to other components
    removeEdges (VertexData x ns no) = let ns' = Map.foldMapWithKey p' ns
                                       in VertexData x ns' (filter (`Map.member` ns') no)
    -- test if we should retain the edge; i.e. if the edge is marked and the
    -- other endpoint exists in our component
    p' i (retain,x) | retain && NEMap.member i m = Map.singleton i x
                    | otherwise                  = Map.empty


-- | Given a connected plane graph in adjacency list format; convert it into an actual
-- PlaneGraph.
--
-- \(O(n\log n)\)
toPlaneGraph             :: (Ord r, Foldable1 f)
                         => proxy s
                         -> GGraph f (Point 2 r) v e -> CPlaneGraph s (Point 2 r) () ()
toPlaneGraph _ (Graph m) = review _CPlanarGraph $ (cPlanarGraph theDarts)&vertexData .~ vtxData
  where
    vtxData = Vector.fromNonEmptyN1 (length m) (NEMap.keys m)

    --  a non-empty list of vertices, with for each vertex the darts in order around the vertex
    theDarts  = ((),) <$> evalState (sequence' theDarts') (0 :+ Map.empty)
    theDarts' = toNonEmpty $ NEMap.mapWithKey toIncidentDarts m
    -- turn the outgoing edges of u into darts
    toIncidentDarts u (VertexData _ _ neighOrder) =
      (\v -> (toDart u v, ())) <$> toNonEmpty neighOrder
    -- create the dart corresponding to vertices u and v

    toDart u v | u <= v    =  flip Dart Positive <$> arc u v
               | otherwise =  flip Dart Negative <$> arc v u

    arc u v = gets (arcOf (u,v)) >>= \case
                Just a  -> pure a
                Nothing -> do a <- nextArc
                              modify $ insertArc (u,v) a
                              pure a

    arcOf x       = Map.lookup x . view extra
    insertArc k v = over extra $ Map.insert k v

    nextArc = do i <- gets (view core)
                 modify $ over core (+1)
                 pure $ Arc i


sequence' :: Applicative m => NonEmpty (NonEmpty (m a, b)) -> m (NonEmpty (NonEmpty (a,b)))
sequence' = traverse $ traverse (\(fa,b) -> (,b) <$> fa)

--------------------------------------------------------------------------------0

-- | pre: n >= 1
genNDistinct       :: Eq a => Int -> Gen a -> Gen (NonEmpty a)
genNDistinct n gen = NonEmpty.fromList <$> go [] n
  where
    go acc 0  = pure acc
    go acc n' = do x <- gen `suchThat` (\x' -> all (/= x') acc)
                   go (x:acc) (n'-1)
