{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.SSSP
-- Copyright   :  (C) David Himmelstrup
-- License     :  see the LICENSE file
-- Maintainer  :  David Himmelstrup
--------------------------------------------------------------------------------
module Algorithms.Geometry.SSSP
  ( SSSP
  , sssp
  ) where

import           Algorithms.Geometry.PolygonTriangulation.Types (PolygonEdgeType)
import           Algorithms.Graph.DFS                           (adjacencyLists, dfs')
import           Control.Lens                                   ((^.))
import qualified Data.CircularSeq                               as C
import           Data.Coerce                                    (coerce)
import           Data.Ext                                       (core)
import qualified Data.FingerTree                                as F
import           Data.Geometry.PlanarSubdivision                (PolygonFaceData (..))
import           Data.Geometry.Point                            (Point, ccw, pattern CCW,
                                                                 pattern CW)
import           Data.Geometry.Polygon                          (Polygon, SimplePolygon,
                                                                 outerBoundary)
import           Data.List                                      (sortOn, (\\))
import           Data.Maybe                                     (fromMaybe)
import           Data.PlanarGraph                               ()
import           Data.PlaneGraph                                (FaceId (..), PlaneGraph,
                                                                 VertexId (..), VertexId', dual,
                                                                 graph)
import qualified Data.PlaneGraph                                as PlaneGraph
import           Data.Tree                                      (Tree (Node))
import qualified Data.Vector                                    as V
import           Data.Vector.Unboxed                            (Vector)
import qualified Data.Vector.Unboxed                            as VU



type SSSP = Vector Int

-- | O(n) Single-Source shortest path.
sssp :: (Ord r, Fractional r)
  => PlaneGraph s Int PolygonEdgeType PolygonFaceData r
  -> SimplePolygon p r
  -> SSSP
sssp trig p =
    ssspFinger p d
  where
    FaceId outer = PlaneGraph.outerFaceId trig
    dualGraph = trig^.graph.dual
    dualTree' = dfs' (V.map (filter (/= outer)) $ adjacencyLists dualGraph) (VertexId 1)
    dualVS = fmap (\v -> PlaneGraph.boundaryVertices (FaceId v) trig) dualTree'
    trigTree = toTrigTree trig dualVS
    d = mkDual trigTree







--------------------------------------------------------------------------------
-- SSSP (with fingertree) implementation





data MinMax = MinMax Index Index | MinMaxEmpty deriving (Show)
instance Semigroup MinMax where
  MinMaxEmpty <> b = b
  a <> MinMaxEmpty = a
  MinMax a _b <> MinMax _c d
    = MinMax a d
instance Monoid MinMax where
  mempty = MinMaxEmpty

newtype Index = Index Int deriving (Show)

type Chain = F.FingerTree MinMax Index
data Funnel = Funnel
  { funnelLeft  :: Chain
  , funnelCusp  :: Index
  , funnelRight :: Chain
  }

instance F.Measured MinMax Index where
  measure i = MinMax i i

splitFunnel :: (Fractional r, Ord r) => SimplePolygon p r -> Index -> Funnel -> (Index, Funnel, Funnel)
splitFunnel p x Funnel{..}
    | isOnLeftChain =
      case doSearch isRightTurn funnelLeft of
        (lower, t, upper) ->
          ( t
          , Funnel upper t (F.singleton x)
          , Funnel (lower F.|> t F.|> x) funnelCusp funnelRight)
    | isOnRightChain =
      case doSearch isLeftTurn funnelRight of
        (lower, t, upper) ->
          ( t
          , Funnel funnelLeft funnelCusp (lower F.|> t F.|> x)
          , Funnel (F.singleton x) t upper)
    | otherwise =
      ( funnelCusp
      , Funnel funnelLeft funnelCusp (F.singleton x)
      , Funnel (F.singleton x) funnelCusp funnelRight)
  where
    isOnLeftChain  = fromMaybe False $
      isLeftTurnOrLinear cuspElt <$> leftElt <*> pure targetElt
    isOnRightChain = fromMaybe False $
      isRightTurnOrLinear cuspElt <$> rightElt <*> pure targetElt
    doSearch fn chain =
      case F.search (searchChain fn) (chain::Chain) of
        F.Position lower t upper -> (lower, t, upper)
        F.OnLeft                 -> error "cannot happen"
        F.OnRight                -> error "cannot happen"
        F.Nowhere                -> error "cannot happen"
    searchChain _ MinMaxEmpty _             = False
    searchChain _ _ MinMaxEmpty             = True
    searchChain check (MinMax _ l) (MinMax r _) =
      check (ringAccess p l) (ringAccess p r) targetElt
    cuspElt   = ringAccess p funnelCusp
    targetElt = ringAccess p x
    leftElt   = ringAccess p <$> chainLeft funnelLeft
    rightElt  = ringAccess p <$> chainLeft funnelRight
    chainLeft chain =
      case F.viewl chain of
        F.EmptyL   -> Nothing
        elt F.:< _ -> Just elt

-- O(n)
ssspFinger :: (Fractional r, Ord r) => SimplePolygon p r -> Dual -> SSSP
ssspFinger p d = toSSSP $
    case d of
      Dual (a,b,c) ab bc ca ->
        (a, a) :
        (b, a) :
        (c, a) :
        loopLeft a c ca ++
        worker (Funnel (F.singleton c) a (F.singleton b)) bc ++
        loopRight a b ab
  where
    toSSSP :: [(Index,Index)] -> SSSP
    toSSSP =
      VU.fromList . map snd . sortOn fst . (coerce :: [(Index,Index)] -> [(Int,Int)])
    loopLeft a outer l =
      case l of
        EmptyDual -> []
        NodeDual x l' r' ->
          (x,a) :
          worker (Funnel (F.singleton x) a (F.singleton outer)) r' ++
          loopLeft a x l'
    loopRight a outer r =
      case r of
        EmptyDual -> []
        NodeDual x l' r' ->
          (x, a) :
          worker (Funnel (F.singleton outer) a (F.singleton x)) l' ++
          loopRight a x r'
    worker _ EmptyDual = []
    worker f (NodeDual x l r) =
      case splitFunnel p x f of
        (v, fL, fR) ->
          (x, v) :
          worker fL l ++
          worker fR r


--------------------------------------------------------------------------------
-- Duals



data Dual = Dual (Index, Index, Index) -- (a,b,c)
                  DualTree -- borders ab
                  DualTree -- borders bc
                  DualTree -- borders ca
  deriving (Show)

data DualTree
  = EmptyDual
  | NodeDual Index -- axb triangle, a and b are from parent.
      DualTree -- borders xb
      DualTree -- borders ax
  deriving (Show)

-- poly :: SimplePolygon Int Rational
-- poly = fromPoints
--   [ Point2 0 0 :+ 0
--   , Point2 1 0 :+ 1
--   , Point2 2 0 :+ 2
--   , Point2 2 1 :+ 3
--   , Point2 1 1 :+ 4
--   , Point2 0 1 :+ 5
--   ]

-- triangulation :: PlaneGraph s Int PolygonEdgeType PolygonFaceData Rational
-- triangulation = triangulate' Proxy poly



toTrigTree :: PlaneGraph s Int PolygonEdgeType PolygonFaceData r
           -> Tree (V.Vector (VertexId' s))
           -> Tree (Int,Int,Int)
toTrigTree trig = fmap toTrig . fmap (fmap toDat)
  where
    toTrig v = case V.toList v of
      [a,b,c] -> (a,b,c)
      _       -> error "Algorithms.Geometry.SSSP: Invalid triangulation."
    toDat v =
      case trig ^. PlaneGraph.vertexDataOf v of
        PlaneGraph.VertexData _loc dat -> dat

-- pp :: Show a => Tree a -> IO ()
-- pp = putStrLn . drawTree . fmap show

mkDual :: Tree (Int,Int,Int) -> Dual
mkDual (Node (a,b,c) forest) =
    Dual (Index a,Index b,Index c)
      (dualTree a b forest)
      (dualTree b c forest)
      (dualTree c a forest)

dualTree :: Int -> Int -> [Tree (Int,Int,Int)] -> DualTree
dualTree p1 p2 (Node (a,b,c) sub:xs) =
  case [a,b,c] \\ [p1,p2] of
    [x] -> NodeDual (Index x) (dualTree p1 x sub) (dualTree p2 x sub)
    _   -> dualTree p1 p2 xs
dualTree _p1 _p2 [] = EmptyDual





--------------------------------------------------------------------------------
-- Helpers

ringAccess :: Polygon t p r -> Index -> Point 2 r
ringAccess p (Index idx) =
  (C.index (p ^. outerBoundary) idx) ^. core

isRightTurnOrLinear :: (Ord r, Num r) => Point 2 r -> Point 2 r -> Point 2 r -> Bool
isRightTurnOrLinear p1 p2 p3 = not $ isLeftTurn p1 p2 p3

isLeftTurnOrLinear :: (Ord r, Num r) => Point 2 r -> Point 2 r -> Point 2 r -> Bool
isLeftTurnOrLinear p1 p2 p3 = not $ isRightTurn p1 p2 p3

isLeftTurn :: (Ord r, Num r) => Point 2 r -> Point 2 r -> Point 2 r -> Bool
isLeftTurn p1 p2 p3 =
  ccw p1 p2 p3 == CCW

isRightTurn :: (Ord r, Num r) => Point 2 r -> Point 2 r -> Point 2 r -> Bool
isRightTurn p1 p2 p3 =
  ccw p1 p2 p3 == CW

