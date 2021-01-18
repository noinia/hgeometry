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
  , triangulate
  , sssp
  , visibilityDual
  , visibilityFinger
  , visibilitySensitive
  ) where

import Algorithms.Geometry.PolygonTriangulation.Triangulate (triangulate')
import Algorithms.Geometry.PolygonTriangulation.Types       (PolygonEdgeType)

import           Algorithms.Graph.DFS            (adjacencyLists, dfs', dfsSensitive)
import           Control.Lens                    ((^.))
import           Data.Bitraversable
import           Data.Either
import           Data.Ext                        (ext, extra, type (:+) (..))
import qualified Data.FingerTree                 as F
import           Data.Geometry.Line              (lineThrough)
import           Data.Geometry.LineSegment       (LineSegment (ClosedLineSegment, LineSegment))
import           Data.Geometry.PlanarSubdivision (PolygonFaceData (..))
import           Data.Geometry.Point             (Point, ccw, pattern CCW, pattern CW)
import           Data.Geometry.Polygon
import           Data.Intersection
import           Data.List                       (sortOn, (\\))
import           Data.Maybe                      (fromMaybe)
import           Data.PlanarGraph                (PlanarGraph)
import qualified Data.PlanarGraph                as Graph
import           Data.PlaneGraph                 (FaceId (..), PlaneGraph, VertexData (..),
                                                  VertexId, VertexId', dual, graph, incidentEdges,
                                                  leftFace, vertices)
import qualified Data.PlaneGraph                 as PlaneGraph
import           Data.Proxy
import           Data.Tree                       (Tree (Node))
import qualified Data.Vector                     as V
import qualified Data.Vector.Circular            as CV
import qualified Data.Vector.Circular.Util       as CV
import           Data.Vector.Unboxed             (Vector)
import qualified Data.Vector.Unboxed             as VU
import           Data.Vinyl
import           Data.Vinyl.CoRec

{-
type AbsOffset = Int

data TriangulatedPolygon t p r = TriangulatedPolygon
  { triangulatedMap   :: Map AbsOffset (VertexId () Primal)
  , triangulatedGraph :: PlaneGraph () AbsOffset PolygonEdgeType PolygonFaceData r
  , triangulatedPolygon :: Polygon t p r
  }
-}



-- | Single-source shortest paths tree. Both keys and values are vertex offset ints.
--
--   @parentOf(i) = sssp[i]@
type SSSP = Vector Int

-- FIXME: The code for generating the dual cannot deal with offsets so
--        we're running 'unsafeFromPoints . toPoints' to reset the polygon.
--        Super silly. Please fix.
-- | \( O(n \log n) \)
triangulate :: (Ord r, Fractional r) => SimplePolygon p r -> PlaneGraph s Int PolygonEdgeType PolygonFaceData r
triangulate p =
  let poly' = snd $ bimapAccumL (\a _ -> (a+1,a)) (,) 0 $ unsafeFromPoints $ toPoints p
  in triangulate' Proxy poly'

-- | \( O(n) \) Single-Source shortest path.
sssp :: (Ord r, Fractional r)
  => PlaneGraph s Int PolygonEdgeType PolygonFaceData r
  -> SSSP
sssp trig =
    ssspFinger d
  where
    Just v0 = fst <$> V.find (\(_vid, VertexData _ idx) -> idx == 0) (vertices trig)
    v0i = incidentEdges v0 trig
    Just (FaceId firstFace) = V.find (/= FaceId outer) $ V.map (`leftFace` trig) v0i
    FaceId outer = PlaneGraph.outerFaceId trig
    dualGraph = trig^.graph.dual
    dualTree' = dfs' (V.map (filter (/= outer)) $ adjacencyLists dualGraph) firstFace
    dualVS = fmap (\v -> toCCW $ PlaneGraph.boundaryVertices (FaceId v) trig) dualTree'
    trigTree = toTrigTree trig dualVS
    d = mkDual trigTree

    toCCW v =
      let cv = CV.reverse $ CV.unsafeFromVector v
      in CV.toVector $ fromMaybe cv $ CV.findRotateTo (== v0) cv

{-
1. Find the starting face.
-}
visibilitySensitive :: forall s r. (Ord r, Fractional r, Show r)
  => PlaneGraph s Int PolygonEdgeType PolygonFaceData r
  -> SimplePolygon () r
visibilitySensitive = fromPoints . map ext . rights . visibilityFinger . visibilityDual


visibilityDual :: forall s r. (Ord r, Fractional r)
  => PlaneGraph s Int PolygonEdgeType PolygonFaceData r
  -> Dual r
visibilityDual trig = d
  where
    Just v0 = fst <$> V.find (\(_vid, VertexData _ idx) -> idx == 0) (vertices trig)
    v0i = incidentEdges v0 trig

    outer :: VertexId s Graph.Dual
    FaceId outer = PlaneGraph.outerFaceId trig

    firstFace :: VertexId s Graph.Dual
    Just (FaceId firstFace) = V.find (/= FaceId outer) $ V.map (`leftFace` trig) v0i

    dualGraph :: PlanarGraph s Graph.Dual PolygonFaceData PolygonEdgeType (VertexData r Int)
    dualGraph = trig^.graph.dual

    dualTree' :: Tree (VertexId s Graph.Dual)
    dualTree' = dfsSensitive neigh firstFace

    neigh :: VertexId s Graph.Dual -> [VertexId s Graph.Dual]
    neigh v = V.toList $ V.filter (/=outer) $ Graph.neighboursOf v dualGraph

    dualVS :: Tree (V.Vector (VertexId' s))
    dualVS = fmap (\v -> toCCW $ PlaneGraph.boundaryVertices (FaceId v) trig) dualTree'

    trigTree :: Tree (Index r, Index r, Index r)
    trigTree = toTrigTree trig dualVS

    d :: Dual r
    d = mkDual trigTree

    toCCW v =
      let cv = CV.reverse $ CV.unsafeFromVector v
      in CV.toVector $ fromMaybe cv $ CV.findRotateTo (== v0) cv



visibilityFinger :: forall r. (Fractional r, Ord r, Show r) => Dual r -> [Either (Int, Int, Int) (Point 2 r)]
visibilityFinger d =
    case d of
      Dual (a,b,c) ab bc ca ->
        Left (indexExtra a, indexExtra b, indexExtra c) :
        worker (Funnel (F.singleton b) a F.empty) ab ++
        worker (Funnel (F.singleton c) a (F.singleton b)) bc ++
        worker (Funnel F.empty a (F.singleton c)) ca
  where
    -- Final edge is the leftmost of each funnel.
    -- The most visible are the rightmost of each funnel.
    -- Cut line segment.
    worker f EmptyDual =
      let edgeA = ringAccess $ funnelRightTop f
          edgeB = ringAccess $ funnelLeftTop f
          edge = ClosedLineSegment (ext edgeA) (ext edgeB)
          coneA = ringAccess $ funnelRightBottom f
          coneB = ringAccess $ funnelLeftBottom f
          lineA = lineThrough (ringAccess $ funnelCusp f) coneA
          lineB = lineThrough (ringAccess $ funnelCusp f) coneB
          -- findIntersection :: Line 2 r -> Point 2 r
          findIntersection line =
            match (edge `intersect` line) $
               H (\NoIntersection -> error "no intersection")
            :& H (\pt -> Right pt)
            :& H (\LineSegment{} -> error "line intersection")
            :& RNil
      in [if edgeA == coneA then Right coneA else findIntersection lineA] ++
         if edgeB == coneB then [] else [findIntersection lineB]
    worker f (NodeDual x l r) =
      Left (indexExtra $ fromMaybe (funnelCusp f) $ chainTop (funnelRight f)
           ,indexExtra x
           ,indexExtra $ fromMaybe (funnelCusp f) $ chainTop (funnelLeft f)) :
      case splitFunnel x f of
        (_v, fL, fR, dir) -> case dir of
          -- 'x' is to the left of the visibility cone. Everything further to the left cannot
          -- be visible to just go right.
          SplitLeft  -> worker fR r -- assert cusp of fR == cusp of f
          -- 'x' is visible from our cusp. Add it to the output and go both to the left and right.
          NoSplit    -> worker fR r ++ [Right (ringAccess x)] ++ worker fL l
          -- 'x' is to the right of the visibility cone. Everything further to the right cannot
          -- be visible to just go left.
          SplitRight -> worker fL l -- assert cusp of fL == cusp of f


--------------------------------------------------------------------------------
-- SSSP (with fingertree) implementation





data MinMax r = MinMax (Index r) (Index r) | MinMaxEmpty deriving (Show)
instance Semigroup (MinMax r) where
  MinMaxEmpty <> b = b
  a <> MinMaxEmpty = a
  MinMax a _b <> MinMax _c d
    = MinMax a d
instance Monoid (MinMax r) where
  mempty = MinMaxEmpty

-- Including the 'Point 2 r' here means we don't have to look it up.
-- This mattered since lookups used to be O(log n) rather than O(1).
newtype Index r = Index (Point 2 r :+ Int) -- deriving (Show)

instance Show (Index r) where
  show = show . indexExtra

indexExtra :: Index r -> Int
indexExtra (Index p) = p^.extra

instance Eq (Index r) where
  Index (_ :+ a) == Index (_ :+ b) = a == b

type Chain r = F.FingerTree (MinMax r) (Index r)
data Funnel r = Funnel
  { funnelLeft  :: Chain r -- Left-most element is furthest away from cusp.
  , funnelCusp  :: Index r
  , funnelRight :: Chain r -- Left-most element is furthest away from cusp.
  } deriving (Show)

-- Left side of the funnel, furthest away from the cusp.
funnelLeftTop :: Funnel r -> Index r
funnelLeftTop f = fromMaybe (funnelCusp f) $ chainTop (funnelLeft f)

-- Left side of the funnel, closest to the cusp.
funnelLeftBottom :: Funnel r -> Index r
funnelLeftBottom f = fromMaybe (funnelCusp f) $ chainBottom (funnelLeft f)

-- Right side of the funnel, furthest away from the cusp.
funnelRightTop :: Funnel r -> Index r
funnelRightTop f = fromMaybe (funnelCusp f) $ chainTop (funnelRight f)

-- Right side of the funnel, closest to the cusp.
funnelRightBottom :: Funnel r -> Index r
funnelRightBottom f = fromMaybe (funnelCusp f) $ chainBottom (funnelRight f)

-- Element closest to the cusp.
chainBottom :: Chain r -> Maybe (Index r)
chainBottom chain = case F.viewl chain of
  F.EmptyL   -> Nothing
  elt F.:< _ -> Just elt

-- Element furthest away from the cusp.
chainTop :: Chain r -> Maybe (Index r)
chainTop chain = case F.viewr chain of
  F.EmptyR   -> Nothing
  _ F.:> elt -> Just elt

instance F.Measured (MinMax r) (Index r) where
  measure i = MinMax i i

data SplitDirection = SplitLeft | NoSplit | SplitRight
  deriving (Show)

-- Split a funnel w.r.t. a point 'x'. There are three cases:
--   1. 'x' is visible from the cusp.
--   2. the path to 'x' hits the left side of the funnel.
--   3. the path to 'x' hits the right side of the funnel.
--
-- ********************************************************
-- Drawing guide:
--                       \     /
-- left side of funnel -> \   / <- right side of funnel
--                         \ /
--                          * <- cusp
-- ********************************************************
--
-- Case 1:
--      x
--   \     /
--    \   /
--     \ /
--      *
--
-- Case 2:
--
-- x
--   \     /
--    \   /
--     \ /
--      *
--
-- Case 3:
--
--           x
--   \     /
--    \   /
--     \ /
--      *
--
-- If 'x' is visible from the cusp, then the shortest path is a straight line and we're done.
-- If 'x' is not visible from the cusp, then we find the first point up the funnel where
-- 'x' becomes visible. We'll use a fingertree to find the point in O(log(min(n,m))). Because
-- of math, this adds up to O(n) for the entire SSSP tree.
--
-- Once we've found the first point that can see 'x', we split the funnel in two: One funnel
-- that will be used for points to the left of 'x' and one funnel for points to the right of
-- 'x'. Oh, "left" and "right" here are used to indicate branches in the dual tree.
splitFunnel :: (Fractional r, Ord r) => Index r -> Funnel r -> (Index r, Funnel r, Funnel r, SplitDirection)
splitFunnel x Funnel{..}
    | isOnLeftChain =
      case doSearch isRightTurn funnelLeft of
        (lower, t, upper) ->
          ( t
          , Funnel upper t (F.singleton x)
          , Funnel (lower F.|> t F.|> x) funnelCusp funnelRight
          , SplitLeft)
    | isOnRightChain =
      case doSearch isLeftTurn funnelRight of
        (lower, t, upper) ->
          ( t
          , Funnel funnelLeft funnelCusp (lower F.|> t F.|> x)
          , Funnel (F.singleton x) t upper
          , SplitRight)
    | otherwise =
      ( funnelCusp
      , Funnel funnelLeft funnelCusp (F.singleton x)
      , Funnel (F.singleton x) funnelCusp funnelRight
      , NoSplit)
  where
    isOnLeftChain  = fromMaybe False $
      isLeftTurnOrLinear cuspElt <$> leftElt <*> pure targetElt
    isOnRightChain = fromMaybe False $
      isRightTurnOrLinear cuspElt <$> rightElt <*> pure targetElt
    doSearch fn chain =
      case F.search (searchChain fn) chain of
        F.Position lower t upper -> (lower, t, upper)
        F.OnLeft                 -> error "cannot happen"
        F.OnRight                -> error "cannot happen"
        F.Nowhere                -> error "cannot happen"
    searchChain _ MinMaxEmpty _             = False
    searchChain _ _ MinMaxEmpty             = True
    searchChain check (MinMax _ l) (MinMax r _) =
      check (ringAccess l) (ringAccess r) targetElt
    cuspElt   = ringAccess funnelCusp
    targetElt = ringAccess x
    leftElt   = ringAccess <$> chainBottom funnelLeft
    rightElt  = ringAccess <$> chainBottom funnelRight

-- FIXME: Turning a list of pairs into a vector is incredibly inefficient.
--        Would be much faster to write directly into a mutable vector and
--        then freeze it at the end.
-- \( O(n) \)
ssspFinger :: (Fractional r, Ord r) => Dual r -> SSSP
ssspFinger d = toSSSP $
    case d of
      Dual (a,b,c) ab bc ca ->
        (a, a) :
        (b, a) :
        (c, a) :
        loopLeft a c ca ++
        worker (Funnel (F.singleton c) a (F.singleton b)) bc ++
        loopRight a b ab
  where
    toSSSP :: [(Index r,Index r)] -> SSSP
    toSSSP lst =
      VU.fromList . map snd . sortOn fst $
      [ (a,b) | (Index (_ :+ a), Index (_ :+ b)) <- lst ]
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
      case splitFunnel x f of
        (v, fL, fR, _) ->
          (x, v) :
          worker fL l ++
          worker fR r


--------------------------------------------------------------------------------
-- Duals



data Dual r = Dual (Index r, Index r, Index r) -- (a,b,c)
                   (DualTree r) -- borders ab
                   (DualTree r) -- borders bc
                   (DualTree r) -- borders ca
  deriving (Show)

data DualTree r
  = EmptyDual
  | NodeDual (Index r) -- axb triangle, a and b are from parent.
      (DualTree r) -- borders xb
      (DualTree r) -- borders ax
  deriving (Show)

toTrigTree :: PlaneGraph s Int PolygonEdgeType PolygonFaceData r
           -> Tree (V.Vector (VertexId' s))
           -> Tree (Index r,Index r,Index r)
toTrigTree trig = fmap toTrig . fmap (fmap toDat)
  where
    toTrig v = case V.toList v of
      [a,b,c] -> (a,b,c)
      _       -> error "Algorithms.Geometry.SSSP: Invalid triangulation."
    toDat v = Index $ PlaneGraph.vtxDataToExt (trig ^. PlaneGraph.vertexDataOf v)

-- pp :: Show a => Tree a -> IO ()
-- pp = putStrLn . drawTree . fmap show

mkDual :: Tree (Index r,Index r,Index r) -> Dual r
mkDual (Node (a,b,c) forest) =
    Dual (a, b, c)
      (dualTree a b forest)
      (dualTree b c forest)
      (dualTree c a forest)

dualTree :: Index r -> Index r -> [Tree (Index r,Index r,Index r)] -> DualTree r
dualTree p1 p2 (Node (a,b,c) sub:xs) =
  case [a,b,c] \\ [p1,p2] of
    [x] -> NodeDual x (dualTree x p2 sub) (dualTree p1 x sub)
    _   -> dualTree p1 p2 xs
dualTree _p1 _p2 [] = EmptyDual





--------------------------------------------------------------------------------
-- Helpers

ringAccess :: Index r -> Point 2 r
ringAccess (Index (pt :+ _idx)) = pt

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
