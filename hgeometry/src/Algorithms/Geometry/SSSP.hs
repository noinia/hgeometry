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
import           Data.Ext                                       (extra, type (:+) (..))
import qualified Data.FingerTree                                as F
import           Data.Geometry.PlanarSubdivision                (PolygonFaceData (..))
import           Data.Geometry.Point                            (Point, ccw, pattern CCW,
                                                                 pattern CW)
import           Data.List                                      (sortOn, (\\))
import           Data.Maybe                                     (fromMaybe)
import           Data.PlanarGraph                               ()
import           Data.PlaneGraph                                (FaceId (..), PlaneGraph,
                                                                 VertexData (..), VertexId', dual,
                                                                 graph, incidentEdges, leftFace,
                                                                 vertices)
import qualified Data.PlaneGraph                                as PlaneGraph
import           Data.Tree                                      (Tree (Node))
import qualified Data.Vector                                    as V
import qualified Data.Vector.Circular                           as CV
import qualified Data.Vector.Circular.Util                      as CV
import           Data.Vector.Unboxed                            (Vector)
import qualified Data.Vector.Unboxed                            as VU

-- import           Algorithms.Geometry.PolygonTriangulation.Triangulate
-- import           Data.Bitraversable
-- import           Data.Ext
-- import           Data.Geometry.Point
-- import           Data.Geometry.Polygon
-- import           Data.PlaneGraph (vertices, incidentEdges, leftFace, VertexData(..))
-- import qualified Data.PlanarGraph as Planar
-- import           Data.Proxy
-- import           Data.Maybe
-- import           Debug.Trace
-- import qualified Data.Vector.Circular as CV
-- import qualified Data.Vector.Circular.Util as CV
-- import Data.Ratio
-- import qualified Algorithms.Geometry.SSSP.Naive as Naive

-- p1 :: SimplePolygon () Rational
-- p1 = fromPoints $ map ext [Point2 0 0, Point2 1 0, Point2 1 1, Point2 0 1]

-- p2 :: SimplePolygon () Rational
-- p2 = fromPoints $ map ext [Point2 0 0, Point2 1 1, Point2 2 0, Point2 2 2, Point2 0 2]

-- p3 :: SimplePolygon () Rational
-- p3 = fromPoints $ map ext [Point2 2 0, Point2 3 1, Point2 4 1, Point2 2 2, Point2 0 1, Point2 1 1]

-- p4 :: SimplePolygon () Rational
-- p4 = fromPoints $ map ext $
--   [Point2 1  6
--   ,Point2 0.5  4
--   ,Point2 0  1.5
--   ,Point2 2  0
--   ,Point2 3  1.5
--   ,Point2 4  3
--   ,Point2 8 3]

-- p5 :: SimplePolygon () Rational
-- p5 = fromPoints
--  [Point2 (121476500528279 % 17592186044416) (940868147077711 % 17592186044416) :+ (),Point2 (106858149839557 % 17592186044416) (941364970998817 % 17592186044416) :+ (),Point2 (323393675321 % 68719476736) (934000795019089 % 17592186044416) :+ (),Point2 (8625042991556479 % 2251799813685248) (908118223297489 % 17592186044416) :+ (),Point2 (7464742244206753 % 2251799813685248) (451642227488709 % 8796093022208) :+ (),Point2 (7464651404945793 % 2251799813685248) (225821019136137 % 4398046511104) :+ (),Point2 (71196828790639 % 17592186044416) (450951575750627 % 8796093022208) :+ (),Point2 (87503380799573 % 17592186044416) (452779096862071 % 8796093022208) :+ (),Point2 (12329870497849 % 2199023255552) (224464412555587 % 4398046511104) :+ (),Point2 (54154537845749 % 8796093022208) (893748511844673 % 17592186044416) :+ (),Point2 (105353587004367 % 17592186044416) (228045817564183 % 4398046511104) :+ (),Point2 (115921890907129 % 17592186044416) (228047635767005 % 4398046511104) :+ (),Point2 (120381033328035 % 17592186044416) (229703109436419 % 4398046511104) :+ (),Point2 (15595590041479 % 2199023255552) (934919896545601 % 17592186044416) :+ ()]

-- -- Can trigger Double precision error in the naive implementation.
-- p6 :: SimplePolygon () Rational
-- p6 = fromPoints $ map ext
--   [Point2 4312521495778239 58119566291039296
--   ,Point2 3732371122103376 57810205118554752
--   ,Point2 3732325702472896 57810180898851072
--   ,Point2 4556597042600896 57721801696080256
--   ,Point2 5600216371172672 57955724398345088
--   ,Point2 6312893694898688 57462889614230272
--   ,Point2 6931780844255872 57199904758059072
--   ,Point2 6742629568279488 58379729296430848]

-- pIndex p n = CV.index (p^.outerBoundary) n

-- [0,0,0,2,2,2,2,2,7,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,69,69,69,69,69,69,69,35,35,69,69,38,41,40,41,69,69,69,69,53,53,53,53,53,53,53,53,69,69,69,57,59,59,69,62,62,69,69,65,67,67,68,69,0,0,75,75,74,75,0,77,78,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0] /=
-- [0,0,0,2,2,2,2,2,7,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,69,69,69,69,69,69,69,35,35,69,69,38,41,40,41,69,69,69,69,53,53,53,53,53,53,53,53,69,69,69,57,59,59,69,62,62,69,69,65,67,67,68,69,0,0,75,74,74,75,0,77,78,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]

-- fastSSSP :: (Ord r, Fractional r) => SimplePolygon b r -> SSSP
-- fastSSSP p =
--   let poly' = snd $ bimapAccumL (\a p -> (a+1,a)) (,) 0 p
--       graph = triangulate' Proxy poly'
--   in sssp graph

type SSSP = Vector Int

-- | O(n) Single-Source shortest path.
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

newtype Index r = Index (Point 2 r :+ Int) -- deriving (Show)

instance Show (Index r) where
  show = show . indexExtra

indexExtra :: Index r -> Int
indexExtra (Index p) = p^.extra

instance Eq (Index r) where
  Index (_ :+ a) == Index (_ :+ b) = a == b

type Chain r = F.FingerTree (MinMax r) (Index r)
data Funnel r = Funnel
  { funnelLeft  :: Chain r
  , funnelCusp  :: Index r
  , funnelRight :: Chain r
  } deriving (Show)

instance F.Measured (MinMax r) (Index r) where
  measure i = MinMax i i

splitFunnel :: (Fractional r, Ord r) => Index r -> Funnel r -> (Index r, Funnel r, Funnel r)
splitFunnel x Funnel{..}
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
    leftElt   = ringAccess <$> chainLeft funnelLeft
    rightElt  = ringAccess <$> chainLeft funnelRight
    chainLeft chain =
      case F.viewl chain of
        F.EmptyL   -> Nothing
        elt F.:< _ -> Just elt

-- O(n)
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
        (v, fL, fR) ->
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

-- p1: 5
-- p2: 2
-- x:  4
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
