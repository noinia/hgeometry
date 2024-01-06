-- |
-- Module      :  Algorithms.Geometry.PolyLineSimplification.ImaiIri
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Algorithms.Geometry.PolyLineSimplification.ImaiIri
  ( simplify
  , simplifyWith
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.Graph (Graph,buildG)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import qualified Data.Sequence as Sequence
import           Data.Tree
import qualified Data.Vector as V
import qualified Data.Vector.NonEmpty as NonEmptyV
import           HGeometry.Ext
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Sequence.NonEmpty
import           HGeometry.Vector
import           Hiraffe.BFS (bfs)
import           Hiraffe.Tree (pathsTo)
import           Witherable

--------------------------------------------------------------------------------

-- | Line simplification with the Imai-Iri alogrithm. Given a distance
-- value eps and a polyline pl, constructs a simplification of pl
-- (i.e. with vertices from pl) s.t. all other vertices are within
-- dist eps to the original polyline.
--
-- Running time: \( O(n^2) \) time.
simplify     :: ( PolyLine_ polyLine point
                , Point_ point d r
                , Ord r, Fractional r
                , HasSquaredEuclideanDistance point
                )
             => r -> polyLine -> PolyLine point
simplify eps = simplifyWith $ \shortcut subPoly -> all (closeTo shortcut) (subPoly^.points)
  where
    closeTo seg (p :+ _) = squaredEuclideanDistTo p seg  <= epsSq
    epsSq = eps*eps

-- | Given a function that tests if the shortcut is valid, compute a
-- simplification using the Imai-Iri algorithm.
--
-- Running time: \( O(Tn^2 \) time, where \(T\) is the time to
-- evaluate the predicate.
simplifyWith            :: ( PolyLine_ polyLine point
                           , Point_ point d r
                           , Ord r, Fractional r
                           )
                        => (ClosedLineSegment point -> polyLine -> Bool)
                        -> polyLine -> PolyLine point
simplifyWith isValid pl = PolyLine $ extract path vs
  where
    pl'@(PolyLine vs) = vertices polyLineFromPoints pl
    g                 = mkGraph isValid vs
    spt               = bfs 0 g
    path              = case pathsTo (pl'^.end) spt of
                          []      -> error "no path found?"
                          (pth:_) -> pth

----------------------------------------

-- | Constructs the shortcut graph
mkGraph                          :: (ClosedLineSegment point -> PolyLine point  -> Bool)
                                 -> NonEmptyV.NonEmptyVector point -> Graph
mkGraph isValid pl@(PolyLine vs) =
    buildG rng [ (i,j) | i <- Array.range rng, j <- Array.range rng, i < j, isValid' i j ]
  where
    rng = (0,numVertices pl-1)
    isValid' i j = let subPoly = PolyLine $ NonEmptyV.unsafeFromVector $ NonEmptyV.slice i j vs
                       shortcut = ClosedLineSegment (sybPoly^.start) (subPoly.end)
                   in isValid shortcut subPoly

-- | Given a non-empty list of indices, and some LSeq, extract the elemnets
-- on those indices.
--
-- running time: \(O(n)\)
extract    :: NonEmpty Int
           -> NonEmptyV.NonEmptyVector point -> NonEmptyV.NonEmptyVector point
extract is = NonEmptyVector.unsafeFromList . extract' (F.toList is) 0 . F.toList

-- | Extract the indices
extract'                                 :: [Int] -> Int -> [a] -> [a]
extract' []         _ _                  = []
extract' (_:_)      _ []                 = []
extract' is'@(i:is) j (x:xs) | i == j    = x : extract' is (j+1) xs
                             | otherwise = extract' is' (j+1) xs

--------------------------------------------------------------------------------


-- tr :: Tree Int
-- tr = Node 0 [Node 1 [], Node 2 [Node 3 [], Node 2 [], Node 4 [Node 5 []]]]

-- poly :: PolyLine 2 Int R
-- poly = case fromPoints [origin :+ 0, Point2 1 1 :+ 1, Point2 2 2 :+ 2, Point2 3 3 :+ 3] of
--          Just p -> p

-- test = Seq.fromList [0..5]

-- myTree :: Tree Int
-- myTree = Node {rootLabel = 0, subForest = [Node {rootLabel = 1, subForest = []}
--                                        ,Node {rootLabel = 2, subForest = []}
--                                        ,Node {rootLabel = 3, subForest = []}]
--            }
