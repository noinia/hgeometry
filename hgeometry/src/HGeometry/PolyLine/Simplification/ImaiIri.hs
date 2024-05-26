-- |
-- Module      :  HGeometry.PolyLine.Simplification.ImaiIri
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.PolyLine.Simplification.ImaiIri
  ( simplify
  , simplifyWith
  ) where

import           Control.Lens
import qualified Data.Array as Array
import qualified Data.Foldable as F
import           Data.Graph (Graph,buildG)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector.NonEmpty as NonEmptyVector
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.PolyLine
import           Hiraffe.BFS (bfs)
import           Hiraffe.Graph
import           Hiraffe.Tree (pathsTo)

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
simplify eps =
    simplifyWith $ \shortcut subPoly -> allOf vertices (closeTo shortcut) subPoly
  where
    closeTo seg p = squaredEuclideanDistTo p seg <= epsSq
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
                        => (ClosedLineSegment point -> PolyLine point -> Bool)
                        -> polyLine -> PolyLine point
simplifyWith isValid pl = PolyLine $ extract path (pl'^._PolyLineF)
  where
    pl'  = polyLineFromPoints . toNonEmptyOf allPoints $ pl
    g    = mkGraph isValid pl'
    spt  = bfs g 0
    path = case pathsTo (numVertices pl' - 1) spt of
             []      -> error "no path found?"
             (pth:_) -> pth

----------------------------------------

-- | Constructs the shortcut graph
mkGraph                          :: (ClosedLineSegment point -> PolyLine point  -> Bool)
                                 -> PolyLine point -> Graph
mkGraph isValid pl@(PolyLine vs) =
    buildG rng [ (i,j) | i <- Array.range rng, j <- Array.range rng, i < j, isValid' i j ]
  where
    rng = (0,numVertices pl-1)
    isValid' i j = let subPoly  = PolyLine . NonEmptyVector.unsafeFromVector $
                                    NonEmptyVector.slice i (j+1-i) vs
                       shortcut = ClosedLineSegment (subPoly^.start) (subPoly^.end)
                   in isValid shortcut subPoly

-- | Given a non-empty list of indices, and some LSeq, extract the elemnets
-- on those indices.
--
-- running time: \(O(n)\)
extract    :: NonEmpty Int
           -> NonEmptyVector.NonEmptyVector point -> NonEmptyVector.NonEmptyVector point
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
