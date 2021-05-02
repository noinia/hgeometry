-- |
-- Module      :  Algorithms.Geometry.PolyLineSimplification.ImaiIri
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Algorithms.Geometry.PolyLineSimplification.ImaiIri where

import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Vector
import qualified Data.LSeq as LSeq
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import           Data.Tree
import qualified Data.Vector as V
import           Witherable

--------------------------------------------------------------------------------

-- | Line simplification with the Imai-Iri alogrithm. Given a distance
-- value eps and a polyline pl, constructs a simplification of pl
-- (i.e. with vertices from pl) s.t. all other vertices are within
-- dist eps to the original polyline.
--
-- Running time: \( O(n^2) \) worst case.
simplify     :: (Ord r, Fractional r, Arity d)
             => r -> PolyLine d p r -> PolyLine d p r
simplify eps = simplifyWith $ \shortcut subPoly -> all (closeTo shortcut) (subPoly^.points)
  where
    closeTo seg (p :+ _) = sqDistanceToSeg p seg  <= epsSq
    epsSq = eps*eps


-- | Given a function that tests if the shortcut is valid, compute a simplification.
simplifyWith            :: (LineSegment d p r -> PolyLine  d p r -> Bool)
                        -> PolyLine d p r -> PolyLine d p r
simplifyWith isValid pl = pl&points %~ (LSeq.promise @2 . extract path)
  where
    g    = mkGraph isValid pl
    spt  = bfs g
    path = case pathsTo (pl^.points.to F.length - 1) spt of
             []      -> error "no path found?"
             (pth:_) -> pth


type Graph = V.Vector [Int]

mkGraph         :: (LineSegment d p r -> PolyLine d p r -> Bool) -> PolyLine d p r -> Graph
mkGraph isValid = V.imap f . V.fromList . F.toList . allPrefixes
  where
    f i subPl = catMaybes
              $ zipWith isValid' [i+1..] . F.toList . allSuffixes $ subPl

    isValid' j subPoly = let shortcut = ClosedLineSegment (subPoly^.start) (subPoly^.end)
                         in if isValid shortcut subPoly then Just j else Nothing

allPrefixes    :: PolyLine d p r -> Seq.Seq (PolyLine d p r)
allPrefixes pl = mapMaybe mkPolyLine . Seq.tails . LSeq.toSeq $ pl^.points

mkPolyLine :: Seq.Seq (Point d r :+ p) -> Maybe (PolyLine d p r)
mkPolyLine = fmap PolyLine . LSeq.eval @2 . LSeq.fromSeq

allSuffixes :: PolyLine d p r -> Seq.Seq (PolyLine d p r)
allSuffixes pl = mapMaybe mkPolyLine . Seq.drop 2 . Seq.inits . LSeq.toSeq $ pl^.points


-- | Get all paths to the particular element in the tree.
pathsTo   :: Eq a => a -> Tree a -> [NonEmpty a]
pathsTo x = findPaths (== x)

-- | All paths to the nodes satisfying the predicate.
findPaths   :: (a -> Bool) -> Tree a -> [NonEmpty a]
findPaths p = go
  where
    go (Node x chs) = case foldMap go chs of
                        []    | p x       -> [x:|[]]
                              | otherwise -> []
                        paths | p x       -> (x:|[]) : map (x NonEmpty.<|) paths
                              | otherwise ->           map (x NonEmpty.<|) paths


extract    :: NonEmpty Int -> LSeq.LSeq n a -> LSeq.LSeq 0 a
extract is = LSeq.fromList . extract' (F.toList is) 0 . F.toList


extract'                             :: [Int] -> Int -> [a] -> [a]
extract' []     _ _                  = []
extract' (_:_)  _ []                 = []
extract' (i:is) j (x:xs) | i == j    = x : extract' is (j+1) xs
                         | otherwise = extract' is (j+1) xs






bfs = undefined


tr :: Tree Int
tr = Node 0 [Node 1 [], Node 2 [Node 3 [], Node 2 [], Node 4 [Node 5 []]]]

poly = case fromPoints [origin :+ 0, Point2 1 1 :+ 1, Point2 2 2 :+ 2, Point2 3 3 :+ 3] of
         Just p -> p

test = Seq.fromList [0..5]
