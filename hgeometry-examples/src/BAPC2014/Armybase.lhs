---
title: Army Base with Ternary Search
author: Frank Staals
@EXPECTED_RESULTS@: CORRECT
---

Army Base with Ternary Search
=============================

$O(n^2 \log n)$ solution.



> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module BAPC2014.Armybase where


> import Control.Lens((^.))
> import Data.Ix
> import Data.Ext
> import Data.Geometry.Triangle
> import Data.Geometry.Point
> import Data.Geometry.Polygon
> import Data.Geometry.Polygon.Convex
> import Algorithms.Geometry.ConvexHull.GrahamScan
> import qualified Data.Array   as A
> import qualified Data.Foldable as F
> import qualified Data.List     as L
> import qualified Data.List.NonEmpty as NonEmpty

Preliminaries
-------------


> type PointSet = [Point 2 Int]


A value of type Half should still be halved. I.e. 'Half 2x = x'

> newtype Half = Half Int
>                        deriving (Show,Eq,Ord)

> instance Num Half where
>   (Half a) + (Half b) = Half $ a + b
>   (Half a) - (Half b) = Half $ a - b
>   (Half a) * (Half b) = Half $ a * b
>   abs (Half a) = Half $ abs a
>   signum (Half a) = Half $ 2 * signum a
>   fromInteger a = Half $ 2 * fromInteger a

> showValue :: Half -> String
> showValue (Half i) = concat [ show $ i `div` 2
>                             , if odd i then ".5" else ""
>                             ]

> type Area = Half



> triangArea :: Triangle 2 p Int -> Half
> triangArea = Half . doubleArea




TODO: Discuss degenerate cases here


Let $V(Q)$ denote the vertices of polygon $Q$, and let $\mathcal{CH}(P)$ denote
the convex hull of the set of points $P$.


<div class="lemma">
There is an maximum area quadrangle $Q^*$, such that $V(Q^*) \subseteq
V(\mathcal{CH}(P))$.
</div>

<div class="proof">
TODO
</div>


Main Algorithm
---------------

> maxBaseArea    :: PointSet -> Area
> maxBaseArea [] = 0
> maxBaseArea p  = case convexHull . NonEmpty.fromList $ map ext p of
>     ch@(ConvexPolygon h) -> case F.toList $ h ^. outerBoundary of
>                               [_,_]   -> 0
>                               [a,b,c] -> triangArea $ Triangle a b c
>                               _       -> maxAreaQuadrangle ch


<div class="observation">
Left an Right chains are independent
</div>

Main idea: Pick two non-adjacent vertices $p$ and $q$ of the convex hull as the
diagonals of our quadrangle. This splits the problem into two independent
subproblems, in both of which we have to find the largest triangle that has $p$
and $q$ as vertices.

> maxAreaQuadrangle :: ConvexPolygon () Int -> Area
> maxAreaQuadrangle = maximum' . map (uncurry3 maxAreaQuadrangleWith) . allChains
>   where
>     uncurry3 f (a,b,c) = f a b c
>     maximum' = L.foldl1' max -- saves memory :)

the function `allChains` finds all alowed pairs $p$ and $q$, and the chains of
vertices (along the convex hull) connecting $p$ to $q$ and $q$ to $p$.

> type Chain = Array Int (Point 2 Int)

> allChains                 :: ConvexPolygon () Int
>                           -> [(Point 2 Int, Point 2 Int, (Chain,Chain))]
> allChains (ConvexPolygon ch) =
>     [ (chA ! i, chA ! j, chains chA i j) | i <- [1..n-2], j <- rest i ]
>   where
>     n   = F.length $ ch^.outerBoundary
>     chA = listArray (1,n) . map (^.core) . F.toList $ ch^.outerBoundary
>     rest i = [i+2.. if i == 1 then n - 1 else n ]

we make sure that we only select non-neighbouring pairs. Hence the i+2 in rest
i. Then also the last valid start-point p is the one with index n-2.

To make sure `allChains` runs in $O(n^2)$ time we build an Array representing
our convex hull vertices. All individiual chains are then views of this underlying array:

> chains          :: Array Int a -> Int -> Int -> (Array Int a, Array Int a)
> chains a pi qi = (ixSubMap (1,qi-pi-1) fu a, ixSubMap (1,r + pi - 1) fv a)
>   where
>     n    = rangeSize . bounds $ a
>     r    = n - qi
>     fu i = pi + i
>     fv i = if i <= r then qi + i
>                      else i - r

We then use `maxAreaQuadrangleWith` to find the largest quadrangle given its
diagonals $p$ and $q$ (and the chains connecting $p$ and $q$).

> maxAreaQuadrangleWith             :: Point 2 Int -> Point 2 Int -> (Chain,Chain) -> Area
> maxAreaQuadrangleWith p q (us,vs) = let pqu = findLargestTriang p q (Unimodal us)
>                                         pqv = findLargestTriang q p (Unimodal vs)
>                                     in (pqu `seq` triangArea pqu)
>                                      + (pqv `seq` triangArea pqv)


To efficiently find the largest triangle we use that the area is a unimodal
function along the convex hull. We prove this in the following lemmas.

<div class="lemma">
Let $p$ and $q$ be points on the convex hull $\mathcal{CH}(P)$, and let
$\mathcal{C}$ denote the portion of $\partial$\mathcal{CH}(P)$ between $p$ and
$q$. The area $a(v)$ of the triangle $\Delta pqv$, with $v \in \mathcal{C}$
depends only on the (Euclidean) distance $d(v)$ between $v$ and the line
segment $\overline{pq}$. More specifically, we have $a(v) = cd(v)$, for some
constant $c$.
</div>

<div class="proof">
TODO
</div>

<div class="observation">
Let $p$ and $q$ be points on the convex hull $\mathcal{CH}(P)$, let
$\mathcal{C}$ denote the portion of $\partial$\mathcal{CH}(P)$ between $p$ and
$q$, and let $v(t)$, with $t \in [0,1]$ denote the position along
$\mathcal{C}$. The function $d(t)$ expressing the (Euclidean) distance between
$v(t)$ and line segment $\overline{pq}$ is unimodal.
</div>

<div class="lemma">
Let $p$ and $q$ be points on the convex hull $\mathcal{CH}(P)$, let
$\mathcal{C}$ denote the portion of $\partial$\mathcal{CH}(P)$ between $p$ and
$q$, and let $v(t)$, with $t \in [0,1]$ denote the position along
$\mathcal{C}$. The function $a(t)$ expressing the area of the triangle $\Delta pqv(t)$ is unimodal.
</div>

<div class="proof">
Directly from previous lemma and observation.
</div>

This means we can find the triangle $\Delta pqv_i$ in $O(\log n)$ time using a
ternary search.


> newtype Unimodal s a = Unimodal { unU :: s a }
>                      deriving (Show,Eq,Ord,Read,Functor)


> findLargestTriang        :: Point 2 Int -> Point 2 Int
>                          -> Unimodal (Array Int) (Point 2 Int) -> Triangle 2 () Int
> findLargestTriang p q us = triang . ternarySearchArray area' $ us
>   where
>     triang v = Triangle (ext p) (ext q) (ext v)
>     area' = triangArea . triang


Ternary Search
--------------

> ternarySearchArray            :: (Ix i, Integral i, Ord b)
>                               => (a -> b) -> Unimodal (Array i) a -> a
> ternarySearchArray f (Unimodal a)
>   | rangeSize (bounds a) == 0 = error "empty array"
>   | otherwise                 = let (l,u) = bounds a
>                                     i     = ternarySearch (\i -> f $ a ! i) (pred l) (succ u)
>                                 in a ! i

Given a function $f$, a lowerbound $\ell$, and a n upperbound $u$ find the
value $i \in (\ell,u)$ such that $f i$ is maximal.

> ternarySearch          :: (Integral r, Ord a) => (r -> a) -> r -> r -> r
> ternarySearch f l u

>   | u - l < 2  = error "ternarySearch: l and u too close"
>   | u - l == 2 = l + 1
>   | otherwise  = let t = (u - l) `div` 3
>                      n = l + t
>                      m = l + 2*t
>                  in if f n > f m then ternarySearch f l m
>                                  else ternarySearch f n u


Input & Output
--------------

> readPointSet :: [String] -> PointSet
> readPointSet = map readPoint
>   where
>     readPoint s = let [x,y] = map read . words $ s in Point2 x y

> readInput           :: [String] -> [PointSet]
> readInput []        = []
> readInput (ns:rest) = let n       = read ns
>                           (xs,ys) = L.splitAt n rest
>                       in readPointSet xs : readInput ys


> armybase :: String -> String
> armybase = unlines . map (showValue . maxBaseArea) . readInput . tail . lines


> main :: IO ()
> main = interact armybase


> show' (p,q,(a,b)) = (p,q,elems a, elems b)

Array Stuff
-----------

> data Array i a = Array { bounds       :: (i,i)
>                        , accessTransf :: i -> i
>                        , arrayData    :: A.Array i a
>                        }

> instance Ix i => Functor (Array i) where
>   fmap f (Array b g a) = Array b g (fmap f a)

> instance (Show i, Show a, A.Ix i) => Show (Array i a) where
>   show a@(Array bs g _) = concat [ "Array "
>                                  , show bs
>                                  , " "
>                                  , show $ assocs a
>                                  ]


> (!) :: Ix i => Array i a -> i -> a
> (Array _ g a) ! i = a A.! (g i)

> elems                   :: Ix i => Array i a -> [a]
> elems a = [ a ! i | i <- A.range $ bounds a ]

> listArray   :: Ix i => (i,i) -> [a] -> Array i a
> listArray b = Array b id . A.listArray b

> assocs :: A.Ix i => Array i a -> [(i,a)]
> assocs (Array _ g a) = (\(k,v) -> (g k, v)) <$> A.assocs a

> ixSubMap :: Ix i => (i,i) -> (i -> i) -> Array i a -> Array i a
> ixSubMap bs f (Array obs g a) = Array bs (g . f) a



Testing stuff
-------------

-- > testPs :: PointSet
-- > testPs = [Point (0,0), Point (2,2), Point (4,1), Point (5,0), Point (3,-1)]

-- > test2 = [Point (0,0), Point (-2,-2), Point (3,-2), Point (0,1), Point (0,3)]


-- > testPs3 = [Point (-16,0), Point (16,16), Point (16,-16), Point (-16,16), Point (-16,-16)]
