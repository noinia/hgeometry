{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.ConvexHull.DivideAndConquer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(O(n\log n)\) time divide and conquer algorithm to compute the convex hull
-- of a set of \(n\) points in \(\mathbb{R}^2\).
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.ConvexHull.DivideAndConquerSoS( convexHull
                                                         , upperHull
                                                         , lowerHull
                                                         ) where

import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.SoS

import           Data.Ext
import           Data.Geometry.Point(Point(..))
import           Data.Geometry.Point.Orientation
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex (ConvexPolygon(..))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Util

import Data.RealNumber.Rational

type R = RealNumber 5

--------------------------------------------------------------------------------

-- | \(O(n \log n)\) time ConvexHull using divide and conquer. The resulting polygon is
-- given in clockwise order.
convexHull           :: (Ord r, Num r) => NonEmpty (Point 2 r :+ p) -> ConvexPolygon p r
convexHull (p :| []) = ConvexPolygon . unsafeFromPoints $ [p]
convexHull pts       = undefined


  -- combine . (upperHull' &&& lowerHull') . NonEmpty.sortBy incXdecY $ pts
  -- where
  --   combine (l:|uh,_:|lh) = ConvexPolygon . unsafeFromPoints $ l : uh <> reverse (init lh)

----------------------------------------
-- * Computing a lower hull

-- -- | \(O(n \log n)\) time LowerHull using divide and conquer. The resulting Hull is
-- -- given from left to right, i.e. in counter clockwise order.
lowerHull = undefined
-- lowerHull :: (Ord r, Num r)
--           => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
-- lowerHull = lowerHull' . NonEmpty.sortBy incXdecY

-- lowerHull' :: (Ord r, Num r) => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
-- lowerHull' = unLH . divideAndConquer1 (LH . (:|[]))

-- newtype LH r p = LH { unLH :: NonEmpty (Point 2 r :+ p) } deriving (Eq,Show)

-- instance (Num r, Ord r) => Semigroup (LH r p) where
--   (LH lh) <> (LH rh) = LH $ hull lowerTangent' lh rh

--------------------------------------------------------------------------------

myPoints = NonEmpty.fromList . zipWith (flip (:+)) [1..] $ [ Point2 0 0
                                                           , Point2 3 3
                                                             -- , Point2
                                                           ]


----------------------------------------
-- * Computing an upper hull

-- | \(O(n \log n)\) time UpperHull using divide and conquer. The resulting Hull is
-- given from left to right, i.e. in clockwise order.
upperHull :: (Ord r, Num r) => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
upperHull = simulateSimplicity (upperHull' . NonEmpty.sortBy incX)

incX     :: (CanAquire (P i 2 r p), Ord r) => P i 2 r p -> P i 2 r p -> Ordering
incX p q = undefined

upperHull' :: (Ord r, Num r, CanAquire (P i 2 r p))
           => NonEmpty (P i 2 r p) -> NonEmpty (Point 2 r :+ p)
upperHull' = replaceByOriginal . unUH . divideAndConquer1 (UH . (:|[]))

newtype UH i r p = UH { unUH :: NonEmpty (P i 2 r p) }


instance (Num r, Ord r, CanAquire (P i 2 r p)) => Semigroup (UH i r p) where
  (UH lh) <> (UH rh) = UH $ hull upperTangent' lh rh

----------------------------------------

-- | The function that does the actual merging part
hull               :: (NonEmpty p -> NonEmpty p -> Two (p :+ [p]))
                   -> NonEmpty p -> NonEmpty p -> NonEmpty p
hull tangent lh rh = let Two (l :+ lh') (r :+ rh') = tangent (NonEmpty.reverse lh) rh
                     in NonEmpty.fromList $ reverse lh' <> [l,r] <> rh'

--------------------------------------------------------------------------------

-- incXdecY  :: Ord r => Point 2 r :+ p -> Point 2 r :+ q -> Ordering
-- incXdecY (Point2 px py :+ _) (Point2 qx qy :+ _) =
--   compare px qx <> compare qy py



-- | Compute the upper tangent of the two convex chains lp and rp
--
--   pre: - the chains lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line
--          having lp on the left and rp on the right.
--        - The vertices in the left-chain are given in clockwise order, (right to left)
--        - The vertices in the right chain are given in counterclockwise order (left-to-right)
--
-- The result returned is the two endpoints l and r of the tangents,
-- and the remainders lc and rc of the chains (i.e.)  such that the upper hull
-- of both chains is: (reverse lc) ++ [l,h] ++ rc
--
-- Running time: \(O(n+m)\), where n and m are the sizes of the two chains
-- respectively
upperTangent'       :: forall i r p. (Ord r, Num r, CanAquire (P i 2 r p))
                    => NonEmpty (P i 2 r p) -> NonEmpty (P i 2 r p)
                    -> Two (P i 2 r p :+ [P i 2 r p])
upperTangent' l0 r0 = go l0 r0
  where
    ne = NonEmpty.fromList
    isLeft'           :: [P i 2 r p] -> P i 2 r p -> P i 2 r p -> Bool
    isLeft' []    _ _ = False
    isLeft' (x:_) l r = ccw' l r x == CCW

    go lh@(l:|ls) rh@(r:|rs) | isLeft' rs l r = go lh      (ne rs)
                             | isLeft' ls l r = go (ne ls) rh
                             | otherwise      = Two (l :+ ls) (r :+ rs)
