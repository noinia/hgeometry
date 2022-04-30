{-# LANGUAGE UndecidableInstances #-}
module Geometry.SoS.ConvexHull
  -- ( -- convexHull
  -- -- , upperHull
  --  lowerHull
  -- )
  where

import           Algorithms.DivideAndConquer
import           Control.Lens (over, view)
import           Data.Ext
import           Data.Indexed
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Symbolic
import           Data.Semigroup.Foldable (Foldable1 (..))
import           Data.Util
import           Geometry.Point
import           Geometry.Vector
import           Geometry.SoS.Point
--------------------------------------------------------------------------------

-- | \(O(n \log n)\) time LowerHull using divide and conquer. The resulting Hull is
-- given from left to right, i.e. in counter clockwise order.
lowerHull :: (Ord r, Num r)
          => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
lowerHull = lowerHull' . NonEmpty.sortBy incXdecY

lowerHull' :: (Ord r, Num r) => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
lowerHull' = fmap fromSymbolic' . unLH
           . divideAndConquer1 (LH . (:|[]))
           . fmap toSymbolic'
           . labelWithIndex

debug :: (Ord r, Num r)
      => NonEmpty (Point 2 r :+ p) -> NonEmpty (WithIndex (Point 2 (Symbolic SoSI r) :+ p))
debug = fmap toSymbolic'
      . labelWithIndex
      . NonEmpty.sortBy incXdecY

fromSymbolic' :: (Arity d, Num r) => WithIndex (Point d (Symbolic i r) :+ e) -> Point d r :+ e
fromSymbolic' = over core fromSymbolic . view theValue

toSymbolic'                        :: (Arity d, ToAPoint point d r, Num r)
                                   => WithIndex (point :+ e)
                                   -> WithIndex (Point d (Symbolic SoSI r) :+ e)
toSymbolic' (WithIndex i (p :+ e)) = WithIndex i ((toSymbolic $ WithIndex i p) :+ e)




newtype LH point = LH { unLH :: NonEmpty point } deriving (Eq,Show)

instance (Num r, Ord r, ToAPoint point 2 r, HasIndex point) => Semigroup (LH point) where
  (LH lh) <> (LH rh) = LH $ hull lowerTangent lh rh

----------------------------------------

-- | The function that does the actual merging part
hull               :: (NonEmpty p -> NonEmpty p -> Two (p :+ [p]))
                   -> NonEmpty p -> NonEmpty p -> NonEmpty p
hull tangent lh rh = let Two (l :+ lh') (r :+ rh') = tangent (NonEmpty.reverse lh) rh
                     in NonEmpty.fromList $ reverse lh' <> [l,r] <> rh'

--------------------------------------------------------------------------------

incXdecY  :: Ord r => Point 2 r :+ p -> Point 2 r :+ q -> Ordering
incXdecY (Point2 px py :+ _) (Point2 qx qy :+ _) =
  compare px qx <> compare qy py


-- | Compute the lower tangent of the two convex chains lp and rp
--
--   pre: - the chains lp and rp have at least 1 vertex
--        - lp and rp are disjoint, and there is a vertical line
--          having lp on the left and rp on the right.
--        - The vertices in the left-chain are given in clockwise order, (right to left)
--        - The vertices in the right chain are given in counterclockwise order (left-to-right)
--
-- The result returned is the two endpoints l and r of the tangents,
-- and the remainders lc and rc of the chains (i.e.)  such that the lower hull
-- of both chains is: (reverse lc) ++ [l,h] ++ rc
--
-- Running time: \(O(n+m)\), where n and m are the sizes of the two chains
-- respectively
lowerTangent       :: forall point r f. (Ord r, Num r, Foldable1 f, ToAPoint point 2 r, HasIndex point)
                   => f point -> f point -> Two (point :+ [point])
lowerTangent l0 r0 = go (toNonEmpty l0) (toNonEmpty r0)
  where
    ne = NonEmpty.fromList
    isRight' []    _ _ = False
    isRight' (x:_) l r = strictCcw l r x == SCW

    go lh@(l:|ls) rh@(r:|rs) | isRight' rs l r = go lh      (ne rs)
                             | isRight' ls l r = go (ne ls) rh
                             | otherwise       = Two (l :+ ls) (r :+ rs)
