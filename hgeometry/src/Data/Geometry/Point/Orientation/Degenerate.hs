module Data.Geometry.Point.Orientation.Degenerate(
    CCW(..)
  , pattern CCW, pattern CW, pattern CoLinear

  , ccw, ccw'

  , isCoLinear

  , sortAround, sortAround'

  , ccwCmpAroundWith, ccwCmpAroundWith'
  , cwCmpAroundWith, cwCmpAroundWith'
  , ccwCmpAround, ccwCmpAround'
  , cwCmpAround, cwCmpAround'

  , insertIntoCyclicOrder
  ) where

import           Control.Lens
import qualified Data.CircularList as C
import qualified Data.CircularList.Util as CU
import           Data.Ext
import           Data.Geometry.Point.Internal
import           Data.Geometry.Vector
import qualified Data.List as L

--------------------------------------------------------------------------------

-- $setup
-- >>> import Data.Double.Approximate

-- | Data type for expressing the orientation of three points, with
-- the option of allowing Colinearities.
newtype CCW = CCWWrap Ordering deriving Eq

-- | CounterClockwise orientation. Also called a left-turn.
pattern CCW      :: CCW
pattern CCW      = CCWWrap GT

-- | Clockwise orientation. Also called a right-turn.
pattern CW       :: CCW
pattern CW       = CCWWrap LT

-- | CoLinear orientation. Also called a straight line.
pattern CoLinear :: CCW
pattern CoLinear = CCWWrap EQ
{-# COMPLETE CCW, CW, CoLinear #-}

instance Show CCW where
  show = \case
    CCW      -> "CCW"
    CW       -> "CW"
    CoLinear -> "CoLinear"


-- | Given three points p q and r determine the orientation when going from p to r via q.
--
-- Be vary of numerical instability:
-- >>> ccw (Point2 0 0.3) (Point2 1 0.6) (Point2 2 (0.9::Double))
-- CCW
--
-- >>> ccw (Point2 0 0.3) (Point2 1 0.6) (Point2 2 (0.9::Rational))
-- CoLinear
--
-- If you can't use 'Rational', try 'SafeDouble' instead of 'Double':
-- >>> ccw (Point2 0 0.3) (Point2 1 0.6) (Point2 2 (0.9::SafeDouble))
-- CoLinear
--
ccw :: (Ord r, Num r) => Point 2 r -> Point 2 r -> Point 2 r -> CCW
ccw p q r = CCWWrap $ (ux*vy) `compare` (uy*vx)
-- ccw p q r = CCWWrap $ z `compare` 0 -- Comparing against 0 is bad for numerical robustness.
                                       -- I've added a testcase that fails if comparing against 0.
            -- case z `compare` 0 of
            --   LT -> CW
            --   GT -> CCW
            --   EQ -> CoLinear
     where
       Vector2 ux uy = q .-. p
       Vector2 vx vy = r .-. p
      --  _z             = ux * vy - uy * vx

-- | Given three points p q and r determine if the line from p to r via q is straight/colinear.
--
-- This is identical to `ccw p q r == CoLinear` but doesn't have the `Ord` constraint.
isCoLinear :: (Eq r, Num r) => Point 2 r -> Point 2 r -> Point 2 r -> Bool
isCoLinear p q r = (ux * vy) == (uy * vx)
     where
       Vector2 ux uy = q .-. p
       Vector2 vx vy = r .-. p

-- | Given three points p q and r determine the orientation when going from p to r via q.
ccw' :: (Ord r, Num r) => Point 2 r :+ a -> Point 2 r :+ b -> Point 2 r :+ c -> CCW
ccw' p q r = ccw (p^.core) (q^.core) (r^.core)

-- | \( O(n log n) \)
-- Sort the points arround the given point p in counter clockwise order with
-- respect to the rightward horizontal ray starting from p.  If two points q
-- and r are colinear with p, the closest one to p is reported first.
sortAround   :: (Ord r, Num r)
             => Point 2 r -> [Point 2 r] -> [Point 2 r]
sortAround c = L.sortBy (ccwCmpAround c <> cmpByDistanceTo c)

-- | \( O(n log n) \)
-- Sort the points arround the given point p in counter clockwise order with
-- respect to the rightward horizontal ray starting from p.  If two points q
-- and r are colinear with p, the closest one to p is reported first.
sortAround'   :: (Ord r, Num r)
             => Point 2 r :+ q -> [Point 2 r :+ p] -> [Point 2 r :+ p]
sortAround' c = L.sortBy (ccwCmpAround' c <> cmpByDistanceTo' c)


-- | Given a zero vector z, a center c, and two points p and q,
-- compute the ccw ordering of p and q around c with this vector as zero
-- direction.
--
-- pre: the points p,q /= c
ccwCmpAroundWith                              :: (Ord r, Num r)
                                              => Vector 2 r
                                              -> Point 2 r
                                              -> Point 2 r -> Point 2 r
                                              -> Ordering
ccwCmpAroundWith z@(Vector2 zx zy) c q r =
    case (ccw c a q, ccw c a r) of
      (CCW,CCW)      -> cmp
      (CCW,CW)       -> LT
      (CCW,CoLinear) | onZero r  -> GT
                     | otherwise -> LT

      (CW, CCW)      -> GT
      (CW, CW)       -> cmp
      (CW, CoLinear) -> GT

      (CoLinear, CCW) | onZero q  -> LT
                      | otherwise -> GT

      (CoLinear, CW)      -> LT
      (CoLinear,CoLinear) -> case (onZero q, onZero r) of
                               (True, True)   -> EQ
                               (False, False) -> EQ
                               (True, False)  -> LT
                               (False, True)  -> GT
  where
    a = c .+^ z
    b = c .+^ Vector2 (-zy) zx
    -- b is on a perpendicular vector to z

    -- test if the point lies on the ray defined by z, starting in c
    onZero d = case ccw c b d of
                 CCW      -> False
                 CW       -> True
                 CoLinear -> True -- this shouldh appen only when you ask for c itself

    cmp = case ccw c q r of
            CCW      -> LT
            CW       -> GT
            CoLinear -> EQ

ccwCmpAroundWith'                             :: (Ord r, Num r)
                                              => Vector 2 r
                                              -> Point 2 r :+ c
                                              -> Point 2 r :+ a -> Point 2 r :+ b
                                              -> Ordering
ccwCmpAroundWith' z (c :+ _) (q :+ _) (r :+ _) = ccwCmpAroundWith z c q r

-- | Given a zero vector z, a center c, and two points p and q,
-- compute the cw ordering of p and q around c with this vector as zero
-- direction.
--
-- pre: the points p,q /= c
cwCmpAroundWith     :: (Ord r, Num r)
                    => Vector 2 r
                    -> Point 2 r
                    -> Point 2 r -> Point 2 r
                    -> Ordering
cwCmpAroundWith z c = flip (ccwCmpAroundWith z c)

cwCmpAroundWith'    :: (Ord r, Num r)
                    => Vector 2 r
                    -> Point 2 r :+ a
                    -> Point 2 r :+ b -> Point 2 r :+ c
                    -> Ordering
cwCmpAroundWith' z c = flip (ccwCmpAroundWith' z c)

-- | Counter clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
ccwCmpAround :: (Num r, Ord r)
             => Point 2 r -> Point 2 r -> Point 2 r -> Ordering
ccwCmpAround = ccwCmpAroundWith (Vector2 1 0)

-- | Counter clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
ccwCmpAround' :: (Num r, Ord r)
             => Point 2 r :+ qc -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
ccwCmpAround' = ccwCmpAroundWith' (Vector2 1 0)

-- | Clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
cwCmpAround :: (Num r, Ord r)
            => Point 2 r -> Point 2 r -> Point 2 r -> Ordering
cwCmpAround = cwCmpAroundWith (Vector2 1 0)

-- | Clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
cwCmpAround' :: (Num r, Ord r)
            => Point 2 r :+ qc -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
cwCmpAround' a b c = cwCmpAround (a^.core) (b^.core) (c^.core)

-- | \( O(n) \)
-- Given a center c, a new point p, and a list of points ps, sorted in
-- counter clockwise order around c. Insert p into the cyclic order. The focus
-- of the returned cyclic list is the new point p.
insertIntoCyclicOrder   :: (Ord r, Num r)
                        => Point 2 r :+ q -> Point 2 r :+ p
                        -> C.CList (Point 2 r :+ p) -> C.CList (Point 2 r :+ p)
insertIntoCyclicOrder c = CU.insertOrdBy (ccwCmpAround' c <> cmpByDistanceTo' c)
