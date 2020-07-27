module Data.Geometry.Point.Orientation.Degenerate(
    CCW(..)
  , pattern CCW, pattern CW, pattern CoLinear

  , ccw, ccw'

  , sortAround

  , ccwCmpAroundWith, cwCmpAroundWith
  , ccwCmpAround, cwCmpAround

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

-- | Data type for expressing the orientation of three points, with
-- the option of allowing Colinearities.
newtype CCW = CCWWrap Ordering deriving Eq

pattern CCW      :: CCW
pattern CCW      = CCWWrap GT

pattern CW       :: CCW
pattern CW       = CCWWrap LT

pattern CoLinear :: CCW
pattern CoLinear = CCWWrap EQ
{-# COMPLETE CCW, CW, CoLinear #-}

instance Show CCW where
  show = \case
    CCW      -> "CCW"
    CW       -> "CW"
    CoLinear -> "CoLinear"


-- | Given three points p q and r determine the orientation when going from p to r via q.
ccw :: (Ord r, Num r) => Point 2 r -> Point 2 r -> Point 2 r -> CCW
ccw p q r = CCWWrap $ z `compare` 0
            -- case z `compare` 0 of
            --   LT -> CW
            --   GT -> CCW
            --   EQ -> CoLinear
     where
       Vector2 ux uy = q .-. p
       Vector2 vx vy = r .-. p
       z             = ux * vy - uy * vx

-- | Given three points p q and r determine the orientation when going from p to r via q.
ccw' :: (Ord r, Num r) => Point 2 r :+ a -> Point 2 r :+ b -> Point 2 r :+ c -> CCW
ccw' p q r = ccw (p^.core) (q^.core) (r^.core)

-- | Sort the points arround the given point p in counter clockwise order with
-- respect to the rightward horizontal ray starting from p.  If two points q
-- and r are colinear with p, the closest one to p is reported first.
-- running time: O(n log n)
sortAround   :: (Ord r, Num r)
             => Point 2 r :+ q -> [Point 2 r :+ p] -> [Point 2 r :+ p]
sortAround c = L.sortBy (ccwCmpAround c <> cmpByDistanceTo c)


-- | Given a zero vector z, a center c, and two points p and q,
-- compute the ccw ordering of p and q around c with this vector as zero
-- direction.
--
-- pre: the points p,q /= c
ccwCmpAroundWith                              :: (Ord r, Num r)
                                              => Vector 2 r
                                              -> Point 2 r :+ c
                                              -> Point 2 r :+ a -> Point 2 r :+ b
                                              -> Ordering
ccwCmpAroundWith z@(Vector2 zx zy) (c :+ _) (q :+ _) (r :+ _) =
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

-- | Given a zero vector z, a center c, and two points p and q,
-- compute the cw ordering of p and q around c with this vector as zero
-- direction.
--
-- pre: the points p,q /= c
cwCmpAroundWith     :: (Ord r, Num r)
                    => Vector 2 r
                    -> Point 2 r :+ a
                    -> Point 2 r :+ b -> Point 2 r :+ c
                    -> Ordering
cwCmpAroundWith z c = flip (ccwCmpAroundWith z c)

-- | Counter clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
ccwCmpAround :: (Num r, Ord r)
             => Point 2 r :+ qc -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
ccwCmpAround = ccwCmpAroundWith (Vector2 1 0)

-- | Clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
cwCmpAround :: (Num r, Ord r)
            => Point 2 r :+ qc -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
cwCmpAround = cwCmpAroundWith (Vector2 1 0)


-- | Given a center c, a new point p, and a list of points ps, sorted in
-- counter clockwise order around c. Insert p into the cyclic order. The focus
-- of the returned cyclic list is the new point p.
--
-- running time: O(n)
insertIntoCyclicOrder   :: (Ord r, Num r)
                        => Point 2 r :+ q -> Point 2 r :+ p
                        -> C.CList (Point 2 r :+ p) -> C.CList (Point 2 r :+ p)
insertIntoCyclicOrder c = CU.insertOrdBy (ccwCmpAround c <> cmpByDistanceTo c)
