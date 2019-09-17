module Algorithms.Geometry.WeightedClosestPair.Sweep where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Ball
import           Data.Geometry.Point
import qualified Data.Map as Map
import           Data.UnBounded
import           Data.Util

-- | Squared weighted distance between two disks p and q, i.e. the square of the scaling t
-- such that p*t and q*t touch in a point.
sqWDist                         :: Fractional r => Disk p r -> Disk p r -> r
sqWDist (Disk p wp) (Disk q wq) = let d = squaredEuclideanDist (p^.core) (q^.core)
                                      w = wp + wq
                                  in  d / (w*w)



data EventKind = Insert | Delete deriving (Show,Eq)



type EventQueue a r = Map.Map r (Event a r)

type Event p r = SP EventKind (Disk p r)

type SweepLineDS p r = Map.Map r (Disk p r)

type StatusStruct p r = SP (Top r) (SweepLineDS p r)
-- current value of the squaredSemiDiskDistance and all disks that
-- we think intersect the sweep line.

-- TODO: handle multiple simultaneous events

-- |
--
sqSemiDiskDistance     :: (Fractional r, Ord r) => [Disk p r] -> Top r
sqSemiDiskDistance sds = sweep initialEq (SP Top mempty)
  where
    initialEq = Map.fromList [(p^.center.core.xCoord,SP Insert p) | p <- sds]

    sweep eq ss = case Map.minViewWithKey eq of
                           Nothing        -> view _1 ss
                           Just (evt,eq') -> handle evt ss eq'

    handle (x,SP Delete q) (SP t m) eq = sweep eq  (SP t $ Map.delete (q^.center.core.yCoord) m)
    handle (x,SP Insert q) (SP t m) eq = sweep eq' (SP t' $ Map.insert y q m')
      where
        y            = q^.center.core.yCoord
        SP neighs m' = findNeighsOf x y t m
        t'           = foldr (shrink q) t neighs
        eq'          = maybe eq (\x' -> Map.insert x' (SP Delete q) eq) $ deletionTime q t'
                       -- the first disk estimate is +infty, so it never gets deleted.

-- Do we even need to handle deletions explicitly at all?
deletionTime      :: Disk p r -> Top r -> Maybe r
deletionTime p tt = Nothing


-- | Given a new disk q, an old disk p, and the current t value, tests
-- if we can place q with the current distance to be disjoint with p, and if not
-- shrinks t to the appropriate value.
shrink        :: (Fractional r, Ord r) => Disk p r -> Disk p r -> Top r -> Top r
shrink q p tt = let d = sqWDist p q in ValT d `min` tt


intersectsSweepLineAt        :: (Num r, Ord r) => r -> Top r -> Disk p r -> Bool
intersectsSweepLineAt x tt p = let h  = x-p^.center.core.xCoord
                                   ws = p^.squaredRadius
                               in case tt of
                                    Top     -> True
                                    ValT ts -> h*h <= ts * ws
                                  --       (x-p_x)^2 <= t^2w_p^2
                                  -- equiv x <= p_x + tw_p


findNeighsOf x y t m = let SP msucc m'  = findSuccOf x y t m
                           SP mpred m'' = findPredOf x y t m'
                       in SP (maybeToList msucc <> maybeToList mpred) m''

-- | Finds the first disk above y currently intersecting the sweep line, if one exists.
-- As a side effect, the function may prune old disks from the data structure.
--
findSuccOf         :: (Num r, Ord r) =>
                   -> r
                   -> r
                   -> Top r
                   -> SweepLineDS p r
                   -> SP (Maybe (Disk p r)) (SweepLineDS p r)
findSuccOf x y t m = findInSS Map.lookupGE (intersectsSweepLineAt x t)

-- | Symmetric to findSuccOf
findPredOf x y t m = findInSS Map.lookupLE (intersectsSweepLineAt x t)


-- | Finds the first disk above y currently intersecting the sweep line, if one exists.
-- As a side effect, the function may prune old disks from the data structure.
--
-- running time: \(O((1+k)log n)\), where \(k\) is the number of elements pruned.
findInSS         :: Ord r
                 => (r -> Map.Map r (Disk p r) -> Maybe (Disk p r))
                 -> (Disk p r -> Bool)
                 -> r
                 -> SweepLineDS p r
                 -> SP (Maybe (Disk p r)) (SweepLineDS p r)
findInSS lookupF intersectsSweepLine y = go
  where
    go m = case lookupF y m of
           Nothing                            -> SP Nothing m
           r@(Just p) | intersectsSweepLine p -> SP r m
                      | otheriwse             -> go $ Map.delete (p^.center.core.yCoord)
                                                 -- disk no longer intersects the sweep line
                                                 -- kill it
