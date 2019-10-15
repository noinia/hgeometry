{-# LANGUAGE UndecidableInstances #-}
module Algorithms.Geometry.WeightedClosestPair.Sweep where

import           Algorithms.Geometry.WeightedClosestPair.Naive (sqWDist)
import           Control.Lens
import           Data.Ext
import           Data.Geometry.Ball
import           Data.Geometry.Point
import qualified Data.Map as Map
import           Data.Maybe (maybeToList)
import           Data.UnBounded
import           Data.Util


import GHC.TypeNats

--------------------------------------------------------------------------------


-- | Computes the square weighted closest pair distance
sqWeightedClosestPair :: (Fractional r, Ord r) => [Disk p r] -> Top r
sqWeightedClosestPair = \case
    []          -> Top
    [_]         -> Top
    sds@(p:q:_) -> ValT $ computeFiniteDist t
      where
        ti  = sqWDist p q -- initial distance estimate
        t  = tl `min` tr
        tl = sqSemiDiskDistance ti sds
        tr = sqSemiDiskDistance ti $ map (\p -> p&center.core.xCoord %~ (* (-1))) sds
        -- mirrors the x-coordinates; so that we can compute the 'right' semi disk distances

        computeFiniteDist t = minimum
                            . (t:)
                            . map (\(Two a b) -> sqWDist a b)
                            . intersectionGraphEdges . map (scale t) $ sds


scale t p = undefined

intersectionGraphEdges :: [Disk p r] -> [Two (Disk p r)]
intersectionGraphEdges = undefined

--------------------------------------------------------------------------------

data EventKind = Insert | Delete deriving (Show,Eq)

type EventQueue a r = Map.Map r (Event a r)

type Event p r = SP EventKind (Disk p r)

type SweepLineDS p r = Map.Map r (Disk p r)

type StatusStruct p r = SP r (SweepLineDS p r)
-- current value of the squaredSemiDiskDistance and all disks that
-- we think intersect the sweep line.

-- TODO: handle multiple simultaneous events

-- TODO: Looks like we can just write this as a foldl' since no new events
-- occur

-- |
--
sqSemiDiskDistance        :: (Fractional r, Ord r)
                          => r  -- ^ initial distance estimate
                          -> [Disk p r] -> r
sqSemiDiskDistance it sds = sweep initialEq (SP it mempty)
  where
    initialEq = Map.fromList [(p^.center.core.xCoord,SP Insert p) | p <- sds]

    sweep eq ss = case Map.minViewWithKey eq of
                           Nothing        -> view _1 ss
                           Just (evt,eq') -> handle evt ss eq'

    handle (_,SP Delete q) (SP t m) eq = sweep eq  (SP t $ Map.delete (q^.center.core.yCoord) m)
    handle (x,SP Insert q) (SP t m) eq = sweep eq' (SP t' $ Map.insert y q m')
      where
        y            = q^.center.core.yCoord
        SP neighs m' = findNeighsOf x y t m
        t'           = foldr (shrink q) t neighs
        eq'          = maybe eq (\x' -> Map.insert x' (SP Delete q) eq) $ deletionTime q t'
                       -- the first disk estimate is +infty, so it never gets deleted.

-- Do we even need to handle deletions explicitly at all?. I don't think so.
deletionTime     :: Disk p r -> r -> Maybe r
deletionTime _ _ = Nothing

-- | Given a new disk q, an old disk p, and the current t value, tests
-- if we can place q with the current distance to be disjoint with p, and if not
-- shrinks t to the appropriate value.
shrink       :: (Fractional r, Ord r) => Disk p r -> Disk p r -> r -> r
shrink q p t = let d = sqWDist p q in d `min` t

-- | Tests if for a given scaling factor t a disk still intersects the
-- sweep line (at x).
intersectsSweepLineAt        :: (Num r, Ord r) => r -> r -> Disk p r -> Bool
intersectsSweepLineAt x ts p = let h  = x-p^.center.core.xCoord
                                   ws = p^.squaredRadius
                               in h*h <= ts * ws
                                  -- (x-p_x)^2 <= t^2w_p^2
                                  -- equiv x <= p_x + tw_p

-- | Finds the first disk above and below y currently intersecting the sweep line. As
-- a side effect, the function may prune old disks from the data
-- structure.
--
-- running time: \(O((1+k)log n)\), where \(k\) is the number of
-- elements pruned.
findNeighsOf         :: (Num r, Ord r)
                     => r
                     -> r
                     -> r
                     -> SweepLineDS p r
                     -> SP [Disk p r] (SweepLineDS p r)
findNeighsOf x y t m = let SP msucc m'  = findInSS Map.lookupGE (intersectsSweepLineAt x t) y m
                           SP mpred m'' = findInSS Map.lookupLE (intersectsSweepLineAt x t) y m'
                       in SP (maybeToList msucc <> maybeToList mpred) m''

-- | Finds the first disk above or below y (depending on the lookup
-- function) currently intersecting the sweep line, if one exists.  As
-- a side effect, the function may prune old disks from the data
-- structure.
--
-- running time: \(O((1+k)log n)\), where \(k\) is the number of elements pruned.
findInSS         :: forall r p. Ord r
                 => (forall k v. Ord k => k -> Map.Map k v -> Maybe (k,v))
                 -> (Disk p r -> Bool)
                 -> r
                 -> SweepLineDS p r
                 -> SP (Maybe (Disk p r)) (SweepLineDS p r)
findInSS lookupF intersectsSweepLine y = go
  where
    go   :: SweepLineDS p r -> SP (Maybe (Disk p r)) (SweepLineDS p r)
    go m = case lookupF y m of
           Nothing                            -> SP Nothing m
           Just (_,p) | intersectsSweepLine p -> SP (Just p) m
                      | otherwise             -> go $ Map.delete (p^.center.core.yCoord) m
                                                 -- disk no longer intersects the sweep line
                                                 -- kill it


data Peano = Z | S Peano

type family ToPeano n where
  ToPeano 0 = Z
  ToPeano n = ToPeano (n - 1)

type Foo (n :: Nat) = FooP (ToPeano n)

data family FooP (n :: Peano) :: *

newtype instance FooP Z = MyZeroFoo Int
newtype instance FooP (S Z) = MyOneFoo Char
newtype instance FooP (S (S n)) = MyFoo Bool


instance Show (FooP Z) where
  show = undefined

instance Show (FooP n) => Show (FooP (S (S n))) where
  show (MyFoo b) = show b
