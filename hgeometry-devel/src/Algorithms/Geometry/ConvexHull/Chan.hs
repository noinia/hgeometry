{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.ConvexHull.KineticDivideAndConquer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(3\)-d convex hull algorithm. The implementation is based on
--
-- <http://tmc.web.engr.illinois.edu/ch3d/ch3d.pdf A Minimalist’s Implementationof the3-dDivide-and-ConquerConvex Hull Algorithm>
-- by Timothy M. Chan
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.ConvexHull.Chan where

import           Algorithms.DivideAndConquer
import           Data.List.Util
import           Data.Ord (comparing)
-- import           Data.Geometry.Point
import           Data.Geometry.Polygon.Convex (lowerTangent')
import           Data.Geometry.Properties
import           Data.Geometry.Triangle
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Sequence (Seq(..),ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import           Data.Maybe
import           Data.Kind

--------------------------------------------------------------------------------

type Triangle' point = (point,point,point)
type ConvexHull point = [Triangle' point]


lowerHull :: (r ~ NumType point
             , Ord r, Fractional r
             )
          => NonEmpty point -> ConvexHull point
lowerHull = fromSimulation . divideAndConquer1 singleton . NonEmpty.sortBy cmpXYZ

--------------------------------------------------------------------------------

class Hull (hull :: Type -> Type) where
  bridgeOf :: hull point -> hull point -> Bridge hull point
  apply    :: Event point -> Bridge hull point -> Bridge hull point

--------------------------------------------------------------------------------

type Hull' = Seq
-- data Hull' point = Singleton !point
--                  | Bridged (Bridge (Seq point) point)
--                  deriving (Show,Eq)

data Bridge hull point = Bridge (hull point) !point !point (hull point)
                       deriving (Show,Eq)

toSeq (Bridge ll l r rr) = ll <> Seq.singleton l <> Seq.singleton r <> rr

-- toSeq :: Hull' point -> Seq point
-- toSeq = \case
--   Singleton p -> Seq.singleton p
--   Bridged br  -> toSeq' br


tangentR   :: point -> Hull' point -> Seq point
tangentR q = dropWhile2L (isLeftTurn q)

tangentL   :: point -> Hull' point -> Seq point
tangentL q = dropWhile2R (isLeftTurn q)

isLeftTurn :: point -> point -> point -> Bool
isLeftTurn = undefined

dropWhile2L   :: (a -> a -> Bool) -> Seq a -> Seq a
dropWhile2L p = go
  where
    go = \case
      (x :<| xs@(y :<| _)) | p x y -> go xs
      s                            -> s

dropWhile2R   :: (a -> a -> Bool) -> Seq a -> Seq a
dropWhile2R p = go
  where
    go = \case
      (xs@(_ :|> y) :|> x) | p x y -> go xs
      s                            -> s


-- instance Semigroup (Hull' point) where
--   (Singleton p)         <> (Singleton q) = Bridged $ Bridge mempty p q mempty
--   (Singleton p)         <> hr            = let r :<| rr = tangentR p hr
--                                            in Bridged $ Bridge mempty p r rr
--   hl                    <> (Singleton q) = let ll :|> l = tangentL q hl
--                                            in Bridged $ Bridge ll l q mempty
--   (toSeq -> (hl :|> l)) <> (toSeq -> hr) = undefined

    -- goL hl l hr
    -- where
    --   goL hl l hr = let (r :<| rr) = tangentR l hr
    --                 in goR hl r rr
    --   goR hl r rr = let (ll :|> l)

instance Hull Hull' where

--------------------------------------------------------------------------------

type Time point = NumType point


data Event point = Delete       (Time point) point
                 | InsertBefore (Time point) point point

deriving instance (Show (Time point), Show point) => Show (Event point)
deriving instance (Eq   (Time point), Eq   point) => Eq   (Event point)

eventTime :: Event point -> Time point
eventTime = \case
  Delete       t _   -> t
  InsertBefore t _ _ -> t


-- | The simulation
data Simulation point = Sim { initialHull :: Hull' point
                            , events      :: [Event point]
                            }
deriving instance (Show (Time point), Show point) => Show (Simulation point)
deriving instance (Eq (Time point), Eq point)     => Eq   (Simulation point)

singleton   :: point -> Simulation point
singleton p = Sim (Seq.singleton p) []


instance Ord (Time point) => Semigroup (Simulation point) where
  (Sim l el) <> (Sim r er) = Sim (toSeq b) events
    where
      b      = bridgeOf l r
      events = runSim b (mergeSortedListsBy (comparing eventTime) el er)

data NextEvent point = None
                     | BridgeEvent   (Event point)
                     | ExistingEvent (Event point) [Event point]

deriving instance (Show (Time point), Show point) => Show (NextEvent point)
deriving instance (Eq (Time point), Eq point)     => Eq   (NextEvent point)

firstEvent              :: Ord (Time point)
                        => [Event point] -> [Event point] -> NextEvent point
firstEvent bridgeEvents = \case
  []       -> case firstEvent' bridgeEvents of
                Nothing -> None
                Just e  -> BridgeEvent e
  (e : es) -> case firstEvent' bridgeEvents of
                Nothing -> ExistingEvent e es
                Just be -> case be `cmp` e of
                             LT -> BridgeEvent be
                             EQ -> error "simulatneous event; this better not happen"
                             GT -> ExistingEvent e es
  where
    cmp = comparing eventTime
    firstEvent' = minimum1By cmp

runSim                            :: ( Hull hull
                                     , Ord (Time point)
                                     )
                                  => Bridge hull point
                                  -> [Event point]
                                  -> [Event point]
runSim b@(Bridge ll l r rr) events = case firstEvent bridgeEvents events of
    None                    -> []
    BridgeEvent  e          -> runSim (apply e b) events
    ExistingEvent e events' -> runSim (apply e b) events'
  where
    bridgeEvents = catMaybes [ bridgeEventL ll l r
                             , bridgeEventR l r rr
                             ]


bridgeEventL = undefined
bridgeEventR = undefined





fromSimulation :: Simulation point -> ConvexHull point
fromSimulation = undefined

--------------------------------------------------------------------------------

-- | Comparator for the points. We sort the points lexicographically
-- on increasing x-coordiante, decreasing y-coordinate, and increasing
-- z-coordinate. The extra data is ignored.
--
-- The divide and conquer algorithm needs the points sorted in
-- increasing order on x.
--
-- The choice of sorting order of the y and z-coordinates is such that
-- in a leaf (all points with the same x-coord). Are already
-- pre-sorted in the right way: in particular, increasing on their
-- "slope" in the "Time x Y'" space. This means that when we compute
-- the lower envelope of these lines (using the duality and upper
-- hull) we don't have to re-sort the points. See 'simulateLeaf'' for
-- details.
cmpXYZ  :: point -> point -> Ordering
cmpXYZ = undefined

-- (Point3 px py pz :+ _) (Point3 qx qy qz :+ _) =
--   compare px qx <> compare (Down py) (Down qy) <> compare pz qz


safeHead :: [a] -> Maybe a
safeHead = listToMaybe
