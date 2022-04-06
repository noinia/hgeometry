{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.ConvexHull.Minimalist
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
module Algorithms.Geometry.ConvexHull.Minimalist where

import           Algorithms.BinarySearch
import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.ConvexHull.Minimalist.Hull
import           Algorithms.Geometry.ConvexHull.Minimalist.Point
import           Control.Lens ((^.), view)
import           Data.Geometry.Point (xCoord, yCoord, zCoord)
import qualified Data.Geometry.Point as Point
import           Data.Geometry.Polygon.Convex (lowerTangent')
import           Data.Geometry.Properties
import           Data.List.Util
import           Data.Ord (comparing, Down(..))
-- import           Data.Geometry.Triangle
-- import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..), ViewL(..), ViewR(..))
import qualified Data.OrdSeq as OrdSeq
import qualified Data.Set as Set


import           Data.Maybe
import           Data.Kind

import Data.RealNumber.Rational


--------------------------------------------------------------------------------

type R = RealNumber 5



type Triangle' point = (point,point,point)
type ConvexHull point = [Triangle' point]


lowerHull :: Point point => NonEmpty point -> ConvexHull point
lowerHull = fromSimulation . divideAndConquer1 (simulation @HullZ). NonEmpty.sortBy cmpXYZ




--------------------------------------------------------------------------------


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------





--------------------------------------------------------------------------------


-- toSeq                    :: Bridge Set a -> Seq a
-- toSeq (Bridge ll l r rr) = ll <> Seq.singleton l <> Seq.singleton r <> rr

-- tangentR   :: point -> Hull' point -> Hull' point
-- tangentR q = dropWhile2L (isLeftTurn q)

-- tangentL   :: point -> Hull' point -> Hull' point
-- tangentL q = dropWhile2R (isLeftTurn q)

-- isLeftTurn :: point -> point -> point -> Bool
-- isLeftTurn = undefined

-- dropWhile2L   :: (a -> a -> Bool) -> Hull' a -> Hull' a
-- dropWhile2L p = go
--   where
--     go = \case
--       (x :<| xs@(y :<| _)) | p x y -> go xs
--       s                            -> s

-- dropWhile2R   :: (a -> a -> Bool) -> Seq a -> Seq a
-- dropWhile2R p = go
--   where
--     go = \case
--       (xs@(_ :|> y) :|> x) | p x y -> go xs
--       s                            -> s


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








--------------------------------------------------------------------------------


data EventKind = Insert | Delete deriving (Show,Eq,Ord)

data Event point = Event { eventKind  :: !EventKind
                         , eventTime  :: !(Time point)
                         , eventPoint :: !point
                         }

deriving instance (Show (Time point), Show point) => Show (Event point)
deriving instance (Eq   (Time point), Eq   point) => Eq   (Event point)

type Existing e = Either e e

eventTime' :: Existing (Event point) -> Time point
eventTime' = either eventTime eventTime

toEvent :: Existing (Event point) -> Event point
toEvent = either id id

--------------------------------------------------------------------------------

-- | The simulation
data Simulation hull point = Sim { _initialHull :: hull point
                                 , _events      :: [Event point]
                                 }
deriving instance (Show (Time point), Show point, Show (hull point))
                  => Show (Simulation hull point)
deriving instance (Eq (Time point), Eq point, Eq (hull point))
                  => Eq   (Simulation hull point)

-- | Creates a singleton simulation
simulation   :: forall hull point. Hull hull => point -> Simulation hull point
simulation p = Sim (singleton p) []


instance (Point point, Hull hull) => Semigroup (Simulation hull point) where
  (Sim l el) <> (Sim r er) = Sim (fromBridge b) events
    where
      b      = bridgeOf l r
      events = runSim minInftyT b $ merge (Left <$> el) (Right <$> er)
      merge = mergeSortedListsBy (comparing eventTime')
      -- minInftyT = Nothing
      minInftyT = -10000 -- FIXME


-- | Runs the simulation; producing a list of events
runSim                                       :: (Hull hull, Point point)
                                             => Time point -- ^ current time
                                             -> Bridge hull point -- ^ current bridge
                                             -> [Existing (Event point)]     -- ^ future events
                                             -> [Event point]
runSim now b@(Bridge l r) events = case firstEvent bridgeEvents events of
    None                    -> []
    BridgeEvent  e          -> toEvent e   : runSim (eventTime' e) (apply e b) events
    ExistingEvent e events' -> output e b <> runSim (eventTime' e) (apply e b) events'
  where
    bridgeEvents = mapMaybe (\e -> if now < eventTime' e then Just e else Nothing)
                 . concat $
                   [ Left  <$> bridgeEventL l (focus r)
                   , Right <$> bridgeEventR (focus l) r
                   ]
      -- TODO: if we make the Bottom into a type class we may be able
      -- to avoid creating explit ValB's all the time. That should save allocations

-- | Apply the event on the bridge
apply                 :: (Point point, Hull hull)
                      => Existing (Event point) -> Bridge hull point -> Bridge hull point
apply el (Bridge l r) = case el of
    Left e  -> Bridge (applyE e l) r
    Right e -> Bridge l (applyE e r)
  -- FIXME: I guess this may change the bride points; so we may have to rotate here I guess
  -- I guess that is only when we are at a bridge event.
  where
    applyE e = case eventKind e of
                 Insert -> insert (eventPoint e)
                 Delete -> delete (eventPoint e)

-- | Should we output this event
output                 :: (Point point, Hull hull)
                       => Existing (Event point) -> Bridge hull point -> [Event point]
output ee (Bridge l r) = case ee of
    Left e  -> case (eventPoint e) `compareX` (focus l) of
                 GT -> []
                 _  -> [e]
    Right e -> case (eventPoint e) `compareX` (focus r) of
                 LT -> []
                 _  -> [e]

data NextEvent point = None
                     | BridgeEvent   !(Existing (Event point))
                     | ExistingEvent !(Existing (Event point)) [Existing (Event point)]

deriving instance (Show (Time point), Show point) => Show (NextEvent point)
deriving instance (Eq (Time point), Eq point)     => Eq   (NextEvent point)

-- | Computes the first event that will happen.
firstEvent              :: Ord (Time point)
                        => [Existing (Event point)] -- ^ bridge events
                        -> [Existing (Event point)] -- ^ existing events
                        -> NextEvent point
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
    cmp = comparing eventTime'
    firstEvent' = minimum1By cmp


data ApplyWhere = NoWhere
                | LeftHull
                | LeftBridgePoint
                | RightBridgePoint
                | RightHull
                deriving (Show,Eq)

applyWhere                        :: (Point point, Hull hull)
                                  => point -> Bridge hull point -> ApplyWhere
applyWhere q (Bridge l r) = case q `compareX` (focus l) of
    LT -> LeftHull
    EQ -> LeftBridgePoint
    GT -> case q `compareX` (focus r) of
            LT -> NoWhere
            EQ -> RightBridgePoint
            GT -> RightHull


  --   LeftHull         -> Bridge (apply' e ll) l r rr
  --   RightHull        -> Bridge ll l r (apply' e rr)
  --   LeftBridgePoint  -> undefined
  --   RightBridgePoint -> undefined
  -- where
  --   apply' = undefined


  --     let (ll',l') = extractMax ll
  --                     in Bridge ll' l' r rr



  -- case e of
  --   Delete _ p         ->

  --   InsertBefore _ p q -> undefined -- case Seq.Seq.insertAt ()


bridgeEventL      :: ( Hull hull, Point point)
                  => hull point -> point -> [Event point]
bridgeEventL hl r = let l = focus hl
                    in catMaybes
                      [ do p <- predOf l hl
                           t <- colinearTime p l r
                           pure $ Event Delete t l
                      , do p <- succOf l hl
                           t <- colinearTime l p r
                           pure $ Event Insert t p -- verify that this should not be an insert
                      ]

bridgeEventR           :: (Hull hull, Point point)
                       => point -> hull point -> [Event point]
bridgeEventR l hr = let r = focus hr
                    in catMaybes
                       [ do p <- predOf r hr
                            t <- colinearTime l p r
                            pure $ Event Insert t p
                       , do p <- succOf r hr
                            t <- colinearTime l r p
                            pure $ Event Delete t r  -- verify that this should not be an insert
                       ]

fromSimulation :: Simulation hull point -> ConvexHull point
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
cmpXYZ  :: Point point => point -> point -> Ordering
cmpXYZ (toPt3 -> Point.Point3 px py pz) (toPt3 -> Point.Point3 qx qy qz) =
  compare px qx <> compare (Down py) (Down qy) <> compare pz qz

-- | compute the time at which r becomes colinear with the line through
-- p and q.
--
-- pre: x-order is: p,q,r
colinearTime :: Point point => point -> point -> point -> Maybe (Time point)
colinearTime (toPt3 -> Point.Point3 px py pz)
             (toPt3 -> Point.Point3 qx qy qz) (toPt3 -> Point.Point3 rx ry rz) =
    if b == 0 then Nothing else Just $ a / b
  where        -- by unfolding the def of ccw
    ux = qx - px
    vx = rx - px
    a = ux*(rz - pz)  - vx*(qz - pz)
    b = ux*(ry - py)  - vx*(qy - py)
  -- b == zero means the three points are on a vertical plane. This corresponds
  -- to t = -\infty.



--------------------------------------------------------------------------------

simulate :: forall hull point. (Point point, Hull hull)
         => NonEmpty point -> Simulation hull point
simulate = divideAndConquer1 simulation . NonEmpty.sortBy cmpXYZ

hulls :: Point point => NonEmpty point -> HullZ point
hulls = divideAndConquer1 singleton . NonEmpty.sortBy cmpXYZ

--------------------------------------------------------------------------------

myPoints :: NonEmpty (Point.Point 3 R)
myPoints = NonEmpty.fromList
           [ Point.Point3 0 10 20
           , Point.Point3 1 1 10
           , Point.Point3 5 5 0
           , Point.Point3 12 1 1
           , Point.Point3 22 20 1
           ]

test = hulls myPoints


testSim :: Simulation HullZ _
testSim = simulate myPoints
