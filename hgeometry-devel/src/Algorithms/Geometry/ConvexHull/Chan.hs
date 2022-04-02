{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
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
import qualified Data.Geometry.Point as Point
import           Data.Geometry.Point (xCoord,yCoord,zCoord)
import           Data.List.Util
import           Data.Ord (comparing)
-- import           Data.Geometry.Polygon.Convex (lowerTangent')
import           Data.Geometry.Properties
-- import           Data.Geometry.Triangle
-- import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Sequence (Seq(..))
import qualified Data.OrdSeq as OrdSeq
import qualified Data.Set as Set


import           Data.Maybe
import           Data.Kind

--------------------------------------------------------------------------------

type Triangle' point = (point,point,point)
type ConvexHull point = [Triangle' point]


lowerHull :: Point point
          => NonEmpty point -> ConvexHull point
lowerHull = fromSimulation . divideAndConquer1 simulation . NonEmpty.sortBy cmpXYZ

--------------------------------------------------------------------------------

class ( Ord (NumType point), Fractional (NumType point)
      ) => Point point where
  toPt3 :: point -> Point.Point 3 (NumType point)

toPt2                                :: Point point
                                     => Time point -> point -> Point.Point 2 (NumType point)
toPt2 t (toPt3 -> Point.Point3 x y z) = Point.Point2 x (z - t*y)

--------------------------------------------------------------------------------


class Hull (hull :: Type -> Type) where
  singleton :: point -> hull point

  fromBridge :: Bridge hull point -> hull point

  default fromBridge :: (Semigroup (hull point)) => Bridge hull point -> hull point
  fromBridge (Bridge ll l _ _ r rr) = ll <> singleton l <> singleton r <> rr

  bridgeOf :: hull point -> hull point -> Bridge hull point

  delete :: point -> hull point -> hull point
  insert :: point -> hull point -> hull point

  predOf :: point -> hull point -> Maybe point
  succOf :: point -> hull point -> Maybe point

  extractPred :: point -> hull point -> Maybe (hull point, point)
  extractSucc :: point -> hull point -> Maybe (point, hull point)


--------------------------------------------------------------------------------

type Hull' = NonEmpty

-- type Hull' = Set.Set
-- data Hull' point = Singleton !point
--                  | Bridged (Bridge (Seq point) point)
--                  deriving (Show,Eq)

data Bridge hull point = Bridge (hull point) !point (hull point)
                                (hull point) !point (hull point)
                       deriving (Show,Eq)

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

instance Hull NonEmpty where
  singleton = (:| [])
  bridgeOf = undefined






--------------------------------------------------------------------------------

type Time point = NumType point

data EventKind = Insert | Delete deriving (Show,Eq,Ord)

data Event point = Event { eventKind  :: !EventKind
                         , eventTime  :: !(Time point)
                         , eventPoint :: !point
                         }

deriving instance (Show (Time point), Show point) => Show (Event point)
deriving instance (Eq   (Time point), Eq   point) => Eq   (Event point)

--------------------------------------------------------------------------------

-- | The simulation
data Simulation point = Sim { _initialHull :: Hull' point
                            , _events      :: [Event point]
                            }
deriving instance (Show (Time point), Show point) => Show (Simulation point)
deriving instance (Eq (Time point), Eq point)     => Eq   (Simulation point)

simulation   :: point -> Simulation point
simulation p = Sim (singleton p) []


instance Point point => Semigroup (Simulation point) where
  (Sim l el) <> (Sim r er) = Sim (fromBridge b) events
    where
      b      = bridgeOf l r
      events = runSim minInftyT b (mergeSortedListsBy (comparing eventTime) el er)
      minInftyT = Nothing

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


runSim                                       :: (Hull hull, Point point)
                                             => Maybe (Time point)
                                             -> Bridge hull point
                                             -> [Event point]
                                             -> [Event point]
runSim now b@(Bridge ll l lr rl r rr) events = case firstEvent bridgeEvents events of
    None                    -> []
    BridgeEvent  e          -> runSim (Just $ eventTime e) (apply e b) events
    ExistingEvent e events' -> runSim (Just $ eventTime e) (apply e b) events'
  where
    bridgeEvents = mapMaybe (\e -> if now < Just (eventTime e) then Just e else Nothing)
                 . concat $
                   [ bridgeEventL ll l lr r
                   , bridgeEventR l rl r rr
                   ]
      -- TODO: if we make the Bottom into a type class we may be able
      -- to avoid creating explit ValB's all the time. That should save allocations

compareX :: point -> point -> Ordering
compareX = undefined

data ApplyWhere = NoWhere
                | LeftHull
                | LeftBridgePoint
                | RightBridgePoint
                | RightHull
                deriving (Show,Eq)

applyWhere                        :: point -> Bridge hull point -> ApplyWhere
applyWhere q (Bridge _ l _ _ r _) = case q `compareX` l of
    LT -> LeftHull
    EQ -> LeftBridgePoint
    GT -> case q `compareX` r of
            LT -> NoWhere
            EQ -> RightBridgePoint
            GT -> RightHull

apply                           :: Event point -> Bridge hull point -> Bridge hull point
apply e b@(Bridge ll l _ _ r rr) = case applyWhere (eventPoint e) b of
    NoWhere          -> b
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


bridgeEventL           :: ( Hull hull, Point point)
                       => hull point -> point -> hull point -> point -> [Event point]
bridgeEventL ll l lr r = catMaybes
  [ do p <- predOf l ll
       t <- colinearTime p l r
       pure $ Event Delete t l
  , do p <- succOf l lr
       t <- colinearTime l p r
       pure $ Event Insert t p -- verify that this should not be an insert
  ]

bridgeEventR           :: (Hull hull, Point point)
                       => point -> hull point -> point -> hull point -> [Event point]
bridgeEventR l rl r rr = catMaybes
  [ do p <- predOf r rl
       t <- colinearTime l p r
       pure $ Event Insert t p
  , do p <- succOf r rr
       t <- colinearTime l r p
       pure $ Event Delete t r  -- verify that this should not be an insert
  ]

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
