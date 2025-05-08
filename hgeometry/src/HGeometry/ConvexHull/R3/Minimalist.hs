--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ConvexHull.R3.Minimalist
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of Chan's minimalist 3D convex hull algorithm
--
--------------------------------------------------------------------------------
module HGeometry.ConvexHull.R3.Minimalist
  (

  ) where

import Control.Lens
import Data.Foldable (toList)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (isNothing)
import HGeometry.Algorithms.DivideAndConquer
import HGeometry.Combinatorial.Util
import HGeometry.HalfSpace
import HGeometry.HyperPlane
import HGeometry.Intersection (intersects)
import HGeometry.Point
import HGeometry.Triangle
import HGeometry.Vector

--------------------------------------------------------------------------------

type ConvexHull point = [Triangle point]


-- | Computes the lower hull without its vertical triangles.
--
-- note we lose an \(O(\log n)\) factor since we use Sequences to implement the intermediate hulls.
--
-- pre: The points are in general position. In particular, no four
-- points should be coplanar.
--
-- running time: \(O(n\log^2 n)\)
lowerHull' :: forall point r.
              (Foldable1 set, Ord r, Fractional r, Show r, Point_ point 3 r)
           => set point -> ConvexHull point
lowerHull' = dropIndices
           . runSimulation . divideAndConquer1 (simulation @HullIntMap)
           . withIndices . NonEmpty.sortBy cmpXYZ


dropIndices = undefined

--------------------------------------------------------------------------------

data Hull point = Hull (Seq point) point (Seq point)
                deriving (Show,Eq)

data Simulation point = Sim { initialHull :: Hull point
                            , events      :: [Event point]
                            }
                      deriving (Show,Eq)

type Time point = NumType point

data EventKind = Insert | Delete deriving (Show,Eq,Ord)

data Event point = Event { eventKind  :: !EventKind
                         , eventTime  :: !(Time point)
                         , eventPoint :: !point
                         }
deriving instance (Show (Time point), Show point) => Show (Event point)
deriving instance (Eq   (Time point), Eq   point) => Eq   (Event point)




-- eval :: (Hull hull point, Point point) => EventKind -> point -> hull point -> hull point
-- eval = \case
--   Insert -> insert
--   Delete -> delete

-- apply   :: (Point point, Hull hull point) => Event point -> hull point -> hull point
-- apply e = eval (eventKind e) (eventPoint e)






type Tagged e = Either e e

unTag :: Tagged e -> e
unTag = either id id

eventTime' :: Tagged (Event point) -> Time point
eventTime' = either eventTime eventTime

-- | Creates a singleton simulation
simulation   :: point -> Simulation point
simulation p = Sim (Hull mempty p mempty) []

-- FIXME: maybe we don't want to continue to 1 point, but to 3 points, so that you can
-- actually pick a time before the first event.


instance (Point point) => Semigroup (Simulation point) where
  (Sim l el) <> (Sim r er) = Sim (fromBridge b) events
    where
      b      = bridgeOf l r
      events = runSim minInftyT b $ merge (Left <$> el) (Right <$> er)
      merge  = mergeSortedListsBy (comparing eventTime')


type Bridge point = Vector 2 (Hull point)

goRightMost h@(Hull as p bs) = case Seq.viewR bs of
                                 EmptyR   -> h
                                 bs' :> q -> Hull (as <> p :< bs') p mempty

goLeftMost h@(Hull as p bs) = case Seq.viewL as of
                                 EmptyR   -> h
                                 q :< as' -> Hull mempty q (as' <> p :< bs)

-- | Computes the bridge of the two given hulls
bridgeOf       :: (Point_ point 3 r)
               => Hull point -> Hull point -> Bridge point
bridgeOf l0 r0 = go (goRightMost l0) (goLeftMost r0)
    where
      go l r | isRight' (succOfF r) l r = go l          (goRight' r)
             | isRight' (predOfF l) l r = go (goLeft' l) r
             | otherwise                = Vector2 l r

      isRight' Nothing  _ _ = False
      isRight' (Just x) l r = ccw (toPt l) (toPt r) (toPt2 t x) /= CCW

      goLeft'  = fromMaybe (error "goLeft': no left")   . goLeft
      goRight' = fromMaybe (error "goRight': no right") . goRight

      toPt h = toPt2 t (focus h)
      t = minInftyT
  -- chan's doesnot use the t here apparently.


runSim :: Hull point
       -> Hull point
       -> NonEmpty (Tagged (Event point))
       -> ( Hull point
          , NonEmpty (Event point)
          )
runSim l r = undefined
-- hmm, the first event may be a bridge event (that is not in the list yet!?)
