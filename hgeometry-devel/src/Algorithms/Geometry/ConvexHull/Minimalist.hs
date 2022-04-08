{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.ConvexHull.Minimalist.Hull
import           Algorithms.Geometry.ConvexHull.Minimalist.Point
import           Data.Ext
import qualified Data.Geometry.Point as Point
import qualified Data.Geometry.PolyLine as PolyLine
import           Data.Geometry.Properties
import           Data.Geometry.LineSegment
import qualified Data.List as List
import           Data.List.Util
import           Data.Ord (comparing, Down(..))
import           Data.Util
-- import           Data.Geometry.Triangle
-- import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe

import qualified Data.Text as Text
import           Ipe
import           Ipe.Color
import           Debug.Trace

import           Data.RealNumber.Rational

--------------------------------------------------------------------------------

type R = RealNumber 5

type Triangle' point = Three point
type ConvexHull point = [Triangle' point]

--------------------------------------------------------------------------------

-- lowerHull :: Point point => NonEmpty point -> ConvexHull point
lowerHull = runSimulation' . divideAndConquer1 (simulation @HullZ). NonEmpty.sortBy cmpXYZ

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

data EventKind = Insert | Delete deriving (Show,Eq,Ord)

eval :: (Hull hull, Point point) => EventKind -> point -> hull point -> hull point
eval = \case
  Insert -> insert
  Delete -> delete

data Event point = Event { eventKind  :: !EventKind
                         , eventTime  :: !(Time point)
                         , eventPoint :: !point
                         }
deriving instance (Show (Time point), Show point) => Show (Event point)
deriving instance (Eq   (Time point), Eq   point) => Eq   (Event point)

apply   :: (Point point, Hull hull) => Event point -> hull point -> hull point
apply e = eval (eventKind e) (eventPoint e)



type Tagged e = Either e e

unTag :: Tagged e -> e
unTag = either id id

eventTime' :: Tagged (Event point) -> Time point
eventTime' = either eventTime eventTime

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
      events = map fst
             . runSim minInftyT b $ merge (Left <$> el) (Right <$> er)
      merge = mergeSortedListsBy (comparing eventTime')
      -- minInftyT = Nothing
      minInftyT = -10000 -- FIXME

-- | Runs the simulation; producing a list of events
runSim                           :: (Hull hull, Point point)
                                 => Time point -- ^ current time
                                 -> Bridge hull point -- ^ current bridge
                                 -> [Tagged (Event point)]     -- ^ future events
                                 -- -> [Event point]
                                 -> [(Event point, Bridge hull point)]
runSim now b@(Bridge l r) events = case firstEvent bridgeEvents events of
    None                    -> []
    BridgeEvent  e          -> unTag' e     : runSim (eventTime' e) (applyBE e b) events
    ExistingEvent e events' -> output e b <> runSim (eventTime' e) (applyB e b)  events'
  where
    bridgeEvents = mapMaybe (\e -> if now < eventTime' e then Just e else Nothing)
                 . concat $
                   [ Left  <$> bridgeEventL l (focus r)
                   , Right <$> bridgeEventR (focus l) r
                   ]

    unTag' e = (unTag e,b)


-- | Apply the bridge event on the bridge
applyBE                 :: (Point point, Hull hull)
                        => Tagged (Event point) -> Bridge hull point -> Bridge hull point
applyBE el (Bridge l r) = case el of
    Left e  -> let p = eventPoint e in case eventKind e of
                 Insert -> Bridge (goRight' $ insert p l) r -- changes l to p as the bridge
                 Delete -> Bridge (delete p $ goLeft' l) r
    Right e -> let p = eventPoint e in case eventKind e of
                 Insert -> Bridge l (goLeft' $ insert p r) -- changes r to p as the bridge
                 Delete -> Bridge l (delete p $ goRight' r)
  where
    goRight' = fromMaybe (error "applyBE: goRight empty") . goRight
    goLeft'  = fromMaybe (error "applyBE: goLeft  empty") . goLeft


-- | Apply the event on the bridge
applyB                 :: (Point point, Hull hull)
                       => Tagged (Event point) -> Bridge hull point -> Bridge hull point
applyB el (Bridge l r) = case el of
    Left e  -> Bridge (apply e l) r
    Right e -> Bridge l (apply e r)

-- | Should we output this event
output                  :: (Point point, Hull hull)
                        => Tagged (Event point) -> Bridge hull point
                        -- -> [Event point]
                        -> [(Event point, Bridge hull point)]
output ee b@(Bridge l r) = case ee of
    Left e  -> case eventPoint e `compareX` focus l of
                 GT -> []
                 _  -> [(e,b)]
    Right e -> case eventPoint e `compareX` focus r of
                 LT -> []
                 _  -> [(e,b)]

data NextEvent point = None
                     | BridgeEvent   !(Tagged (Event point))
                     | ExistingEvent !(Tagged (Event point)) [Tagged (Event point)]

deriving instance (Show (Time point), Show point) => Show (NextEvent point)
deriving instance (Eq (Time point), Eq point)     => Eq   (NextEvent point)

-- | Computes the first event that will happen.
firstEvent              :: Ord (Time point)
                        => [Tagged (Event point)] -- ^ bridge events
                        -> [Tagged (Event point)] -- ^ existing events
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

-- | computes the bridge event on the left
bridgeEventL      :: ( Hull hull, Point point) => hull point -> point -> [Event point]
bridgeEventL hl r = let l = focus hl
                    in catMaybes
                      [ do p <- predOfF hl
                           t <- colinearTime p l r
                           pure $ Event Delete t l
                      , do p <- succOfF hl
                           t <- colinearTime l p r
                           pure $ Event Insert t p
                      ]

-- | computes the bridge event on the right
bridgeEventR      :: (Hull hull, Point point) => point -> hull point -> [Event point]
bridgeEventR l hr = let r = focus hr
                    in catMaybes
                       [ do p <- predOfF hr
                            t <- colinearTime l p r
                            pure $ Event Insert t p
                       , do p <- succOfF hr
                            t <- colinearTime l r p
                            pure $ Event Delete t r
                       ]

----------------------------------------
-- | run the merge simulation, also producing all intermediate
-- bridges.
runMerge                       :: (Point point, Hull hull)
                               => Simulation hull point -> Simulation hull point
                               -> (hull point, [(Event point, Bridge hull point)])
runMerge (Sim l el) (Sim r er) = (fromBridge b, events)
    where
      b      = bridgeOf l r
      events = runSim minInftyT b $ merge (Left <$> el) (Right <$> er)
      merge = mergeSortedListsBy (comparing eventTime')
      minInftyT = -10000 -- FIXME

--------------------------------------------------------------------------------

-- | Run the simulation, producing the appropriate triangles
runSimulation                 :: (Point point, Hull hull)
                              => Simulation hull point -> ConvexHull point
runSimulation (Sim h0 events) = snd $ List.foldl' handle (h0,[]) events

-- | Runs a single step of the simulation
handle           :: (Hull hull, Point point)
                 => (hull point, [Three point]) -> Event point -> (hull point, [Three point])
handle (h,out) e = (apply e h, t <> out)
  where
    t = let p = eventPoint e
        in maybeToList $ (\l r -> Three l p r) <$> predOf p h <*> succOf p h

----------------------------------------

-- | runs the entire simulation, prdoducing all intermediate results
-- in increasing order of time.
runSimulation'                 :: (Point point, Hull hull)
                               => Simulation hull point
                               -> NonEmpty ( Maybe (Time point), hull point , ConvexHull point)
runSimulation' (Sim h0 events) = NonEmpty.zipWith (\t (h,o) -> (t,h,o))
                                                  (Nothing :| ((Just . eventTime) <$> events))
                               . NonEmpty.fromList $ List.scanl handle (h0,[]) events

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

-- | Recursively computes the simulation.
simulate :: forall hull point. (Point point, Hull hull)
         => NonEmpty point -> Simulation hull point
simulate = divideAndConquer1 simulation . NonEmpty.sortBy cmpXYZ

-- | computes all initial hulls
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

-- test :: ConvexHull _
test = mapM_ print $ fmap (\(t,h,_) -> (t,h)) $ lowerHull myPoints



theLeft :: Simulation HullZ (Point.Point 3 R)
theLeft = simulate . NonEmpty.fromList $
           [ Point.Point3 0 10 20
           , Point.Point3 1 1 10
           ]

theRight :: Simulation HullZ (Point.Point 3 R)
theRight = simulate . NonEmpty.fromList $
           [ Point.Point3 5 5 0
           -- , Point.Point3 12 1 1
           ]

testMerge = runMerge theLeft theRight


--------------------------------------------------------------------------------
-- * Properties

propIncreasingTime :: (Point point, Hull hull) => Simulation hull point -> Bool
propIncreasingTime = isIncreasing . fmap (\(t,_,_) -> t) . runSimulation'

isIncreasing           :: Ord a => NonEmpty a -> Bool
isIncreasing (x :| xs) = case NonEmpty.nonEmpty xs of
                           Nothing           -> True
                           Just xs'@(y :| _) -> x < y && isIncreasing xs'

propIncreasingEvents :: (Point point, Hull hull) => Simulation hull point -> Bool
propIncreasingEvents = maybe True isIncreasing . NonEmpty.nonEmpty . fmap eventTime . _events


propAllHullsConvex :: (Point point, Hull hull) => Simulation hull point -> Bool
propAllHullsConvex = undefined

--------------------------------------------------------------------------------


testHull :: HullZ _
testHull = hulls myPoints


testSim :: Simulation HullZ _
testSim = simulate myPoints


-- | Run a simulation up to a certain time to compute the hull at that time.
computeHullAt   :: (Hull hull, Point point)
                => Time point -> Simulation hull point -> hull point
computeHullAt t = go . runSimulation'
  where
    go ((_,h,_) :| xs) = case NonEmpty.nonEmpty xs of
                           Nothing                                -> h
                           Just xs'@((mt,_,_) :| _) | Just t > mt -> go xs'
                                                    | otherwise   -> h

--------------------------------------------------------------------------------

renderIpe :: ( Point point
             , Hull hull
             , IpeWriteText (NumType point)
             ) => FilePath -> Simulation hull point -> IO ()
renderIpe fp = writeIpeFile fp . render



render :: ( Point point
          , Hull hull
          , IpeWriteText (NumType point)
          ) => Simulation hull point -> IpeFile (NumType point)
render = ipeFile . fmap (\(mt,h,_) -> fromMaybe emptyPage $ do t  <- mt
                                                               pl <- hullAt t h
                                                               pure $ draw t pl
                        ) . runSimulation'


draw      :: (IpeWriteText r, Num r) => r -> PolyLine.PolyLine 2 () r -> IpePage r
draw t pl = fromContent [ iO $ ipeLabel (fromMaybe "?" (ipeWriteText t) :+ Point.origin)
                        , iO $ defIO pl
                        ]

--------------------------------------------------------------------------------

renderMergeIpe        :: ( Point point
                         , Hull hull
                         , IpeWriteText (NumType point)
                         ) => FilePath
                      -> Simulation hull point
                      -> Simulation hull point -> IO ()
renderMergeIpe fp l r = writeIpeFile fp $ renderMerge l r

renderMerge     :: ( Point point
                   , Hull hull
                   , IpeWriteText (NumType point)
                   )
                => Simulation hull point
                -> Simulation hull point
                -> IpeFile (NumType point)
renderMerge l r = ipeFile $ initialHull :| simPages
  where
     (h0,evs) = runMerge l r
     initialHull = fromMaybe emptyPage $ do let t = -1000
                                            pl <- hullAt t h0
                                            pure $ draw t pl
     simPages = flip map evs $ \(e,Bridge l' r') ->
                  fromMaybe emptyPage $ do let t = eventTime e
                                           pl <- hullAt t l'
                                           pr <- hullAt t r'
                                           pure $ draw' t pl pr (seg t l' r')

     seg t l' r' = ClosedLineSegment (ext $ toPt2 t (focus l')) (ext $ toPt2 t (focus r'))


draw'           :: (IpeWriteText r, Num r)
                => r -> PolyLine.PolyLine 2 () r
                -> PolyLine.PolyLine 2 () r
                -> LineSegment 2 () r -> IpePage r
draw' t pl pr s = fromContent
  [ iO $ ipeLabel (fromMaybe "?" (ipeWriteText t) :+ Point.origin)
  , iO $ defIO pl
  , iO $ defIO pr
  , iO $ defIO s ! attr SStroke red
  ]

-- (hull point, [(Event point, Bridge hull point)])
