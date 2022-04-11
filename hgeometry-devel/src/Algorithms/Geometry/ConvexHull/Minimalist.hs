{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import           Control.Lens (iso, (^.), view)
import           Data.Ext
import           Data.Geometry.LineSegment
import qualified Data.Geometry.Point as Point
import           Data.Geometry.Properties
import           Data.Geometry.Triangle
import qualified Data.List as List
import           Data.List.Util
import           Data.Ord (comparing, Down(..))
import           Data.Util
-- import           Data.Geometry.Triangle
-- import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe

import           Ipe
import           Ipe.Color
-- import           Debug.Trace
import           Data.RealNumber.Rational

--------------------------------------------------------------------------------

type R = RealNumber 5


type LowerHull point = [Three point]

--------------------------------------------------------------------------------

-- | Computes the lower hull of a *set* of points
--
-- pre: the points are assumed to be in general position, i.e.
-- no four coplanar points, all unique x,y,z coordinates.
lowerHull :: Point point => NonEmpty point -> LowerHull point
lowerHull = runSimulation . divideAndConquer1 (simulation @HullSet). NonEmpty.sortBy cmpXYZ


lowerHull' :: (Point point, AsExt point, CoreOf point ~ Point.Point 3 r, ExtraOf point ~ e
              , r ~ NumType point
              ) => NonEmpty point -> [Triangle 3 e r]
lowerHull' = map toTriangle . lowerHull


toTriangle               :: ( Point point, AsExt point
                            , CoreOf point ~ Point.Point 3 r
                            , r ~ NumType point
                            , ExtraOf point ~ e
                            )
                         => Three point -> Triangle 3 e r
toTriangle (Three p q r) = Triangle (p^._Ext) (q^._Ext) (r^._Ext)

--------------------------------------------------------------------------------

-- type VoronoiDiagamRep' point = [Three point]

-- delaunayTriangulation :: ( Point.ToAPoint point 2 r
--                   , Ord r, Fractional r
--                   ) => NonEmpty point -> VoronoiDiagamRep' (Point.Point 3 r)
-- delaunayTriangulation = lowerHull . fmap lift
--   where
--     lift p = let Point.Point2 x y = p^.Point.toPoint
--              in Point.Point3 x y (x*x + y*y)

--------------------------------------------------------------------------------

instance AsExt (Point.Point 3 r) where
  type CoreOf (Point.Point 3 r) = Point.Point 3 r
  type ExtraOf (Point.Point 3 r) = ()
  _Ext = iso ext (view core)

--------------------------------------------------------------------------------

data EventKind = Insert | Delete deriving (Show,Eq,Ord)

eval :: (Hull hull point, Point point) => EventKind -> point -> hull point -> hull point
eval = \case
  Insert -> insert
  Delete -> delete

data Event point = Event { eventKind  :: !EventKind
                         , eventTime  :: !(Time point)
                         , eventPoint :: !point
                         }
deriving instance (Show (Time point), Show point) => Show (Event point)
deriving instance (Eq   (Time point), Eq   point) => Eq   (Event point)

apply   :: (Point point, Hull hull point) => Event point -> hull point -> hull point
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

type instance NumType (Simulation hull point) = NumType point

-- | Creates a singleton simulation
simulation   :: forall hull point. Hull hull point => point -> Simulation hull point
simulation p = Sim (singleton p) []

instance (Point point, Hull hull point) => Semigroup (Simulation hull point) where
  (Sim l el) <> (Sim r er) = Sim (fromBridge b) events
    where
      b      = bridgeOf l r
      events = map fst
             . runSim minInftyT b $ merge (Left <$> el) (Right <$> er)
      merge = mergeSortedListsBy (comparing eventTime')
      -- minInftyT = Nothing

-- | Runs the simulation; producing a list of events
runSim                           :: (Hull hull point, Point point)
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
applyBE                 :: (Point point, Hull hull point)
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
applyB                 :: (Point point, Hull hull point)
                       => Tagged (Event point) -> Bridge hull point -> Bridge hull point
applyB el (Bridge l r) = case el of
    Left e  -> Bridge (apply e l) r
    Right e -> Bridge l (apply e r)

-- | Should we output this event
output                  :: (Point point, Hull hull point)
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
bridgeEventL      :: ( Hull hull point, Point point) => hull point -> point -> [Event point]
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
bridgeEventR      :: (Hull hull point, Point point) => point -> hull point -> [Event point]
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
runMerge                       :: (Point point, Hull hull point)
                               => Simulation hull point -> Simulation hull point
                               -> (hull point, [(Event point, Bridge hull point)])
runMerge (Sim l el) (Sim r er) = (fromBridge b, events)
    where
      b      = bridgeOf l r
      events = runSim minInftyT b $ merge (Left <$> el) (Right <$> er)
      merge = mergeSortedListsBy (comparing eventTime')

--------------------------------------------------------------------------------

-- | Run the simulation, producing the appropriate triangles
runSimulation                 :: (Point point, Hull hull point)
                              => Simulation hull point -> LowerHull point
runSimulation (Sim h0 events) = snd $ List.foldl' handle (h0,[]) events

-- | Runs a single step of the simulation
handle           :: (Hull hull point, Point point)
                 => (hull point, [Three point]) -> Event point -> (hull point, [Three point])
handle (h,out) e = (apply e h, t <> out)
  where
    t = let p = eventPoint e
        in maybeToList $ (\l r -> Three l p r) <$> predOf p h <*> succOf p h
{-# HLINT ignore "Avoid lambda using `infix`" #-}

----------------------------------------

-- | runs the entire simulation, prdoducing all intermediate results
-- in increasing order of time, as well as the output
runSimulation'                 :: (Point point, Hull hull point)
                               => Simulation hull point
                               -> NonEmpty ( Maybe (Time point), hull point , LowerHull point)
runSimulation' (Sim h0 events) = NonEmpty.zipWith (\t (h,o) -> (t,h,o))
                                                  (Nothing :| (Just . eventTime <$> events))
                               . NonEmpty.fromList $ List.scanl handle (h0,[]) events

-- | runs the entire simulation, prdoducing all intermediate results
-- in increasing order of time.
runSimulation'' :: (Point point, Hull hull point)
                => Simulation hull point -> NonEmpty ( Maybe (Time point), hull point)
runSimulation'' = fmap (\(t,h,_) -> (t,h)) . runSimulation'

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
{-# INLINABLE  colinearTime #-}

--------------------------------------------------------------------------------

-- | Recursively computes the simulation.
simulate :: forall hull point. (Point point, Hull hull point)
         => NonEmpty point -> Simulation hull point
simulate = divideAndConquer1 simulation . NonEmpty.sortBy cmpXYZ

-- | computes all initial hulls
hulls :: Point point => NonEmpty point -> HullSet point
hulls = divideAndConquer1 singleton . NonEmpty.sortBy cmpXYZ


--------------------------------------------------------------------------------
-----  END OF THE Main implementation
--------------------------------------------------------------------------------
-- * Properties to test

-- TODO: move to a separate testing module

propIncreasingTime :: (Point point, Hull hull point) => Simulation hull point -> Bool
propIncreasingTime = isIncreasing . fmap (\(t,_,_) -> t) . runSimulation'

isIncreasing           :: Ord a => NonEmpty a -> Bool
isIncreasing (x :| xs) = case NonEmpty.nonEmpty xs of
                           Nothing           -> True
                           Just xs'@(y :| _) -> x < y && isIncreasing xs'

propIncreasingEvents :: (Point point, Hull hull point) => Simulation hull point -> Bool
propIncreasingEvents = maybe True isIncreasing . NonEmpty.nonEmpty . fmap eventTime . _events


propAllHullsConvex :: (Point point, Hull hull point) => Simulation hull point -> Bool
propAllHullsConvex = undefined


allLeftTurnsAt   :: (Hull hull point, Point point) => Time point -> hull point -> Bool
allLeftTurnsAt t = go . fmap (toPt2 t) . toList
  where
    go (p:q:r:rest) = Point.ccw p q r == Point.CCW && go (q:r:rest)
    go _            = True

--------------------------------------------------------------------------------

-- | Run a simulation up to a certain time to compute the hull at that time.
computeHullAt   :: (Hull hull point, Point point)
                => Time point -> Simulation hull point -> hull point
computeHullAt t = go . runSimulation'
  where
    go ((_,h,_) :| xs) = case NonEmpty.nonEmpty xs of
                           Nothing                                -> h
                           Just xs'@((mt,_,_) :| _) | Just t > mt -> go xs'
                                                    | otherwise   -> h

--------------------------------------------------------------------------------
-- * Drawing stuff to help debugging

renderIpe :: ( Point point
             , Hull hull point
             , IpeWriteText (NumType point)
             , NumType (hull point) ~ NumType point
             , RenderAt (hull point), RenderAt point
             ) => FilePath -> (Simulation hull point, NonEmpty point) -> IO ()
renderIpe fp = writeIpeFile fp . render

render :: ( Point point
          , Hull hull point
          , IpeWriteText (NumType point)
          , NumType (hull point) ~ NumType point
          , RenderAt (hull point), RenderAt point
          ) => (Simulation hull point, NonEmpty point) -> IpeFile (NumType point)
render (s,pts) = ipeFile . fmap (\(t,h,_) -> fromContent [ drawTime t
                                                         , renderAt' t h
                                                         , renderAt' t pts
                                                         ]
                           )
                 . runSimulation' $ s


renderMergeIpe        :: ( Point point
                         , Hull hull point
                         , IpeWriteText (NumType point)
                         , NumType (hull point) ~ NumType point
                         , RenderAt (hull point), RenderAt point
                         ) => FilePath
                      -> (Simulation hull point, NonEmpty point)
                      -> (Simulation hull point, NonEmpty point) -> IO ()
renderMergeIpe fp l r = writeIpeFile fp $ renderMerge l r

renderMerge     :: forall point hull. ( Point point
                   , Hull hull point
                   , IpeWriteText (NumType point)
                   , NumType (hull point) ~ NumType point
                   , RenderAt (hull point), RenderAt point
                   )
                => (Simulation hull point, NonEmpty point)
                -> (Simulation hull point, NonEmpty point)
                -> IpeFile (NumType point)
renderMerge (l,lp) (r,rp) = ipeFile $ initialHull :| simPages
  where
     (h0,evs) = runMerge l r
     initialHull = fromContent [ renderAt (-1000) h0
                               , renderAt (-1000) (lp <> rp)
                               ]
     simPages = flip map evs $ \(e,b) -> let t = eventTime e in
                                           fromContent [ renderAt t b
                                                       , renderAt t (lp <> rp)
                                                       ]

drawTime   :: (Num r, IpeWriteText r) => Maybe r -> IpeObject r
drawTime t = iO . ipeLabel $ fromMaybe "?" (ipeWriteText =<< t) :+ Point.origin

renderAt' :: (RenderAt t, Num (NumType t)) => Maybe (NumType t) -> t -> IpeObject (NumType t)
renderAt' = \case
  Nothing -> const . iO $ ipeLabel ("?" :+ Point.origin)
  Just t  -> renderAt t


class RenderAt t where
  renderAt :: NumType t -> t -> IpeObject (NumType t)

type instance NumType (NonEmpty t) = NumType t
instance RenderAt t =>  RenderAt (NonEmpty t) where
  renderAt t = iO . ipeGroup . map (renderAt t) . NonEmpty.toList

instance RenderAt (Point.Point 3 R) where
  renderAt t = iO . defIO . toPt2 t

instance (Point point, RenderAt point) => RenderAt (HullSet point) where
  renderAt t h = case hullAt t h of
                   Nothing -> renderAt t (focus h)
                   Just pl -> iO . defIO $ pl

instance (Point point, Hull hull point, RenderAt (hull point), NumType (hull point) ~ NumType point
         ) => RenderAt (Bridge hull point) where
  renderAt t (Bridge l r) = iO $ ipeGroup [ renderAt t l
                                          , renderAt t r
                                          , iO $ defIO seg ! attr SStroke red
                                          ]
    where
      seg = ClosedLineSegment (ext $ toPt2 t (focus l)) (ext $ toPt2 t (focus r))


instance (Point point, Hull hull point, RenderAt (hull point), NumType (hull point) ~ NumType point
         ) => RenderAt (Simulation hull point) where
  renderAt t = renderAt t . computeHullAt t


--------------------------------------------------------------------------------

-- myPoints :: NonEmpty (Point.Point 3 R)
-- myPoints = NonEmpty.fromList
--            [ Point.Point3 0 10 20
--            , Point.Point3 1 1 10
--            , Point.Point3 5 5 0
--            , Point.Point3 12 1 1
--            , Point.Point3 22 20 1
--            ]

-- testHull :: HullSet _
-- testHull = hulls myPoints


-- testSim :: Simulation HullSet _
-- testSim = simulate myPoints

-- test :: LowerHull _
-- test = lowerHull myPoints

-- theLeft :: Simulation HullSet (Point.Point 3 R)
-- theLeft = simulate theLeftP

-- theLeftP = NonEmpty.fromList [ Point.Point3 0 10 20
--                              , Point.Point3 1 1 10
--                              ]

-- theRight = simulate theRightP

-- theRightP :: NonEmpty (Point.Point 3 R)
-- theRightP = NonEmpty.fromList [ Point.Point3 5 5 0
--            , Point.Point3 12 1 1
--            ]

-- testMerge = runMerge theLeft theRight

-- initialBridge = bridgeOf (_initialHull theLeft) (_initialHull theRight)

-- testz :: Bridge HullSet _
-- testz = let Bridge l r = initialBridge
--             [e] = bridgeEventL l (focus r)
--         in applyBE (Left e) initialBridge
