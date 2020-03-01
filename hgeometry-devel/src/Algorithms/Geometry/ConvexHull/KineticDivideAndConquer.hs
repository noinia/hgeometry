{-# LANGUAGE BangPatterns #-}
module Algorithms.Geometry.ConvexHull.KineticDivideAndConquer where

import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.ConvexHull.Debug
import           Algorithms.Geometry.ConvexHull.Helpers
import           Algorithms.Geometry.ConvexHull.Types
import           Control.Applicative (liftA2)
import           Control.Lens ((^.), (&), (%~), bimap, _1, _2)
import           Control.Monad ((<=<))
import           Control.Monad.State.Class (get, put)
import           Control.Monad.State.Strict (evalStateT)
import           Control.Monad.Trans
import           Data.Either (partitionEithers)
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Convex (lowerTangent')
import           Data.Geometry.Triangle
import           Data.IndexedDoublyLinkedList
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Semigroup (sconcat)
import           Data.UnBounded
import           Data.Util
import qualified Data.Vector as V

import           Data.Geometry.Ipe
import           Data.Maybe (catMaybes)
import           Data.RealNumber.Rational


import           Debug.Trace


--------------------------------------------------------------------------------

-- TODO: Replace Action by a partial function?

-- TODO: We seem to assume that no four points are coplanar, and no
-- three points lie on a vertical plane. Figure out where we assume that exactly.

-- no four points coplanar:
--     - otherwise events may happen simultaneously
--     - output may not be a set of triangles
-- no three points on a vertical plane:
--     Otherwise the test for computing the next time t does not exist
--       (slope of the supp. plane of three such points is +infty)


-- TODO: The kinetic sim. now treats three points on a vertical plane
-- as happening at time t=-\infty. That means we should find faces on
-- the lower envelope that have that property separately.
--
-- I think we can do that by projecting the points down onto the
-- xy-plane, and computing a 2D convex hull.
--
-- I Think this is actually fine, since for s.t. like the delaunay
-- triangulation you don't want those faces anyway.

-- FIXME: We start with some arbitrary starting slope. Fix that




-- lowerHull :: (Ord r, Fractional r) => [Point 3 r :+ p] -> ConvexHull 3 p r
-- lowerHull = maybe mempty lowerHull' . NonEmpty.nonEmpty


lowerHull'      :: forall r p. (Ord r, Fractional r, Show r, IpeWriteText r)
                => NonEmpty (Point 3 r :+ p) -> ConvexHull 3 p r
lowerHull' pts' = map withPt $ runDLListMonad pts computeHull
  where
    computeHull :: HullM s r [Three Index]
    computeHull = output <=< divideAndConquer1 mkLeaf $ NonEmpty.fromList [0..(n-1)]

    n = V.length pts
    (pts,exts) = bimap V.fromList V.fromList . unExt . NonEmpty.sortBy cmpXYZ $ pts'
    unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])

    -- withPt = id
    withPt (Three a b c) = let pt i = pts V.! i :+ exts V.! i in Triangle (pt a) (pt b) (pt c)

    -- sort on x-coord first (and on equal y and z coordinate later)
    cmpXYZ = comparing (^.core)


-- | Creates a Leaf
mkLeaf   :: Int -> HullM s r (MergeStatus r)
mkLeaf i = pure $ MergeStatus i i []


--------------------------------------------------------------------------------

-- | Computes a lowerbound on the z-value with which to start
-- pre: not all points on a vertical plane
lowerboundT     :: (Ord r, Fractional r) => NonEmpty (Point 3 r) -> r
lowerboundT pts = ((-1)*) . maximum . catMaybes
                $ zipWith slope (toList pts') (NonEmpty.tail pts')
  where
    pts' = NonEmpty.sortBy (comparing (^.yCoord) <> comparing (^.zCoord)) pts

    slope p q = let d = q^.yCoord - p^.yCoord
                in if d == 0 then Nothing else Just $ (abs $ q^.zCoord - p^.zCoord) / d


instance (Ord r, Fractional r, Show r, IpeWriteText r)
         => Semigroup (HullM s r (MergeStatus r)) where
  lc <> rc = do l <- lc
                r <- rc


                d <- debugHull l r
                pts <- getPoints

                let esIn = traceShow d $ mergeEvents (events l) (events r)
                    t    = (-10000000) -- TODO; at what time value should we start?
                (h,u,v) <- traceShow ("before merge:",d,t,events l, events r,map (^.core) esIn
                                     ) <$> findBridge t l r
                let b = traceShow ("bridge:", u, v, "hull:",h) $ (Bridge u v)

                es <- runKinetic Bottom esIn
                      $ traceShow (drawDebug ("before_merge_" <> rangeS l r) Bottom h b pts)
                                        b
                writeList $ traceShow ("writing hull: ",h) h
                let !ms = MergeStatus (hd l) (lst r) es
                fp <- renderMovieIO ("movie_" <> rangeS l r) ms
                pure $ traceShow fp ms
                --
                -- pure $ traceShow (drawDebug "combined" ms (Bridge u v) pts) ms
    where
      rangeS l r = show (hd l) <> "-" <> show (lst r)



--------------------------------------------------------------------------------
-- * Producing the Output Hull

-- | Reports all the edges on the CH
output    :: Show r => MergeStatus r -> HullM s r [Three Index]
output ms | traceShow ("output: ", events ms) False = undefined
output ms = concat <$> mapM handle (events ms)
  where
    handle e = do ts <- catMaybes . toList <$> mapM reportTriangle (e^.eventActions)
                  applyEvent e
                  pure ts

reportTriangle :: Action -> HullM s r (Maybe (Three Index))
reportTriangle = \case
    InsertAfter i j  -> fmap (\r   -> Three i j r)   <$> getNext i
    InsertBefore i h -> fmap (\l   -> Three l h i)   <$> getPrev i
    Delete j         -> liftA2 (\l r -> Three l j r) <$> getPrev j <*> getNext j

--------------------------------------------------------------------------------
-- * Finding the Bridge

-- | Computes the Bridge of the Hulls (the Hulls currently encoded in
-- the underlying Doublylinkedlist)
--
-- running time: \(O(n)\)
findBridge       :: (Ord r, Fractional r, Show r)
                 => r
                 -> MergeStatus r
                 -> MergeStatus r
                 -> HullM s r (NonEmpty Index, Index, Index)
findBridge t l r = do lh <- toListFromR (lst l)
                      rh <- toListFrom  (hd r)
                      findBridgeFrom t lh rh

findBridgeFrom       :: (Ord r, Fractional r, Show r)
                     => r
                     -> NonEmpty Index
                     -> NonEmpty Index
                     -> HullM s r (NonEmpty Index, Index, Index)
findBridgeFrom t l r = do lh <- mapM (atTime'' t) l
                          rh <- mapM (atTime'' t) r
                          let Two (u :+ ls) (v :+ rs) = findBridge' lh rh
                          pure $ (fromL $ reverse ls <> [u,v] <> rs, u, v)
  where
    atTime'' t' i = (:+ i) <$> atTime t' i
    findBridge' l0 r0 = f <$> lowerTangent' l0 r0
    f (c :+ es) = c^.extra :+ ((^.extra) <$> es)

    fromL xs = case NonEmpty.nonEmpty xs of
                 Nothing -> error "findBridge"
                 Just n  -> n

--------------------------------------------------------------------------------

mergeEvents       :: (Ord r, Show r)
                  => [Event r] -> [Event r] -> [r :+ NonEmpty (Existing Action)]
mergeEvents ls rs = map combine . groupOn (^.core)
                  $ mergeSortedListsBy (comparing (^.core)) (wrap Left ls) (wrap Right  rs)
  where
    wrap f = map (&extra %~ \k -> f <$> k)
    combine ((t:+as):|es) = t :+ (sconcat $ as :| map (^.extra) es)

--------------------------------------------------------------------------------
-- * Running the simulation

-- | run the kinetic simulation, computing the events at which the
-- hull changes. At any point during the simulation:
--
-- - the multable array in env represents the hulls L and R
-- - we maintain the current bridge u on L and v on R such that
-- - L[1..u] <> R[v..n] is the output hull H
runKinetic        :: (Ord r, Fractional r, Show r, IpeWriteText r )
                  => Bottom r                          -- ^ starting time
                  -> [r :+ NonEmpty (Existing Action)] -- ^ the existing events
                  -> Bridge -- initial bridge
                  -> HullM s r [Event r]
runKinetic t es b = evalStateT (handleEvent t es) b

-- | Given the current time, handling an event means three things:
-- 1. figuring out when the first bridge event is
-- 2. figuring out at what time the next event happens, what actions
--    occur at that time, and performing those.
-- 3. handling all remaining events.
handleEvent        :: (Ord r, Fractional r, Show r)
                   => Bottom r                         -- ^ The current time
                   -> [r :+ NonEmpty (Existing Action)] -- ^ the existing events
                   -> Simulation s r [Event r]
handleEvent now es = do mbe <- firstBridgeEvent now
                        case traceShowId $ nextEvent mbe es of
                          None                   -> pure []
                          Next t eacts es' -> do me  <- handleAllAtTime t eacts
                                                 evs <- handleEvent (ValB t) es'
                                                 pure $ maybeToList me <> evs
--------------------------------------------------------------------------------
-- * Handling all events at a particular time.

-- handles all events at the current time.
handleAllAtTime :: (Ord r, Fractional r, Show r)
                => r
                -- ^ the current time
                -> [Existing Action]
                -- ^ all *existing* events that are happening at the
                -- current time. I.e. events in either the left or right hulls
                -> Simulation s r (Maybe (Event r))
handleAllAtTime now ees | traceShow ("handleAllAtTime",now,ees) False = undefined
handleAllAtTime now ees =
    do b <- get
       let Bridge l r = b
       (delL,ls) <- handleOneSide now levs l
       (delR,rs) <- handleOneSide now revs r
       b' <- newBridge now (NonEmpty.reverse ls) rs
       let Bridge l' r' = b'
           louts = filter (\e -> getRightMost e <= l `max` l') levs
           routs = filter (\e -> r `min` r' <= getLeftMost e) revs
       la <- leftBridgeEvent  l l' delL levs
       ra <- rightBridgeEvent r r' delR revs
       put b'
       pure . outputEvent now $ louts <> routs <> catMaybes [la,ra]
         -- the bridge actions should be after louts and routs;
  where
    (levs,revs) = partitionEithers ees
    outputEvent t acts = (t :+) <$> NonEmpty.nonEmpty acts

-- | Computes if we should output a new bridge action for the left endpoint
leftBridgeEvent                         :: Index -> Index -> Bool -> [Action]
                                        -> Simulation s r (Maybe Action)
leftBridgeEvent l l' alreadyDeleted evs = lift $ case (l `compare` l') of
    LT | shouldBeInserted l' evs -> (\(Just p) -> Just $ InsertAfter p l') <$> getPrev l'
    -- since l < l', l' has a predecessor, and hence the fromJust is safe.
    GT | not alreadyDeleted      -> pure . Just $ Delete l
    _                            -> pure Nothing

rightBridgeEvent                         :: Index -> Index -> Bool -> [Action]
                                         -> Simulation s r (Maybe Action)
--rightBridgeEvent r r' b evs | traceShow ("rightBridgeEvent ",r,r',b,evs) False = undefined
rightBridgeEvent r r' alreadyDeleted evs = lift $ case (r' `compare` r) of
    LT | shouldBeInserted r' evs -> (\(Just p) -> Just $ InsertBefore p r') <$> getNext r'
    -- since r' < r, r' has a successor, and hence the fromJust is safe
    GT | not alreadyDeleted      -> pure . Just $ Delete r
    _                            -> pure Nothing

-- | Figure out if the given index has been inserted in one of the actions.
shouldBeInserted   :: Index -> [Action] -> Bool
shouldBeInserted i = null . filter isInsert
  where
    isInsert = \case
      InsertAfter  _ j | j == i -> True
      InsertBefore _ j | j == i -> True
      _                         -> False

-- | Considering that all points in ls and rs are colinear at time t
-- (and contain the bridge at time). Compute the new bridge.
newBridge         :: (Ord r, Fractional r, Show r)
                  =>  r -> NonEmpty Index -> NonEmpty Index -> Simulation s r Bridge
newBridge t ls rs = lift $ (\(_,l,r) -> Bridge l r) <$> findBridgeFrom (t+1) ls rs
  -- claim: all colinear a t time t means we can pick any time t' > t
  -- to compute the new bridge

--
-- returns wether the current bridge was deleted and the colinears
-- with the current bridge
handleOneSide           :: (Ord r, Num r)
                        => r -> [Action] -> Index -> Simulation s r (Bool, NonEmpty Index)
handleOneSide now evs l = do lift $ mapM_ applyEvent' insertions
                             lift $ mapM_ applyEvent' deletions
                             ls <- colinears now l
                             lift $ mapM_ applyEvent' delBridge
                             pure (isJust delBridge, ls)
  where
    (delBridge, deletions, insertions) = partitionActions evs l

partitionActions       :: [Action] -> Index -> (Maybe Action, [Action], [Action])
partitionActions evs l = (listToMaybe bridgeDels, rest, ins)
  where
    (dels,ins) = List.partition isDelete evs
    (bridgeDels,rest) = List.partition (\(Delete i) -> i == l) dels
    isDelete = \case
      Delete _ -> True
      _        -> False

colinears     :: (Ord r, Num r) => r -> Index -> Simulation s r (NonEmpty Index)
colinears t x = do b  <- get >>= traverse (lift . atTime t)
                   ls <- lift (toListFromR x) >>= takeColinear b
                   rs <- lift (toListFrom  x) >>= takeColinear b
                   pure $ fromL $ (reverse ls) <> [x] <> rs
  where
    takeColinear b (_ :| is) = takeWhileM (isColinearWith b t) is

    fromL xs = case NonEmpty.nonEmpty xs of
                 Nothing -> error "colinears"
                 Just n  -> n

-- | Given two 2d-points (representing the bridge at time t), time t,
-- and index i, test if the point with index i is colinear with the
-- bridge.
isColinearWith               :: (Ord r, Num r)
                             => Two (Point 2 r) -> r -> Index
                             -> Simulation s r Bool
isColinearWith (Two l r) t i = (\p -> ccw l r p == CoLinear) <$> lift (atTime t i)


--------------------------------------------------------------------------------
-- * Computing the Next Event

data NextEvent r = None | Next { nextEventTime   :: !r
                               , existingActions :: ![Existing Action]
                               , remainingEvents :: ![r :+ NonEmpty (Existing Action)]
                               } deriving (Show,Eq)

-- | Figures out what the time of the first event is (if it exists)
-- and collects everything that happens at that time (and which
-- existing events happen later)
nextEvent        :: Ord r
                  => Maybe r
                  -> [r :+ NonEmpty (Existing Action)]
                  -> NextEvent r
nextEvent Nothing   []                       = None
nextEvent Nothing   ((te :+ eacts) : es')    = Next te (toList eacts) es'
nextEvent (Just tb) []                       = Next tb []             []
nextEvent (Just tb) es@((te :+ eacts) : es') = case tb `compare` te of
                                                 LT -> Next tb []             es
                                                 EQ -> Next tb (toList eacts) es'
                                                 GT -> Next te (toList eacts) es'

-- | Computes the first time a bridge event happens.
firstBridgeEvent     :: (Ord r, Fractional r) => Bottom r -> Simulation s r (Maybe r)
firstBridgeEvent now = do br <- get
                          let Bridge l r = br
                          cands <- sequence [ getPrev l >>~ \a -> colinearTime a l r
                                            , getNext l >>~ \b -> colinearTime l b r
                                            , getPrev r >>~ \c -> colinearTime l c r
                                            , getNext r >>~ \d -> colinearTime l r d
                                            ]
                          pure $ minimum' [t | c@(ValB t) <- cands, now < c]
  where
    c >>~ k = lift c >>= \case
                Nothing -> pure Bottom
                Just i  -> k i

--------------------------------------------------------------------------------


-- | compute the time at which r becomes colinear with the line throuh
-- p and q.
colinearTime       :: (Ord r, Fractional r) => Index -> Index -> Index -> Simulation s r (Bottom r)
colinearTime p q r = colinearTime' <$> pointAt' p <*> pointAt' q <*> pointAt' r

-- | compute the time at which r becomes colinear with the line through
-- p and q.
--
-- pre: x-order is: p,q,r
colinearTime'  :: (Ord r, Fractional r) => Point 3 r -> Point 3 r -> Point 3 r -> Bottom r
colinearTime' (Point3 px py pz) (Point3 qx qy qz) (Point3 rx ry rz) =
    if b == 0 then Bottom else ValB $ a / b
  where        -- by unfolding the def of ccw
    ux = qx - px
    vx = rx - px
    a = ux*(rz - pz)  - vx*(qz - pz)
    b = ux*(ry - py)  - vx*(qy - py)
  -- b == zero means the three points are on a vertical plane. This corresponds
  -- to t = -\infty.



--------------------------------------------------------------------------------

myPts :: NonEmpty (Point 3 (RealNumber 10) :+ Int)
myPts = NonEmpty.fromList $ [ Point3 5  5  0  :+ 2
                            , Point3 1  1  10 :+ 1
                            , Point3 0  10 20 :+ 0
                            , Point3 12 1  1  :+ 3
                            , Point3 22 20  1  :+ 4
                            ]

-- myResult = [1 2 3
--             2 3 4
--             0 1 2
--             0 2 4
--            ]

myPts' :: NonEmpty (Point 3 (RealNumber 10) :+ Int)
myPts' = NonEmpty.fromList $ [ Point3 5  5  0  :+ 2
                             , Point3 1  1  10 :+ 1
                             , Point3 0  10 20 :+ 0
                             , Point3 12 1  1  :+ 3
                             ]

-- 1 2 3
-- 0 1 2
-- 0 2 3


test :: IO ()
test = mapM_ print $ lowerHull' myPts

test' :: IO ()
test' = mapM_ print $ lowerHull' myPts'
