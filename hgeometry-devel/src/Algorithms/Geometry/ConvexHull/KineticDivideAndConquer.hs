{-# LANGUAGE BangPatterns #-}
module Algorithms.Geometry.ConvexHull.KineticDivideAndConquer where

import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.ConvexHull.Debug
import           Algorithms.Geometry.ConvexHull.Helpers
import           Algorithms.Geometry.ConvexHull.Types
import           Control.Applicative (liftA2,(<|>))
import           Control.Lens ((^.), (&), (%~), bimap, Lens', _1, _2)
import           Control.Monad ((<=<))
import           Control.Monad (replicateM)
import           Control.Monad.State.Class (gets, get, put)
import           Control.Monad.State.Strict (StateT, evalStateT)
import           Control.Monad.Trans
import           Data.Bitraversable
import           Data.Either (partitionEithers)
import           Data.Ext
import           Data.Foldable (forM_)
import           Data.Foldable (toList)
import           Data.Geometry.Line (lineThrough, onSideUpDown, SideTestUpDown(..))
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PolyLine
import           Data.Geometry.Polygon.Convex (lowerTangent')
import           Data.Geometry.Triangle
import           Data.Geometry.Vector
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
import qualified Data.Vector.Mutable as MV

import           Data.Geometry.Ipe
import           Data.Geometry.Ipe.Color
import           Data.Maybe (catMaybes)
import           Data.RealNumber.Rational


import           Control.Monad.Reader.Class
import           Debug.Trace
import           System.IO.Unsafe
import           System.Random
import qualified Data.Text as Text

import           Algorithms.Geometry.ConvexHull.RenderPLY

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
                STR h u v <- traceShow ("before merge:",d,t,events l, events r,map (^.core) esIn
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
                 -> HullM s r (STR (NonEmpty Index) Index Index)
findBridge t l r = do lh <- mapM (atTime'' t) =<< toListFromR (lst l)
                      rh <- mapM (atTime'' t) =<< toListFrom  (hd r)
                      let Two (u :+ ls) (v :+ rs) = findBridge' lh rh
                      pure $ STR (NonEmpty.fromList $ reverse ls <> [u,v] <> rs) u v
  where
    atTime'' t' i = (:+ i) <$> atTime t' i

    findBridge' l0 r0 = f <$> lowerTangent' l0 r0
    f (c :+ es) = c^.extra :+ ((^.extra) <$> es)

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
handleEvent        :: (Ord r, Fractional r)
                   => Bottom r                         -- ^ The current time
                   -> [r :+ NonEmpty (Existing Action)] -- ^ the existing events
                   -> Simulation s r [Event r]
handleEvent now es = do mbe <- firstBridgeEvent now
                        case nextEvent mbe es of
                          None                   -> pure []
                          Next t bacts eacts es' -> (:) <$> handleAllAtTime t bacts eacts
                                                        <*> handleEvent (ValB t) es'

--------------------------------------------------------------------------------
-- * Handling all events at a particular time.

-- handles all events at the current time.
handleAllAtTime :: (Ord r, Num r)
                => r
                -- ^ the current time
                -> [Action]
                -- ^ The bridge events happening at the current time (if they exists).

                -- I guess we care only about deletions here now anyway, since we have to recompute the bridge event. Maybe that is not entirely true....
                --

                -> [Existing Action]
                -- ^ all *existing* events that are happening at the
                -- current time. I.e. events in either the left or right hulls
                -> Simulation s r (Event r)
handleAllAtTime now mbe ees =
    do b <- get
       let Bridge l r = b
       ls <- handleOneSide now levs l
       rs <- handleOneSide now revs r
       b' <- newBridge (NonEmpty.reverse ls) rs
       let Bridge l' r' = b'
           louts = filter (\e -> getRightMost e <= l `max` l') levs
           routs = filter (\e -> r `min` r' <= getLeftMost e) revs
           -- TODO: we need to output bridge events
           -- I guess that we can just figure out what type of bridge event we have here based on
           -- l, l', r, and r' no?

           -- since even if the bridge event is 'delete l' or so, then
           -- we would actually find this now.



       put b'
       pure $ now :+ NonEmpty.fromList (louts <> routs)
  where
    (levs,revs) = partitionEithers ees

-- at most two?
determineBridgeEvents                             :: Bridge -> Bridge -> [Action]
determineBridgeEvents (Bridge l r) (Bridge l' r') = le <> re
  where
    le = case (l `compare` l') of
           LT -> [InsertAfter l l'] -- TDOO should this really be after l? Or after whatever is the prececessor of l' now?
           EQ -> []
           GT -> [Delete l] -- I guess we should only report this delete if it did not hapen already anyway? Can that even happen?

    re = case (r `compare` r') of
           LT -> [Delete r]
           EQ -> []
           GT -> [InsertBefore r r'] -- TODO; same here !






newBridge       :: Ord r =>  NonEmpty Index -> NonEmpty Index -> Simulation s r Bridge
newBridge ls rs = Bridge <$> fastestPoint ls <*> fastestPoint rs

fastestPoint :: Ord r => NonEmpty Index -> Simulation s r Index
fastestPoint = fmap ((^._1) . maximumOn1 (^._2.yCoord)) . mapM (\i -> (i,) <$> pointAt' i)

-- | Handles one side of the stuff to do at the moment.
handleOneSide           :: (Ord r, Num r)
                        => r -> [Action] -> Index -> Simulation s r (NonEmpty Index)
handleOneSide now evs l = do lift $ mapM_ applyEvent' insertions
                             lift $ mapM_ applyEvent' deletions
                             ls <- colinears now l
                             lift $ mapM_ applyEvent' delBridge
                             pure ls
  where
    (delBridge, deletions, insertions) = partitionActions evs l

partitionActions       :: [Action] -> Index -> (Maybe Action, [Action], [Action])
partitionActions evs l = (listToMaybe bridgeDels, rest, ins)
  where
    (dels,ins) = List.partition isDelete evs
    (bridgeDels,rest) = List.partition (\(Delete i) -> i == l) dels
    isDelete = \case
      Delete _ -> False
      _        -> True

colinears     :: (Ord r, Num r) => r -> Index -> Simulation s r (NonEmpty Index)
colinears t x = do b <- get >>= traverse (lift . atTime t)
                   ls <- lift (toListFromR x) >>= takeColinear b
                   rs <- lift (toListFrom  x) >>= takeColinear b
                   pure $ NonEmpty.fromList $ (reverse ls) <> [x] <> rs
  where
    takeColinear b (_ :| is) = takeWhileM (isColinearWith b t) is

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
                               , bridgeActions   :: ![Action]
                               , existingActions :: ![Existing Action]
                               , remainingEvents :: ![r :+ NonEmpty (Existing Action)]
                               } deriving (Show,Eq)

-- | Figures out what the time of the first event is (if it exists)
-- and collects everything that happens at that time (and which
-- existing events happen later)
nextEvent        :: Ord r
                  => Maybe (Event r)
                  -> [r :+ NonEmpty (Existing Action)]
                  -> NextEvent r
nextEvent Nothing              []                       = None
nextEvent Nothing              ((te :+ eacts) : es')    = Next te []             (toList eacts) es'
nextEvent (Just (tb :+ bacts)) []                       = Next tb (toList bacts) []             []
nextEvent (Just (tb :+ bacts)) es@((te :+ eacts) : es') =
    case tb `compare` te of
      LT -> Next tb (toList bacts) []             es
      EQ -> Next tb (toList bacts) (toList eacts) es'
      GT -> Next te []             (toList eacts) es'

-- | Computes the first time a bridge event happens, and all actions
-- that happen at that time.
firstBridgeEvent     :: Ord r => Bottom r -> Simulation s r (Maybe (Event r))
firstBridgeEvent now = collectEarliest <$> candidateBridgeEvents now

-- | Collect all a's that actually happen at the earliest time.
collectEarliest    :: Ord t => [t :+ NonEmpty a] -> Maybe (t :+ NonEmpty a)
collectEarliest xs = let mt = minimumOn (^.core) xs in
  (\(t :+ _) -> t :+ sconcat (NonEmpty.fromList [ a | t' :+ a <- xs, t' == t])) <$> mt

candidateBridgeEvents     :: Bottom r -> Simulation s r [Event r]
candidateBridgeEvents now = undefined


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
