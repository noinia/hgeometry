module Algorithms.Geometry.ConvexHull.KineticDivideAncConquer where

import           Algorithms.DivideAndConquer
import           Control.Lens ((^.), bimap, Lens', _1, _2)
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Control.Monad.ST
import           Control.Monad.State.Class (gets, get, put)
import           Control.Monad.State.Strict (StateT, evalStateT)
import           Control.Monad.Trans
import           Control.Monad.Writer (Writer)
import           Data.Bitraversable
import           Data.Foldable (forM_)
import           Data.Ord (comparing)
import           Data.IndexedDoublyLinkedList
import           Data.Maybe
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Convex (lowerTangent')
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import           Debug.Trace

--------------------------------------------------------------------------------0

-- type ConvexHull p r = [Two (Point 3 r :+ p)]


type ConvexHull d p r = [Two (Point 3 r)]

  -- V.Vector [Index] -- [Edge]--[Two (Point 3 r :+ p)]


-- lowerHull :: (Ord r, Fractional r) => [Point 3 r :+ p] -> ConvexHull 3 p r
-- lowerHull = maybe mempty lowerHull' . NonEmpty.nonEmpty

lowerHull'      :: (Ord r, Fractional r, Show r) => NonEmpty (Point 3 r :+ p) -> ConvexHull 3 p r
lowerHull' pts' = runST
                 $ runDLListMonad (singletons pts)
                 . (>>= output)
                 . divideAndConquer1 mkLeaf
                 $ NonEmpty.fromList [0..(n-1)]
  where
    n = V.length pts
    fromNonEmpty = V.fromList . NonEmpty.toList
    (pts,exts) = bimap V.fromList V.fromList . unExt . NonEmpty.sortWith (^.core.xCoord) $ pts'
    unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])

-- | Creates a Leaf
mkLeaf   :: Int -> HullM s r (MergeStatus r)
mkLeaf i = pure $ MergeStatus i i []

type HullM s r = DLListMonad s (Point 3 r)

--------------------------------------------------------------------------------

instance (Ord r, Fractional r, Show r)
         => Semigroup (HullM s r (MergeStatus r)) where
  lc <> rc = traceShow "<>" $
             do l <- lc
                r <- rc
                let esIn = mergeEvents (events l) (events r)
                    t    = mkT esIn
                STR h u v <- traceShowId <$> findBridge t l r
                es <- runKinetic t esIn (Bridge u v)
                writeList h
                pure $ MergeStatus (hd l) (lst r) es
    where
      mkT = \case
        []    -> (-10000000) -- TODO; at what time value should we start?
        (e:_) -> e^.core - 1

--------------------------------------------------------------------------------
-- * Producing the Output Hull

-- | Reports all the edges on the CH
output    :: Show r => MergeStatus r -> HullM s r (ConvexHull 3 p r)
output ms | traceShow ("output: ", events ms) False = undefined
output ms = do h  <- toListFrom (hd ms)
               es  <- sequence $ zipWith mkEdge (NonEmpty.toList h) (NonEmpty.tail h)
               es' <- concat <$> mapM (handle . eventKind) (events ms)
               pure $ es <> es'
  where
    handle ek = do es <- reportEdges ek
                   applyEvent ek
                   mapM (\(Two u v) -> mkEdge u v) es

    mkEdge u v = Two <$> pointAt u <*> pointAt v

-- | Given an event, produces the list of (at most two) new edges on the hull.
reportEdges :: EventKind -> HullM s r [Two Index]
reportEdges = \case
    InsertAfter i j  -> comb [Two i j] (\r -> Two j r) <$> getNext i
    InsertBefore i j -> comb [Two j i] (\l -> Two l j) <$> getPrev i
    Delete j         -> asList Two <$> getPrev j <*> getNext j
  where
    comb xs f = maybe xs (\z -> f z : xs)
    asList g ml mr = maybeToList $ g <$> ml <*> mr

--------------------------------------------------------------------------------
-- * Finding the Bridge

type Bridge = Two Index

pattern Bridge     :: Index -> Index -> Bridge
pattern Bridge a b = Two a b
{-# COMPLETE Bridge #-}

leftBridgePoint, rightBridgePoint :: Lens' Bridge Index
leftBridgePoint  = _1
rightBridgePoint = _2

-- | Computes the Bridge of the Hulls (the Hulls currently encoded in
-- the underlying Doublylinkedlist)
--
-- running time: \(O(n)\)
findBridge       :: (Ord r, Fractional r, Show r)
                 => r
                 -> MergeStatus r
                 -> MergeStatus r
                 -> HullM s r (STR (NonEmpty Index) Index Index)
findBridge t l r = do lh <- mapM (atTime' t) =<< toListFromR (lst l)
                      rh <- mapM (atTime' t) =<< toListFrom  (hd r)
                      let Two (u :+ ls) (v :+ rs) = findBridge' lh rh
                      pure $ STR (NonEmpty.fromList $ ls <> [u,v] <> rs) u v
  where
    atTime' t' i = (:+ i) <$> atTime t' i

    findBridge' l0 r0 = bimap f f $ lowerTangent' l0 r0
    f (c :+ es) = c^.extra :+ ((^.extra) <$> es)

--------------------------------------------------------------------------------

-- | The Kinetic Simulation
type Simulation s r = StateT Bridge (HullM s r)

-- | For each of the events form the left and right hulls, construct
-- the appropriate event handler, and merge the two streams of events
-- into one stream.
--
-- Note that the event handler is a piece of code that, when run, may
-- produce an output event (i.e. an event that states that there is a
-- change in the complete hull). Note that whether or not this event
-- is produced depends on the position of the bridge at the time when
-- we handle the event.
mergeEvents       :: Ord r => [Event r] -> [Event r] -> [r :+ Simulation s r (Maybe (Event r))]
mergeEvents ls rs = mergeSortedListsBy (comparing $ (^.core)) (f handleLeft  <$> ls)
                                                              (f handleRight <$> rs)
  where
    f handler e@(Event t _) = t :+ handler e

-- | run the kinetic simulation, computing the events at which the
-- hull changes. At any point during the simulation:
--
-- - the multable array in env represents the hulls L and R
-- - we maintain the current bridge u on L and v on R such that
-- - L[1..u] <> R[v..n] is the output hull H
runKinetic        :: (Ord r, Fractional r, Show r )
                  => r          -- starting time
                  -> [r :+ Simulation s r (Maybe (Event r))]  -- existing events
                  -> Bridge -- initial bridge
                  -> HullM s r [Event r]
runKinetic t es b = evalStateT (handleEvent t es) b

-- | The actual code for handling an event in the kinetic
-- simulation. At every step, we recompute what the next bridge event
-- is, and apply the first event that occurs (either this bridge
-- event, or one of the existing events).
handleEvent        :: (Ord r, Fractional r, Show r)
                   => r -> [r :+ Simulation s r (Maybe (Event r))] -> Simulation s r [Event r]
handleEvent now es | traceShow ("HandleEvent ", now, length es) False = undefined
handleEvent now es = nextBridgeEvent now >>= \mbe -> case (es, mbe) of
    ([],Nothing)                     -> pure []
    ([],Just be)                     -> handleBridge be []
    (e:es', Nothing)                 -> handleExisting e es'
    (e:es', Just be) | e `before` be -> handleExisting e es'
                     | otherwise     -> handleBridge be es
  where
    cons me outEvents = maybeToList me <> outEvents

    before (a :+ _) (b :+ _) = a < b
      -- if bridge event and other event occur simultaneously, do the bridge event first
    handleExisting (t :+ h) es' = cons <$> h <*> handleEvent t es'
    handleBridge   (t :+ h) es' = (:)  <$> h <*> handleEvent t es'

----------------------------------------
-- * Handling the events in the Existing Hulls

handleLeft   :: Ord r => Event r -> Simulation s r (Maybe (Event r))
handleLeft e = handleExisting' e leftBridgePoint (<=) l
  where
    l = case eventKind e of -- find the rightmost point involved in the event
          InsertAfter _ j  -> j
          InsertBefore _ j -> j
          Delete j         -> j

handleRight   :: Ord r => Event r -> Simulation s r (Maybe (Event r))
handleRight e = handleExisting' e rightBridgePoint (>=) r
  where
    r = case eventKind e of -- find the leftmost point involved in the event
          InsertAfter j _  -> j
          InsertBefore j _ -> j
          Delete j         -> j

-- | Handler for an event on the right hull
--
handleExisting'  :: forall s r. Event r -> Lens' Bridge Index -> (r -> r -> Bool) -> Index
                 -> Simulation s r (Maybe (Event r))
handleExisting' e bridgePoint cmp p =
    do lift $ applyEvent (eventKind e)
       v <- gets (^.bridgePoint)
       outputEvent <$> pointAt' p <*> pointAt' v
  where
    outputEvent pp bp = if (pp^.xCoord) `cmp` (bp^.xCoord) then Just e else Nothing

-- | Applies the actual event, mutating the current representation of the hulls.
applyEvent :: EventKind -> HullM s r ()
applyEvent = \case
  InsertAfter i j  -> insertAfter i j
  InsertBefore i j -> insertBefore i j
  Delete j         -> delete j

----------------------------------------
-- * Bridge Events

-- | Given the current time, computes the next bridge event (if it
-- exists) and a handler to handle this bridge event.
nextBridgeEvent     :: (Ord r, Fractional r)
                    => r -> Simulation s r (Maybe (r :+ Simulation s r (Event r)))
nextBridgeEvent now = do b <- get
                         fmap mkHandler <$> nextBridgeEvent' now b
  where
    mkHandler (t :+ (b',k)) = t :+ do put b'
                                      pure $ Event t k

-- | Finds the next event involving the current bridge.
-- The arguments are the current time, and the current bridge indices.
nextBridgeEvent'                  :: forall r s. (Ord r, Fractional r)
                                  => r -> Bridge
                                  -> Simulation s r (Maybe (r :+ (Bridge,EventKind)))
nextBridgeEvent' now (Bridge l r) = findCand <$> mapM runCand cands
  where
    findCand cs = minimumOn (^.core) [ t :+ x | Just (t :+ x) <- cs, now < t]

    runCand (c,f) = lift c >>= \case
      Nothing -> pure Nothing
      Just x  -> Just <$> bitraverse id pure (f x)

    -- candiate next events. Either one of the neighbours of the left bridge endpoint l
    -- becomes colinear with the bridge, or one of the neighbours of r becomes colinear
    -- with the bridge.
    cands :: [ ( HullM s r (Maybe Index), Index -> Simulation s r r :+ (Bridge, EventKind) )]
    cands = [ (getPrev l, \a -> nextTime a l r :+ (Bridge a r, Delete l))
            , (getNext l, \b -> nextTime l b r :+ (Bridge b r, InsertAfter  l b))
            , (getPrev r, \c -> nextTime l c r :+ (Bridge l c, InsertBefore c r))
            , (getNext r, \d -> nextTime l r d :+ (Bridge l d, Delete r))
            ]

----------------------------------------
-- * Helpers for computing the next interesting time in the simulation

-- | compute the time at which r becomes colinear with the line throuh
-- p and l.
nextTime       :: (Ord r, Fractional r) => Index -> Index -> Index -> Simulation s r r
nextTime p l r = nextTime' <$> pointAt' p <*> pointAt' l <*> pointAt' r

-- | compute the time at which r becomes colinear with the line through
-- p and l.
nextTime'  :: (Ord r, Fractional r) => Point 3 r -> Point 3 r -> Point 3 r -> r
nextTime' (Point3 px py pz) (Point3 lx ly lz) (Point3 rx ry rz) = t
  where
    ux = lx - px
    vx = rx - px

    a = (-1)*ux*(rz - pz)  - vx*(lz - pz)
    b =      ux*(py - ry)  - vx*(py - ly)
    t = a / b
    -- by unfolding the def of ccw

--------------------------------------------------------------------------------

data Event r = Event { eventTime :: !r
                     , eventKind :: !EventKind
                     } deriving (Show,Eq)

data EventKind = InsertAfter  !Index !Index -- ^ current Index first, then the Item we insert
               | InsertBefore !Index !Index -- ^ current Index first, then the Item we insert
               | Delete !Index
               deriving (Show,Eq,Ord)

data MergeStatus r = MergeStatus { hd     :: !Index -- ^ first item in the list
                                 , lst    :: !Index -- ^ last item in the list
                                 , events :: ![Event r] -- ^ Events when this Hull changes
                                 } deriving (Show,Eq)

----------------------------------------------------------------------------------
-- * Convienience Functions in the Hull Monad.

pointAt :: Index -> HullM s r (Point 3 r)
pointAt = valueAt

pointAt' :: Index -> Simulation s r (Point 3 r)
pointAt' = lift . pointAt

atTime     :: Num r => r -> Index -> HullM s r (Point 2 r)
atTime t i = (\(Point3 x y z) -> Point2 x (z - t*y)) <$> pointAt i


--------------------------------------------------------------------------------
-- * Pure Helpers

minimumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn f = \case
    [] -> Nothing
    xs -> Just $ List.minimumBy (comparing f) xs

--------------------------------------------------------------------------------
-- * Testing stuff

run      :: (Ord r, Fractional r, Show r) => NonEmpty (Point 3 r :+ p) -> MergeStatus r
run pts' = runST
                 $ runDLListMonad (newDLList pts)
                 -- . (>>= output)
                 . divideAndConquer1 mkLeaf
                 $ NonEmpty.fromList [0..(n-1)]
  where
    n = V.length pts
    fromNonEmpty = V.fromList . NonEmpty.toList
    (pts,exts) = bimap V.fromList V.fromList . unExt . NonEmpty.sortWith (^.core.xCoord) $ pts'
    unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])


myPts :: NonEmpty (Point 3 Rational :+ Int)
myPts = NonEmpty.fromList $ [ Point3 1 1 0 :+ 1
                            , Point3 3 2 0 :+ 2
                            , Point3 5 5 0 :+ 3
                            , Point3 10 20 0 :+ 4
                            ]

myPts' :: NonEmpty (Point 3 Rational :+ Int)
myPts' = NonEmpty.fromList $ [ Point3 5  5  0  :+ 0
                             , Point3 1  1  10 :+ 1
                             , Point3 0  10 20 :+ 2
                             , Point3 12 1  1  :+ 3
                             ]



test' = run myPts

test = mapM_ print $ lowerHull' myPts'

-- foo      :: NonEmpty (Point 3 Rational :+ Int) -> V.Vector (Point 3 Rational)
-- foo pts' = runST $ runDLListMonad (Env pts <$> MV.new 5) sim
--   where
--     (pts,exts) = traceShowId $ bimap V.fromList V.fromList . unExt . NonEmpty.sortWith (^.core.xCoord) $ pts'

--     unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])

--     sim :: DLListMonad s Rational (V.Vector (Point 3 Rational))
--     sim = traceShow "foo"$ asks (traceShowId . points . traceShow "going")
