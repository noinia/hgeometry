module Algorithms.Geometry.ConvexHull.KineticDivideAncConquer where

import           Algorithms.DivideAndConquer
import           Control.Applicative (liftA2)
import           Control.Lens ((^.), bimap, Lens', _1, _2)
import           Control.Monad ((<=<))
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Control.Monad.ST
import           Control.Monad.State.Class (gets, get, put)
import           Control.Monad.State.Strict (StateT, evalStateT)
import           Control.Monad.Trans
import           Control.Monad.Writer (Writer)
import           Data.Bitraversable
import           Data.Ext
import           Data.Foldable (forM_)
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Convex (lowerTangent')
import           Data.IndexedDoublyLinkedList
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import           Data.Maybe (catMaybes)
import           Debug.Trace
import           Data.Ratio
--------------------------------------------------------------------------------


-- TODO: We seem to assume that no four points are coplanar, and no
-- three points lie on a vertical plane. Figure out where we assume that exactly.

-- FIXME: We start with some arbitrary starting slope. Fix that

-- plumb the p's around.


-- type ConvexHull d p r = [Three Index]
type ConvexHull d p r = [Three (Point 3 r :+ p)]


-- lowerHull :: (Ord r, Fractional r) => [Point 3 r :+ p] -> ConvexHull 3 p r
-- lowerHull = maybe mempty lowerHull' . NonEmpty.nonEmpty


lowerHull'      :: forall r p. (Ord r, Fractional r, Show r) => NonEmpty (Point 3 r :+ p) -> ConvexHull 3 p r
lowerHull' pts' = map withPt $ runDLListMonad pts computeHull
  where
    computeHull :: HullM s r [Three Index]
    computeHull = output <=< divideAndConquer1 mkLeaf $ NonEmpty.fromList [0..(n-1)]

    n = V.length pts
    fromNonEmpty = V.fromList . NonEmpty.toList
    (pts,exts) = bimap V.fromList V.fromList . unExt . NonEmpty.sortWith (^.core.xCoord) $ pts'
    unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])

    -- withPt = id
    withPt = fmap (\i -> pts V.! i :+ exts V.! i)


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
                d <- debugHull l r
                let esIn = traceShow d $ mergeEvents (events l) (events r)
                    t    = mkT esIn
                STR h u v <- traceShow ("before merge:",d) <$> findBridge t l r
                es <- runKinetic t esIn (Bridge u v)
                writeList $ traceShow ("writing hull: ",h) h
                pure $ MergeStatus (hd l) (lst r) es
    where
      mkT = \case
        []    -> (-10000000) -- TODO; at what time value should we start?
        (e:_) -> e^.core - 1

      debugHull l r = (\a b c d -> (NonEmpty.toList a
                                   , NonEmpty.toList b
                                   , c
                                   , d
                                   , events l
                                   , events r
                                   )
                      )
                      <$> (toListFrom $ hd l) <*> (toListFrom $ hd r)
                      <*> listHull l <*> listHull r

      listHull s = do xs <- NonEmpty.toList <$> toListFrom (hd s)
                      mapM (atTime t) xs
         where
           t = (-10000000)

--------------------------------------------------------------------------------
-- * Producing the Output Hull

-- | Reports all the edges on the CH
output    :: Show r => MergeStatus r -> HullM s r [Three Index]
output ms | traceShow ("output: ", events ms) False = undefined
output ms = catMaybes <$> mapM (handle . eventKind) (events ms)
  where
    handle ek = do mt <- reportTriangle ek
                   applyEvent ek
                   pure mt

    -- u v w =  <$> pointAt u <*> pointAt v

  -- do h  <- toListFrom (hd ms)
  --              -- es  <- sequence $ zipWith mkEdge (NonEmpty.toList h) (NonEmpty.tail h)
  --              es' <- concat <$> mapM (handle . eventKind) (events ms)
  --              pure es' -- $ es <> es'
  -- where
  --   handle ek = do es <- reportEdges ek
  --                  applyEvent ek
  --                  mapM (\(Two u v) -> mkEdge u v) es

  --   mkEdge u v = Two <$> pure u <*> pure v
  --   -- mkEdge u v = Two <$> pointAt u <*> pointAt v

reportTriangle :: EventKind -> HullM s r (Maybe (Three Index))
reportTriangle = \case
    InsertAfter i j  -> fmap (\r   -> Three i j r)   <$> getNext i
    InsertBefore i h -> fmap (\l   -> Three l h i)   <$> getPrev i
    Delete j         -> liftA2 (\l r -> Three l j r) <$> getPrev j <*> getNext j

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

    findBridge' l0 r0 = f <$> lowerTangent' l0 r0
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
  InsertBefore i h -> insertBefore i h
  Delete j         -> delete j

----------------------------------------
-- * Bridge Events

-- | Given the current time, computes the next bridge event (if it
-- exists) and a handler to handle this bridge event.
nextBridgeEvent     :: (Ord r, Fractional r, Show r )
                    => r -> Simulation s r (Maybe (r :+ Simulation s r (Event r)))
nextBridgeEvent now = do b <- get
                         es <- nextBridgeEvents b
                         fmap mkHandler <$> nextBridgeEvent' now (debug es b)
  where
    mkHandler (t :+ (b',k)) = t :+ do put b'
                                      pure $ Event t k

    debug es b = traceShow ("candidate events with ",b," ", es) b

-- evalNextBridgeEvents   :: Bridge -> Simulation s r [r :+ (Bridge,EventKind)]
-- evalNextBridgeEvents b = do cans <- nextBridgeEvents b
--                             mapM (\(computeT :+ z) -> (:+ z) <$> computeT
--                                  ) $ cans

-- | Finds the next event involving the current bridge.
-- The arguments are the current time, and the current bridge indices.
nextBridgeEvent'       :: (Ord r, Fractional r)
                       => r -> Bridge
                       -> Simulation s r (Maybe (r :+ (Bridge,EventKind)))
nextBridgeEvent' now b = minimumOn (^.core) . filter (\e -> now < e^.core)
                      <$> nextBridgeEvents b

-- | Computes all candidate bridge events
nextBridgeEvents                  :: forall r s. (Ord r, Fractional r)
                                  => Bridge
                                  -> Simulation s r [r :+ (Bridge,EventKind)]
nextBridgeEvents (Bridge l r) = catMaybes <$> mapM runCand cands
  where
    runCand (c,f) = lift c >>= \case
      Nothing -> pure Nothing
      Just x  -> Just <$> bitraverse id pure (f x)

    -- candiate next events. Either one of the neighbours of the left bridge endpoint l
    -- becomes colinear with the bridge, or one of the neighbours of r becomes colinear
    -- with the bridge.
    cands :: [ ( HullM s r (Maybe Index), Index -> Simulation s r r :+ (Bridge, EventKind) )]
    cands = [ (getPrev l, \a -> nextTime a l r :+ (Bridge a r, Delete l))
            , (getNext l, \b -> nextTime l b r :+ (Bridge b r, InsertAfter  l b))
            , (getPrev r, \c -> nextTime l c r :+ (Bridge l c, InsertBefore r c))
            , (getNext r, \d -> nextTime l r d :+ (Bridge l d, Delete r))
            ]

----------------------------------------
-- * Helpers for computing the next interesting time in the simulation

-- | compute the time at which r becomes colinear with the line throuh
-- p and q.
nextTime       :: (Ord r, Fractional r) => Index -> Index -> Index -> Simulation s r r
nextTime p q r = nextTime' <$> pointAt' p <*> pointAt' q <*> pointAt' r

-- | compute the time at which r becomes colinear with the line through
-- p and q.
nextTime'  :: (Ord r, Fractional r) => Point 3 r -> Point 3 r -> Point 3 r -> r
nextTime' (Point3 px py pz) (Point3 qx qy qz) (Point3 rx ry rz) = a / b
  where        -- by unfolding the def of ccw
    ux = qx - px
    vx = rx - px
    a = ux*(rz - pz)  - vx*(qz - pz)
    b = ux*(ry - py)  - vx*(qy - py)


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

myPts :: NonEmpty (Point 3 Double :+ Int)
myPts = NonEmpty.fromList $ [ Point3 5  5  0  :+ 2
                            , Point3 1  1  10 :+ 1
                            , Point3 0  10 20 :+ 0
                            , Point3 12 1  1  :+ 3
                            , Point3 22 20  1  :+ 4
                            ]

myPts' :: NonEmpty (Point 3 Double :+ Int)
myPts' = NonEmpty.fromList $ [ Point3 5  5  0  :+ 2
                             , Point3 1  1  10 :+ 1
                             , Point3 0  10 20 :+ 0
                             , Point3 12 1  1  :+ 3
                             ]



test' = mapM_ print $ lowerHull' myPts

test = mapM_ print $ lowerHull' myPts'
