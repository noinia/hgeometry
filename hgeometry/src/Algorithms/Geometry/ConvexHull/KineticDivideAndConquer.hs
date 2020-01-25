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
-- import qualified Data.DoublyLinkedList as DLList
-- import           Data.DoublyLinkedList (DLList)
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
                 $ runDLListMonad (newDLList pts)
                 . (>>= output)
                 . divideAndConquer1 mkLeaf
                 $ NonEmpty.fromList [0..(n-1)]
  where
    n = V.length pts
    fromNonEmpty = V.fromList . NonEmpty.toList
    (pts,exts) = bimap V.fromList V.fromList . unExt . NonEmpty.sortWith (^.core.xCoord) $ pts'
    unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])


type HullM s r = DLListMonad s (Point 3 r)

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

reportEdges :: EventKind -> HullM s r [Two Index]
reportEdges = \case
    InsertAfter i j  -> comb [Two i j] (\r -> Two j r) <$> getNext i
    InsertBefore i j -> comb [Two j i] (\l -> Two l j) <$> getPrev i
    Delete j         -> asList Two <$> getPrev j <*> getNext j
  where
    comb xs f = maybe xs (\z -> f z : xs)
    asList g ml mr = maybeToList $ g <$> ml <*> mr


instance (Ord r, Fractional r, Show r)
         => Semigroup (HullM s r (MergeStatus r)) where
  lc <> rc = traceShow "<>" $
             do l <- lc
                r <- rc
                let esIn = mergeEvents (events l) (events r)
                    t    = mkT esIn
                STR h u v <- findBridge t l r
                es <- runKinetic t esIn (Bridge u v)
                writeHull h
                pure $ MergeStatus (hd l) (lst r) es
    where
      mkT = \case
        []    -> 0
        (e:_) -> e^.core - 1

pointAt :: Index -> HullM s r (Point 3 r)
pointAt = valueAt

pointAt' :: Index -> Simulation s r (Point 3 r)
pointAt' = lift . pointAt

atTime     :: Num r => r -> Index -> HullM s r (Point 2 r)
atTime t i = (\(Point3 x y z) -> Point2 x (z - t*y)) <$> pointAt i


findBridge    :: (Ord r, Fractional r, Show r)
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

type Bridge = Two Index
pattern Bridge a b = Two a b
{-# COMPLETE Bridge #-}

leftBridgePoint :: Lens' Bridge Index
leftBridgePoint = _1

rightBridgePoint :: Lens' Bridge Index
rightBridgePoint = _2


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

-- | Handler for an event on the left hull
handleLeft                 :: Ord r => Event r -> Simulation s r (Maybe (Event r))
handleLeft e@(Event _ k) = do lift $ applyEvent k
                              u <- gets (^.leftBridgePoint)
                              outputEvent <$> pointAt' l <*> pointAt' u
  where
    l = case k of -- find the rightmost point involved in the event
          InsertAfter _ j  -> j
          InsertBefore _ j -> j
          Delete j         -> j
    outputEvent pl pu = if pl^.xCoord <= pu^.xCoord then Just e else Nothing

-- | Handler for an event on the right hull
handleRight               :: Ord r => Event r -> Simulation s r (Maybe (Event r))
handleRight e@(Event _ k) = do lift $ applyEvent k
                               v <- gets (^.rightBridgePoint)
                               outputEvent <$> pointAt' r <*> pointAt' v
  where
    r = case k of -- find the leftmost point involved in the event
          InsertAfter j _  -> j
          InsertBefore j _ -> j
          Delete j         -> j
    outputEvent pr pv = if pv^.xCoord <= pr^.xCoord then Just e else Nothing

-- | Applies the actual event, mutating the current representation of the hulls.
applyEvent :: EventKind -> HullM s r ()
applyEvent = \case
  InsertAfter i j  -> insertAfter i j
  InsertBefore i j -> insertBefore i j
  Delete j         -> delete j

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

-- | compute the time at which r becomes colinear with the line throuh
-- p and l.
nextTime       :: (Ord r, Fractional r) => Index -> Index -> Index -> Simulation s r r
nextTime p l r = nextTime' <$> pointAt' p <*> pointAt' l <*> pointAt' r

-- | compute the time at which r becomes colinear with the line throuh
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


data Event r = Event { eventTime :: !r
                     , eventKind :: !EventKind
                     } deriving (Show,Eq)

data EventKind = InsertAfter  !Index !Index -- ^ old then new
               | InsertBefore !Index !Index
               | Delete !Index
               deriving (Show,Eq,Ord)

data MergeStatus r = MergeStatus { hd     :: !Index -- first item in the list
                                 , lst    :: !Index -- last item in the list
                                 -- , left   :: !Index -- left element in the bridge
                                 -- , right  :: !Index -- right element in the bridge
                                 , events :: ![Event r]
                                 } deriving (Show,Eq)

mkLeaf   :: Int -> HullM s r (MergeStatus r)
mkLeaf i = do v <- asks llist
              MV.write v i (Cell Nothing Nothing)
              pure $ MergeStatus i i []

----------------------------------------------------------------------------------
-- * Doubly Linked List

type Index = Int

-- | Cells in the Linked List
data Cell = Cell { prev :: Maybe Index
                 , next :: Maybe Index
                 } deriving (Show,Eq)

-- | Doubly linked list implemented by a mutable vector. So actually
-- this data type can represent a collection of Linked Lists that can
-- efficiently be concatenated and split.
data DLList s a = DLList { values :: !(V.Vector a)
                         , llist  :: !(MV.MVector s Cell)
                         }

instance Functor (DLList s) where
  fmap f (DLList v l) = DLList (fmap f v) l


--------------------------------------------------------------------------------

newtype DLListMonad s b a = DLListMonad { runDLListMonad' :: ReaderT (DLList s b) (ST s) a }
                         deriving (Functor,Applicative,Monad)

instance PrimMonad (DLListMonad s b) where
  type PrimState (DLListMonad s b) = s
  primitive = DLListMonad . primitive

instance MonadReader (DLList s b) (DLListMonad s b) where
  local f = DLListMonad . local f . runDLListMonad'
  ask = DLListMonad $ ask

runDLListMonad         :: Show b => (ST s (DLList s b)) -> DLListMonad s b a -> ST s a
runDLListMonad mkE sim = mkE >>= \env -> (flip runReaderT env . runDLListMonad') sim



----------------------------------------

newDLList    :: (PrimMonad m, s ~ PrimState m) => V.Vector b -> m (DLList s b)
newDLList vs = DLList vs <$> MV.new (V.length vs)



-- | Copies the hull h into llist
writeHull   :: NonEmpty Index -> DLListMonad s b ()
writeHull h = do v <- asks llist
                 forM_ (withNeighs h) $ \(STR p i s) ->
                   modify v i $ \c -> c { prev = p , next = s }
  where
    withNeighs (x:|xs) = let l = x:xs
                         in zipWith3 STR (Nothing : map Just l) l (map Just xs ++ [Nothing])

----------------------------------------
-- * Queries

valueAt    :: Index -> DLListMonad s b b
valueAt  i = asks ((V.! i) . values)

getNext   :: Index -> DLListMonad s b (Maybe Index)
getNext i = do v <- asks llist
               next <$> MV.read v i

getPrev   :: Index -> DLListMonad s b (Maybe Index)
getPrev i = do v <- asks llist
               prev <$> MV.read v i

toListFrom   :: Index -> DLListMonad s b (NonEmpty Index)
toListFrom i = (i :|) <$> iterateM getNext i

toListFromR :: Index -> DLListMonad s b (NonEmpty Index)
toListFromR i = (i :|) <$> iterateM getPrev i

----------------------------------------
-- * Updates

-- | Inserts the second argument after the first one into the linked list
insertAfter     :: Index -> Index -> DLListMonad s b ()
insertAfter i j = do v  <- asks llist
                     mr <- getNext i
                     modify  v i  $ \c -> c { next = Just j }
                     modify  v j  $ \c -> c { prev = Just i , next = mr }
                     mModify v mr $ \c -> c { prev = Just j }

-- | Inserts the second argument before the first one into the linked list
insertBefore     :: Index -> Index -> DLListMonad s b ()
insertBefore i h = do v <- asks llist
                      ml <- getPrev i
                      mModify v ml $ \c -> c { next = Just h }
                      modify  v h  $ \c -> c { prev = ml , next = Just i }
                      modify  v i  $ \c -> c { prev = Just h }

-- | Deletes the element from the linked list
delete   :: Index -> DLListMonad s b ()
delete j = do v <- asks llist
              ml <- getPrev j
              mr <- getNext j
              modify  v j  $ \c -> c { prev = Nothing, next = Nothing }
              mModify v ml $ \c -> c { next = mr }
              mModify v mr $ \c -> c { prev = ml }

----------------------------------------
-- * Helper functions

iterateM  :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
iterateM f = go
  where
    go x = f x >>= \case
             Nothing -> pure []
             Just y  -> (y:) <$> go y

mModify   :: PrimMonad m => MV.MVector (PrimState m) a -> Maybe Int -> (a -> a) -> m ()
mModify v mi f = case mi of
                   Nothing -> pure ()
                   Just i  -> modify v i f

modify        :: PrimMonad m => MV.MVector (PrimState m) a -> Int -> (a -> a) -> m ()
modify v i f = MV.modify v f i

--------------------------------------------------------------------------------

minimumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn f = \case
    [] -> Nothing
    xs -> Just $ List.minimumBy (comparing f) xs

--------------------------------------------------------------------------------


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

test' = run myPts
test = lowerHull' myPts

-- foo      :: NonEmpty (Point 3 Rational :+ Int) -> V.Vector (Point 3 Rational)
-- foo pts' = runST $ runDLListMonad (Env pts <$> MV.new 5) sim
--   where
--     (pts,exts) = traceShowId $ bimap V.fromList V.fromList . unExt . NonEmpty.sortWith (^.core.xCoord) $ pts'

--     unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])

--     sim :: DLListMonad s Rational (V.Vector (Point 3 Rational))
--     sim = traceShow "foo"$ asks (traceShowId . points . traceShow "going")
