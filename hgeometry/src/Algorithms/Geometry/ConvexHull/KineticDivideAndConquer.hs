{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.ConvexHull.KineticDivideAncConquer where

import           Algorithms.DivideAndConquer
import           Control.Lens ((^.), bimap)
import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Control.Monad.ST
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
import           Data.Geometry.Polygon.Convex(lowerTangent')
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import           Debug.Trace

--------------------------------------------------------------------------------0

-- type ConvexHull p r = [Two (Point 3 r :+ p)]


type ConvexHull d p r = V.Vector [Index] -- [Edge]--[Two (Point 3 r :+ p)]


-- lowerHull :: (Ord r, Fractional r) => [Point 3 r :+ p] -> ConvexHull 3 p r
-- lowerHull = maybe mempty lowerHull' . NonEmpty.nonEmpty

lowerHull'      :: (Ord r, Fractional r, Show r) => NonEmpty (Point 3 r :+ p) -> ConvexHull 3 p r
lowerHull' pts' = runST
                 $ runSimulation (Env pts <$> newLList n)
                 . (>>= output)
                 . divideAndConquer1 mkLeaf
                 $ NonEmpty.fromList [0..(n-1)]
  where
    n = V.length pts
    fromNonEmpty = V.fromList . NonEmpty.toList
    (pts,exts) = bimap V.fromList V.fromList . unExt . NonEmpty.sortWith (^.core.xCoord) $ pts'
    unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])
    newLList n = MV.new n

type Index = Int
data Cell = Cell { prev :: Maybe Index
                 , next :: Maybe Index
                 } deriving (Show,Eq)

data Env s r = Env { points :: !(V.Vector (Point 3 r))
                   , llist  :: !(MV.MVector s Cell)
                   }

newtype Simulation s r a = Simulation { runSimulation' :: ReaderT (Env s r) (ST s) a }
                         deriving (Functor,Applicative,Monad)

instance PrimMonad (Simulation s r) where
  type PrimState (Simulation s r) = s
  primitive = Simulation . primitive

instance MonadReader (Env s r) (Simulation s r) where
  local f = Simulation . local f . runSimulation'
  ask = Simulation $ ask

runSimulation         :: Show r => (ST s (Env s r)) -> Simulation s r a -> ST s a
runSimulation mkE sim = mkE >>= \env -> (flip runReaderT env . runSimulation') sim


output   :: MergeStatus r -> Simulation s r (ConvexHull 3 p r)
output _ = pure mempty


instance (Ord r, Fractional r, Show r) => Semigroup (Simulation s r (MergeStatus r)) where
  lc <> rc = traceShow "<>" $
             do l <- lc
                r <- rc
                let esIn = traceShowId $ mergeSortedListsBy (comparing eventTime) (events l) (events r)
                STR h u v <- findBridge (t esIn) l r
                es <- runKinetic l r h u v
                writeHull h
                pure $ MergeStatus (hd l) (lst r) u v es
    where
      t = \case
        []    -> 0
        (e:_) -> eventTime e - 1


type LList s = MV.MVector s Cell


askPoint   :: Index -> Simulation s r (Point 3 r)
askPoint i = asks ((V.! i) . points)

atTime     :: Num r => r -> Index -> Simulation s r (Point 2 r)
atTime t i = (\(Point3 x y z) -> Point2 x (z - t*y)) <$> askPoint i


findBridge    :: (Ord r, Fractional r, Show r)
              => r
              -> MergeStatus r
              -> MergeStatus r
              -> Simulation s r (STR (NonEmpty Index) Index Index)
findBridge t l r = do lh <- mapM (atTime' t) =<< toListFromR (lst l)
                      rh <- mapM (atTime' t) =<< toListFrom  (hd r)
                      let Two (u :+ ls) (v :+ rs) = findBridge' lh rh
                      pure $ STR (NonEmpty.fromList $ ls <> [u,v] <> rs) u v
  where
    atTime' t' i = (:+ i) <$> atTime t' i

    findBridge' l0 r0 = bimap f f $ lowerTangent' l0 r0
    f (c :+ es) = c^.extra :+ ((^.extra) <$> es)

-- -- FIXME: We should be able to use lowerTangent' from Data.Geometry.Convex here now.
-- findBridge'       :: (Ord r, Fractional r)
--                   => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
--                   -> Two (SP p [p])
-- findBridge' l0 r0 = bimap f f $ go l0 r0
--   where
--     f (SP x xs) = SP (x^.extra) ((^.extra) <$> xs)
--     ne = NonEmpty.fromList
--     isRight' []    _ _ = False
--     isRight' (x:_) l r = ccw' l r x == CW

--     go lh@(l:|ls) rh@(r:|rs) | isRight' rs l r = go lh      (ne rs)
--                              | isRight' ls l r = go (ne ls) rh
--                              | otherwise       = Two (SP l ls) (SP r rs)

type Bridge = Two Index
pattern Bridge a b = Two a b

-- run the kinetic simulation, computing the events at which the hull
-- changes. At any point during the simulation:

-- - the multable array in env represents the hulls L and R
-- - we maintain the current bridge u on L and v on R such that
-- - L[1..u] <> R[v..n] is the output hull H
runKinetic           :: MergeStatus r    -- left merge status
                     -> MergeStatus r     -- right merge status
                     -> NonEmpty Index      -- current hull
                     -> Index -> Index
                     -> Simulation s r [Event r]
runKinetic l r h u v = do -- e <- nexEvent (events l) (events) r


-- runKinetic' :: EventQueue r ->


  pure []

nextEvent           :: [Event r]
                    -> [Event r]
                    -> Index -> Index -- bridge
                    -> Simulation s r [Event r]
nextEvent ls rs u v = undefined


-- process :: [Event r] -> [Event r] -> Maybe (Event r) ->

-- handleLeft :: Index -> [Event r] -> Simulation s r (Maybe (Event r), [Event r])
-- handleLeft u = \case
--   []                          -> pure (Nothing, [])
--   (e@(Event _ k):es) | leftOf k u -> pure (Just e, es)
--                      | otherwise


applyEvent :: EventKind -> Simulation s r ()
applyEvent = \case
  InsertAfter i j  -> insertAfter i j
  InsertBefore i j -> insertBefore i j
  Delete j         -> delete j


handleLeft                 :: Ord r => Event r -> Index -> Simulation s r (Maybe (Event r))
handleLeft e@(Event _ k) u = g <$> askPoint l <*> askPoint u
  where
    l = case k of -- find the rightmost point involved in the event
      InsertAfter _ j  -> j
      InsertBefore _ j -> j
      Delete j         -> j

    g pl pu = if pl^.xCoord <= pu^.xCoord then Just e else Nothing

handleRight                 :: Ord r => Event r -> Index -> Simulation s r (Maybe (Event r))
handleRight e@(Event _ k) v = g <$> askPoint r <*> askPoint v
  where
    r = case k of -- find the leftmost point involved in the event
      InsertAfter j _  -> j
      InsertBefore j _ -> j
      Delete j         -> j

    g pr pv = if pv^.xCoord <= pr^.xCoord then Just e else Nothing





-- handleEvent       :: Event r -> Bridge
--                   -> Simulation s r (Maybe (Event r), Bridge) -- output event and new bridge
-- handleEvent e u v = undefined



-- | Finds the next event involving the current bridge
nextBridgeEvent         :: forall r s. (Ord r, Fractional r)
                        => r -> Index -> Index
                        -> Simulation s r (Maybe (Event r, Bridge))
nextBridgeEvent now l r = findCand <$> mapM runCand cands
  where
    findCand cs = (\(t :+ (b,k)) -> (Event t k, b))
               <$> minimumOn (^.core) [ t :+ x | Just (t :+ x) <- cs, now <= t]

    runCand (c,f) = c >>= \case
      Nothing -> pure Nothing
      Just x  -> Just <$> bitraverse id pure (f x)

    -- candiate next events
    cands :: [ ( Simulation s r (Maybe Index), Index -> Simulation s r r :+ (Bridge, EventKind) )]
    cands = [ (getPrev l, \a -> nextTime a l r :+ (Bridge a r, Delete l))
            , (getNext l, \b -> nextTime l b r :+ (Bridge b r, InsertAfter  l b))
            , (getPrev r, \c -> nextTime l c r :+ (Bridge l c, InsertBefore c r))
            , (getNext r, \d -> nextTime l r d :+ (Bridge l d, Delete r))
            ]


-- | compute the time at which r becomes colinear with the line throuh
-- p and l.
nextTime       :: (Ord r, Fractional r) => Index -> Index -> Index -> Simulation s r r
nextTime p l r = nextTime' <$> askPoint p <*> askPoint l <*> askPoint r

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


-- | Copies the hull h into llist
writeHull   :: NonEmpty Index -> Simulation s r ()
writeHull h = do v <- asks llist
                 forM_ (withNeighs h) $ \(STR p i s) ->
                   modify v i $ \c -> c { prev = p , next = s }
  where
    withNeighs (x:|xs) = let l = x:xs
                         in zipWith3 STR (Nothing : map Just l) l (map Just xs ++ [Nothing])


getNext   :: Index -> Simulation s r (Maybe Index)
getNext i = do v <- asks llist
               next <$> MV.read v i

getPrev   :: Index -> Simulation s r (Maybe Index)
getPrev i = do v <- asks llist
               prev <$> MV.read v i


iterateM  :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
iterateM f = go
  where
    go x = f x >>= \case
             Nothing -> pure []
             Just y  -> (y:) <$> go y

toListFrom   :: Index -> Simulation s r (NonEmpty Index)
toListFrom i = (i :|) <$> iterateM getNext i

toListFromR :: Index -> Simulation s r (NonEmpty Index)
toListFromR i = (i :|) <$> iterateM getPrev i

-- | Inserts the second argument after the first one into the linked list
insertAfter     :: Index -> Index -> Simulation s r ()
insertAfter i j = do v <- asks llist
                     mr <- getNext i
                     modify  v i  $ \c -> c { next = Just j }
                     modify  v j  $ \c -> c { prev = Just i , next = mr }
                     mModify v mr $ \c -> c { prev = Just j }

-- | Inserts the second argument before the first one into the linked list
insertBefore     :: Index -> Index -> Simulation s r ()
insertBefore i h = do v <- asks llist
                      ml <- getPrev i
                      mModify v ml $ \c -> c { next = Just h }
                      modify  v h  $ \c -> c { prev = ml , next = Just i }
                      modify  v i  $ \c -> c { prev = Just h }

-- | Deletes the element from the linked list
delete     :: Index -> Simulation s r ()
delete j = do v <- asks llist
              ml <- getPrev j
              mr <- getNext j
              modify  v j  $ \c -> c { prev = Nothing, next = Nothing }
              mModify v ml $ \c -> c { next = mr }
              mModify v mr $ \c -> c { prev = ml }


data Event r = Event { eventTime :: !r
                     , eventKind :: !EventKind
                     } deriving (Show,Eq)

data EventKind = InsertAfter  !Index !Index -- ^ old then new
               | InsertBefore !Index !Index
               | Delete !Index
               deriving (Show,Eq,Ord)

data MergeStatus r = MergeStatus { hd     :: !Index -- first item in the list
                                 , lst    :: !Index -- last item in the list
                                 , left   :: !Index -- left element in the bridge
                                 , right  :: !Index -- right element in the bridge
                                 , events :: ![Event r]
                                 } deriving (Show,Eq)


mkLeaf   :: Int -> Simulation s r (MergeStatus r)
mkLeaf i = do v <- asks llist
              MV.write v i (Cell Nothing Nothing)
              pure $ MergeStatus i i i i []













































-- -- produce adjacencylist from the merge status
-- output :: MergeStatus r -> V.Vector [Vertex]
-- output = undefined


-- -- writeEdge :: Vertex -> Vertex ->





-- type Vertex = Int
-- type Edge = SP Int Int







-- data MergeStatus s r = MergeStatus { initialHull :: LList s
--                                    , events      :: [Event r]
--                                    }

-- type Merger r = Reader (V.Vector (Point 3 r))

-- newtype Merge r = M { runMerge :: Merger r (MergeStatus r)
--                     }

-- instance (Ord r, Fractional r) => Semigroup (Merge r) where
--   (M l) <> (M r) = M $ do MergeStatus lh le <- l
--                           MergeStatus rh re <- r
--                           STR h u v <- joinHullsAt (t le re) lh rh
--                           runSimulation h le re
--                           pure $ MergeStatus h es
--     where
--       t le re = head' le `min` head' re
--       head' = \case
--         []    -> 0 -- if there are no events, pick any time we like
--         (x:_) -> eventTime x




-- joinHuls ls rs = do



-- mkM   :: Vertex -> Merge r
-- mkM v = M . pure $ MergeStatus (v :| []) []






-- --------------------------------------------------------------------------------

-- -- | Computes the new lower hull at time t. Returns the new hull, as well as the
-- -- two bridge vertices
-- joinHullsAt         :: (Ord r, Num r) => r -> NonEmpty Vertex -> NonEmpty Vertex
--                     -> Merger r (STR (NonEmpty Vertex) Vertex Vertex)
-- joinHullsAt t lh@(l0:| _) rh = do pl       <- atTime t l0
--                                   SP rh' r <- walkAt t (leftOf pl) rh
--                                   pr       <- atTime t r
--                                   SP lh' l <- walkAt t (rightOf pr) lh
--                                   pure $ STR (NonEmpty.reverse lh' <> rh') l r
--   where
--     rightOf r s l' = ccw l' s r == CCW
--     leftOf  l' s r = ccw r s l' == CW

-- -- Helper to create the new hull
-- walkAt            :: Num r => r
--                   -> (Point 2 r -> Point 2 r -> Bool) -- ^ wether or not to keep the first arg
--                   -> NonEmpty Vertex
--                   -> Merger r (SP (NonEmpty Vertex) Vertex)
-- walkAt t keep ls0 = go ls0
--   where
--     singleton x = x :| []
--     go (l:|ls') = case NonEmpty.nonEmpty ls' of
--                     Nothing          -> pure $ SP (singleton l) l
--                     Just ls@(s :| _) -> do pl <- atTime t l
--                                            ps <- atTime t s
--                                            if keep ps pl
--                                              then (\(SP xs x) -> SP (l <| xs) x) <$> go ls
--                                              else pure $ SP (singleton l) l


-- -- -- | computes the list that we traversed and a copy of its last element
-- -- walkAt             :: (Ord r, Num r) => r -> NonEmpty Vertex -> Point 2 r
-- --                    -> Merger r (SP (NonEmpty Vertex) Vertex)
-- -- walkAt t keep ls0 r = go ls0
-- --   where
-- --     rightOf s l r = ccw l s r == CCW
-- --     singleton x = x :| []

-- --     go (l:|ls') = case NonEmpty.nonEmpty ls' of
-- --                     Nothing          -> pure $ SP (singleton l) l
-- --                     Just ls@(s :| _) -> do pl <- atTime t l
-- --                                            ps <- atTime t s
-- --                                            if rightOf ps pl r
-- --                                              then (\(SP xs x) -> SP (l <| xs) x) <$> go ls
-- --                                              else pure $ SP (singleton l) l

-- --------------------------------------------------------------------------------



-- runSimulation         :: (Ord r, Fractional r)
--                       => NonEmpty Vertex -> [Event r] -> [Event r] -> Merger r [Event r]
-- runSimulation h le re = undefined
-- -- FIXME: I think we will need to simulate the old events as well, so that means
-- -- we need fast O(1) access into the old list




-- -- type EventQueue = Map t

-- data SimEnv s r = SimEnv { pts   :: V.Vector (Point 3 r)
--                          , llist :: LList s
--                          }

-- type Simulation s r = ReaderT (SimEnv s r) (ST s)

-- getP   :: Vertex -> Simulation s r (Point 3 r)
-- getP i = (\v -> v V.! i) <$> asks pts

-- getPT     :: Num r => r -> Vertex -> Simulation s r (Point 2 r)
-- getPT t i = (\(Point3 x y z) -> Point2 x (z - t*y)) <$> getP i


-- neighbours   :: Vertex -> Simulation s r (Two Vertex)
-- neighbours i = do v        <- asks llist
--                   Cell p s <- fromJust <$> (lift $ MV.read v i)
--                   pure $ Two p s


-- --------------------------------------------------------------------------------

-- data Cell = Cell !Vertex
--                  !Vertex deriving (Show,Eq)


-- type LList s = MV.MVector s (Maybe Cell)

-- toLListN             :: Int -> NonEmpty (Vertex) -> ST s (LList s)
-- toLListN n l@(x:|xs) = do v <- MV.replicate n Nothing
--                           forM_ ys $ \(p,y,s) ->
--                             MV.write v y $ Just (Cell p s)
--                           pure v
--   where
--     ys = zip3' (x <| l) l (NonEmpty.fromList $ xs ++ [NonEmpty.last l])

--     zip3' (a :| as) (b:|bs) (c:|cs) = (a,b,c) :| List.zip3 as bs cs


-- -- | If this cell has a next cell, return it
-- getNext   :: Vertex -> Simulation s r (Maybe Vertex)
-- getNext i = do v <- asks llist
--                ms <- lift $ MV.read v i
--                pure $ ms >>= \(Cell _ s) -> if s /= i then Just s else Nothing

-- getPrev   :: Vertex -> Simulation s r (Maybe Vertex)
-- getPrev i = do v <- asks llist
--                mp <- lift $ MV.read v i
--                pure $ mp >>= \(Cell p _) -> if p /= i then Just p else Nothing

-- -- | 'insertAfter c y' inserts y after c in the linked list. Returns the
-- -- new cell containing y.
-- insertAfter     :: Vertex -> Vertex -> Simulation s r ()
-- insertAfter i j = do v <- asks llist
--                      Cell p s <- fromJust <$> (lift $ MV.read v i)
--                      let s' = if i /= s then s else j
--                      lift $ MV.write v j $ Just (Cell i s')
--                      lift $ MV.write v i $ Just (Cell p j)


-- data DeletionResult = Empty        -- ^ the list is now empty
--                     | Last !Vertex -- ^ pointer to the last element in the list
--                     | Succ !Vertex -- ^ pointer to the
--                       -- element that was the
--                                      -- the successor of the
--                                      -- element that we
--                                      -- removed.

-- delete   :: Vertex -> Simulation s r DeletionResult
-- delete i = do v  <- asks llist
--               mp <- getPrev i
--               ms <- getNext i
--               lift $ MV.write v i Nothing
--               case (mp,ms) of
--                 (Nothing,Nothing) -> pure Empty
--                 (Nothing,Just s)  -> do lift $ modify v s $ setPrev s
--                                         pure $ Succ s
--                 (Just p, Nothing) -> do lift $ modify v p $ setNext p
--                                         pure $ Last p
--                 (Just p, Just s)  -> do lift $ modify v s $ setPrev p
--                                         lift $ modify v p $ setNext s
--                                         pure $ Succ s
--   where
--     setPrev p = fmap (\(Cell _ s) -> Cell p s)
--     setNext s = fmap (\(Cell p _) -> Cell p s)

--     modify v i f = MV.modify v f i



----------------------------------------------------------------------------------

mModify   :: PrimMonad m => MV.MVector (PrimState m) a -> Maybe Int -> (a -> a) -> m ()
mModify v mi f = case mi of
                   Nothing -> pure ()
                   Just i  -> modify v i f

modify        :: PrimMonad m => MV.MVector (PrimState m) a -> Int -> (a -> a) -> m ()
modify v i f = MV.modify v f i

minimumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn f = \case
    [] -> Nothing
    xs -> Just $ List.minimumBy (comparing f) xs

--------------------------------------------------------------------------------


run      :: (Ord r, Fractional r, Show r) => NonEmpty (Point 3 r :+ p) -> MergeStatus r
run pts' = runST
                 $ runSimulation (Env pts <$> newLList n)
                 -- . (>>= output)
                 . divideAndConquer1 mkLeaf
                 $ NonEmpty.fromList [0..(n-1)]
  where
    n = V.length pts
    fromNonEmpty = V.fromList . NonEmpty.toList
    (pts,exts) = bimap V.fromList V.fromList . unExt . NonEmpty.sortWith (^.core.xCoord) $ pts'
    unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])
    newLList n = MV.new n


myPts :: NonEmpty (Point 3 Rational :+ Int)
myPts = NonEmpty.fromList $ [ Point3 1 1 0 :+ 1
                            , Point3 3 2 0 :+ 2
                            , Point3 5 5 0 :+ 3
                            , Point3 10 20 0 :+ 4
                            ]

test = run myPts

-- foo      :: NonEmpty (Point 3 Rational :+ Int) -> V.Vector (Point 3 Rational)
-- foo pts' = runST $ runSimulation (Env pts <$> MV.new 5) sim
--   where
--     (pts,exts) = traceShowId $ bimap V.fromList V.fromList . unExt . NonEmpty.sortWith (^.core.xCoord) $ pts'

--     unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])

--     sim :: Simulation s Rational (V.Vector (Point 3 Rational))
--     sim = traceShow "foo"$ asks (traceShowId . points . traceShow "going")
