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
import           Data.Foldable (forM_)
import           Data.Ord (comparing)
-- import qualified Data.DoublyLinkedList as DLList
-- import           Data.DoublyLinkedList (DLList)
import           Data.Maybe
import           Data.Ext
import           Data.Geometry.Point
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import Debug.Trace

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
                STR h u w <- findBridge (t esIn) l r
                es <- runKinetic l r h u w
                writeHull h
                pure $ MergeStatus (hd l) (lst r) u w es
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
                      let Two (SP u ls) (SP v rs) = findBridge' lh rh
                      pure $ STR (NonEmpty.fromList $ ls <> [u,v] <> rs) u v
  where
    atTime' t i = (:+ i) <$> atTime t i

findBridge'       :: (Ord r, Fractional r)
                  => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
                  -> Two (SP p [p])
findBridge' l0 r0 = bimap f f $ go l0 r0
  where
    f (SP x xs) = SP (x^.extra) ((^.extra) <$> xs)
    ne = NonEmpty.fromList
    isRight' []    _ _ = False
    isRight' (x:_) l r = ccw' l r x == CW

    go lh@(l:|ls) rh@(r:|rs) | isRight' rs l r = go lh      (ne rs)
                             | isRight' ls l r = go (ne ls) rh
                             | otherwise       = Two (SP l ls) (SP r rs)



    -- go lh@(l:|lrest@(sl:_)) rh@(r:|rrest@(sr:_)) | isRight sr l r = go lh (ne rrest)
    --                                              | isRight sl l r = go (ne lrest) rh
    --                                              | otherwise      = Two (SP l lrest) (SP r rrest)
    -- go lh@(l:|[])           rh@(r:|rrest@(sr:_)) | isRight sr l r = go lh (ne rrest)
    --                                              | otherwise      = Two (SP l []) (SP r rrest)

    -- go lh@(l:|lrest@(sl:_)) rh@(r:|[])           | isRight sl l r = go (ne lrest) rh
    --                                              | otherwise      = Two (SP l lrest) (SP r [])
    -- go (l:|[])              (r:|[])                               = Two (SP l []) (SP r [])


-- walkOutwards l (r:|rs) = case NonEmpty.nonEmpty rs of
--                            Nothing          -> SP r []
--                            Just rs'@(sr:|_) | rightTurn l r sr -> walkOutwards l rs'
--                                             | otherwise        -> SP r rs

--   where
--     isRightTurn l r sr = ccw' l r sr /= CCW


-- walkOutwards' r (l:|ls) = case NonEmpty.nonEmpty ls of
--                            Nothing -> SP l []
--                            Just ls'@(sl:|_) | rightTurn ls l r -> walkOutwards'












runKinetic           :: MergeStatus r
                     -> MergeStatus r
                     -> NonEmpty Index
                     -> Index -> Index
                     -> Simulation s r [Event r]
runKinetic l r h u w = pure []





-- -- | First event involving the bridge, if any. Also returns the new bridge pair
-- bridgeTime         :: (Ord r, Fractional r)
--                    => r -> Vertex -> Vertex
--                    -> Simulation s r (Maybe (Event r, Two Vertex))
-- bridgeTime now l r = do Two a b <- neighbours l
--                         Two u v <- neighbours r
--                         [pa,pl,pb,pu,pr,pv] <- mapM getP [a,l,b,u,r,v]

--                         let me    = minimumOn (\(t',_,_) -> t')
--                                       [ e | e@(t',_,_) <- cands, t' >= now]
--                             cands = [ (nextTime pa pl pr, Two a r, Delete l)
--                                     , (nextTime pl pb pr, Two b r, InsertAfter  l b)
--                                     , (nextTime pl pu pr, Two l u, InsertBefore u r)
--                                     , (nextTime pl pr pv, Two l v, Delete r)
--                                     ]
--                         pure $ (\(t,br,e) -> (Event t e, br)) <$> me




-- compute the time at which r becomes colinear with the line throuh p
-- and l.
nextTime  :: (Ord r, Fractional r) => Point 3 r -> Point 3 r -> Point 3 r -> r
nextTime (Point3 px py pz) (Point3 lx ly lz) (Point3 rx ry rz) = t
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



-- --------------------------------------------------------------------------------


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
