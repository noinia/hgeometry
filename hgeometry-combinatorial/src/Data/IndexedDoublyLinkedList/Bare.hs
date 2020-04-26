module Data.IndexedDoublyLinkedList.Bare(
    IDLList(..)
  , Cell(..), emptyCell
  , IDLListMonad, runIDLListMonad
  , Index

  , singletons
  , writeList
  , getNext, getPrev
  , toListFrom, toListFromR, toListContains
  , toListFromK, toListFromRK
  , insertAfter, insertBefore
  , delete
  , dump
  ) where

import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Control.Monad.ST
import           Data.Foldable (forM_)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------

type Index = Int

-- | Cells in the Linked List
data Cell = Cell { prev :: !(Maybe Index)
                 , next :: !(Maybe Index)
                 } deriving (Show,Eq)

emptyCell :: Cell
emptyCell = Cell Nothing Nothing

-- | Doubly linked list implemented by a mutable vector. So actually
-- this data type can represent a collection of Linked Lists that can
-- efficiently be concatenated and split.
--
-- Supports O(1) indexing, and O(1) insertions, deletions
newtype IDLList s = IDLList { llist  :: MV.MVector s Cell }

--------------------------------------------------------------------------------

-- | Monad in which we can use the IndexedDoublyLinkedList.
newtype IDLListMonad s a = IDLListMonad { runIDLListMonad' :: ReaderT (IDLList s) (ST s) a }
                        deriving (Functor,Applicative,Monad)

instance PrimMonad (IDLListMonad s) where
  type PrimState (IDLListMonad s) = s
  primitive = IDLListMonad . primitive

instance MonadReader (IDLList s) (IDLListMonad s) where
  local f = IDLListMonad . local f . runIDLListMonad'
  ask = IDLListMonad $ ask

-- | Runs a DLList Computation, starting with n singleton values
runIDLListMonad        :: Int -> (forall s. IDLListMonad s a) -> a
runIDLListMonad n comp = runST $ singletons n >>= runReaderT (runIDLListMonad' comp)

----------------------------------------

-- | Constructs a new DoublyLinkedList, of size at most n
singletons   :: (PrimMonad m, s ~ PrimState m) => Int -> m (IDLList s)
singletons n = IDLList <$> MV.replicate n emptyCell

-- | Sets the DoublyLinkedList to the given List.
--
-- Indices that do not occur in the list are not touched.
writeList   :: NonEmpty Index -> IDLListMonad s ()
writeList h = do v <- asks llist
                 forM_ (withNeighs h) $ \(STR p i s) ->
                   modify v i $ \c -> c { prev = p , next = s }
  where
    withNeighs (x:|xs) = let l = x:xs
                         in zipWith3 STR (Nothing : map Just l) l (map Just xs ++ [Nothing])

----------------------------------------
-- * Queries

-- | Next element in the List
getNext   :: Index -> IDLListMonad s (Maybe Index)
getNext i = do v <- asks llist
               next <$> MV.read v i

-- | Previous Element in the List
getPrev   :: Index -> IDLListMonad s (Maybe Index)
getPrev i = do v <- asks llist
               prev <$> MV.read v i

-- | Computes a maximal length list starting from the Given index
--
-- running time: \(O(k)\), where \(k\) is the length of the output list
toListFrom   :: Index -> IDLListMonad s (NonEmpty Index)
toListFrom i = (i :|) <$> iterateM getNext i

-- | Takes the current element and its k next's
toListFromK     :: Index -> Int -> IDLListMonad s (NonEmpty Index)
toListFromK i k = (i :|) <$> replicateM k getNext i

-- | Computes a maximal length list by walking backwards in the
-- DoublyLinkedList, starting from the Given index
--
-- running time: \(O(k)\), where \(k\) is the length of the output list
toListFromR :: Index -> IDLListMonad s (NonEmpty Index)
toListFromR i = (i :|) <$> iterateM getPrev i

toListFromRK     :: Index -> Int -> IDLListMonad s (NonEmpty Index)
toListFromRK i k = (i :|) <$> replicateM k getPrev i

-- | Computes a maximal length list that contains the element i.
--
-- running time: \(O(k)\), where \(k\) is the length of the output
-- list
toListContains   :: Index -> IDLListMonad s (NonEmpty Index)
toListContains i = f <$> toListFromR i <*> toListFrom i
  where
    f l r = NonEmpty.fromList $ reverse (NonEmpty.toList l) <> NonEmpty.tail r


----------------------------------------
-- * Updates

-- | Inserts the second argument after the first one into the linked list
insertAfter     :: Index -> Index -> IDLListMonad s ()
insertAfter i j = do v  <- asks llist
                     mr <- getNext i
                     modify  v i  $ \c -> c { next = Just j }
                     modify  v j  $ \c -> c { prev = Just i , next = mr }
                     mModify v mr $ \c -> c { prev = Just j }

-- | Inserts the second argument before the first one into the linked list
insertBefore     :: Index -> Index -> IDLListMonad s ()
insertBefore i h = do v <- asks llist
                      ml <- getPrev i
                      mModify v ml $ \c -> c { next = Just h }
                      modify  v h  $ \c -> c { prev = ml , next = Just i }
                      modify  v i  $ \c -> c { prev = Just h }

-- | Deletes the element from the linked list. This element thus
-- essentially becomes a singleton list.
delete   :: Index -> IDLListMonad s ()
delete j = do v <- asks llist
              ml <- getPrev j
              mr <- getNext j
              modify  v j  $ \c -> c { prev = Nothing, next = Nothing }
              mModify v ml $ \c -> c { next = mr }
              mModify v mr $ \c -> c { prev = ml }

----------------------------------------
-- * Helper functions

-- | Applies the action at most n times.
replicateM     :: Monad m => Int -> (a -> m (Maybe a)) -> a -> m [a]
replicateM n f = go n
  where
    go 0 _ = pure []
    go k x = f x >>= \case
               Nothing -> pure []
               Just y  -> (y:) <$> go (k-1) y

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

-- | For debugging purposes, dump the values and the cells
dump :: IDLListMonad s (V.Vector Cell)
dump = do IDLList cs <- ask
          cs' <- V.freeze cs
          pure cs'
