{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
module Data.IndexedDoublyLinkedList.Unboxed -- ( DLList(..)
                                   -- , Cell(..), emptyCell
                                   -- , DLListMonad, runDLListMonad
                                   -- , Index

                                   -- , singletons
                                   -- , writeList
                                   -- , valueAt, getNext, getPrev
                                   -- , toListFrom, toListFromR, toListContains
                                   -- , insertAfter, insertBefore
                                   -- , delete
                                   -- , dump
                                   -- )
where

import           Control.Monad.Primitive (PrimMonad(..))
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Control.Monad.ST
import           Data.Foldable (forM_)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV

--------------------------------------------------------------------------------

type Index = Int

-- TODO: Switch to unobxed sums for these!

type MaybeIndex = (# (# #)  | Index #)

pattern NoIndex :: MaybeIndex
pattern NoIndex = (# (# #) |        #)

pattern JustIndex   :: Index -> MaybeIndex
pattern JustIndex i = (# | i #)

-- | Cells in the Linked List
data Cell = Cell { prev :: MaybeIndex, next :: MaybeIndex }
-- instance Show Cell where
--   show Cell p n =

emptyCell :: Cell
emptyCell = Cell NoIndex NoIndex

-- instance UMV.Unbox Cell

-- newtype instance UV.Vector Cell = VCell (Vector (MaybeIndex,MaybeIndex))

-- instance V.Vector UV.Vector Cell where


-- | Doubly linked list implemented by a mutable vector. So actually
-- this data type can represent a collection of Linked Lists that can
-- efficiently be concatenated and split.
--
-- Supports O(1) indexing, and O(1) insertions, deletions
newtype DLList s = DLList { llist  :: (UMV.MVector s Cell) }


-- instance Functor (DLList s) where
--   fmap f (DLList v l) = DLList (fmap f v) l


-- --------------------------------------------------------------------------------

-- -- | Monad in which we can use the IndexedDoublyLinkedList.
-- newtype DLListMonad s a = DLListMonad { runDLListMonad' :: ReaderT (DLList s) (ST s) a }
--                           deriving (Functor,Applicative,Monad)

-- instance PrimMonad (DLListMonad s) where
--   type PrimState (DLListMonad s) = s
--   primitive = DLListMonad . primitive

-- instance MonadReader (DLList s) (DLListMonad s) where
--   local f = DLListMonad . local f . runDLListMonad'
--   ask = DLListMonad $ ask

-- -- | Runs a DLList Computation, starting with singleton values, crated
-- -- from the input vector.
-- runDLListMonad         :: V.Vector b -> (forall s. DLListMonad s b a) -> a
-- runDLListMonad vs comp = runST $ singletons vs >>= runReaderT (runDLListMonad' comp)

-- ----------------------------------------

-- -- | Constructs a new DoublyLinkedList. Every element is its own singleton list
-- singletons    :: (PrimMonad m, s ~ PrimState m) => V.Vector b -> m (DLList s b)
-- singletons vs = DLList vs <$> MV.replicate (V.length vs) emptyCell

-- -- | Sets the DoublyLinkedList to the given List.
-- --
-- -- Indices that do not occur in the list are not touched.
-- writeList   :: NonEmpty Index -> DLListMonad s b ()
-- writeList h = do v <- asks llist
--                  forM_ (withNeighs h) $ \(STR p i s) ->
--                    modify v i $ \c -> c { prev = p , next = s }
--   where
--     withNeighs (x:|xs) = let l = x:xs
--                          in zipWith3 STR (Nothing : map Just l) l (map Just xs ++ [Nothing])

-- ----------------------------------------
-- -- * Queries

-- -- | Gets the value at Index i
-- valueAt    :: Index -> DLListMonad s b b
-- valueAt  i = asks ((V.! i) . values)

-- | Next element in the List
-- getNext   :: Index -> DLListMonad s b MaybeIndex

-- getNext   :: PrimMonad m => Index -> m MaybeIndex
getNext i = do v <- asks llist
               x <- UMV.read v i
               pure x
               -- next <$> UMV.read v i

-- -- | Previous Element in the List
-- getPrev   :: Index -> DLListMonad s b (MaybeIndex)
-- getPrev i = do v <- asks llist
--                prev <$> MV.read v i

-- -- | Computes a maximal length list starting from the Given index
-- --
-- -- running time: \(O(k)\), where \(k\) is the length of the output list
-- toListFrom   :: Index -> DLListMonad s b (NonEmpty Index)
-- toListFrom i = (i :|) <$> iterateM getNext i

-- -- | Computes a maximal length list by walking backwards in the
-- -- DoublyLinkedList, starting from the Given index
-- --
-- -- running time: \(O(k)\), where \(k\) is the length of the output list
-- toListFromR :: Index -> DLListMonad s b (NonEmpty Index)
-- toListFromR i = (i :|) <$> iterateM getPrev i

-- -- | Computes a maximal length list that contains the element i.
-- --
-- -- running time: \(O(k)\), where \(k\) is the length of the output
-- -- list
-- toListContains   :: Index -> DLListMonad s b (NonEmpty Index)
-- toListContains i = f <$> toListFromR i <*> toListFrom i
--   where
--     f l r = NonEmpty.fromList $ reverse (NonEmpty.toList l) <> NonEmpty.tail r


-- ----------------------------------------
-- -- * Updates

-- -- | Inserts the second argument after the first one into the linked list
-- insertAfter     :: Index -> Index -> DLListMonad s b ()
-- insertAfter i j = do v  <- asks llist
--                      mr <- getNext i
--                      modify  v i  $ \c -> c { next = Just j }
--                      modify  v j  $ \c -> c { prev = Just i , next = mr }
--                      mModify v mr $ \c -> c { prev = Just j }

-- -- | Inserts the second argument before the first one into the linked list
-- insertBefore     :: Index -> Index -> DLListMonad s b ()
-- insertBefore i h = do v <- asks llist
--                       ml <- getPrev i
--                       mModify v ml $ \c -> c { next = Just h }
--                       modify  v h  $ \c -> c { prev = ml , next = Just i }
--                       modify  v i  $ \c -> c { prev = Just h }

-- -- | Deletes the element from the linked list. This element thus
-- -- essentially becomes a singleton list.
-- delete   :: Index -> DLListMonad s b ()
-- delete j = do v <- asks llist
--               ml <- getPrev j
--               mr <- getNext j
--               modify  v j  $ \c -> c { prev = Nothing, next = Nothing }
--               mModify v ml $ \c -> c { next = mr }
--               mModify v mr $ \c -> c { prev = ml }

-- ----------------------------------------
-- -- * Helper functions

-- iterateM  :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
-- iterateM f = go
--   where
--     go x = f x >>= \case
--              Nothing -> pure []
--              Just y  -> (y:) <$> go y

-- mModify   :: PrimMonad m => MV.MVector (PrimState m) a -> Maybe Int -> (a -> a) -> m ()
-- mModify v mi f = case mi of
--                    Nothing -> pure ()
--                    Just i  -> modify v i f

-- modify        :: PrimMonad m => MV.MVector (PrimState m) a -> Int -> (a -> a) -> m ()
-- modify v i f = MV.modify v f i


-- --------------------------------------------------------------------------------

-- -- | For debugging purposes, dump the values and the cells
-- dump :: DLListMonad s a (V.Vector a, V.Vector Cell)
-- dump = do DLList v cs <- ask
--           cs' <- V.freeze cs
--           pure (v,cs')
