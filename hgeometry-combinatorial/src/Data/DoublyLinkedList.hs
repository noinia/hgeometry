module Data.DoublyLinkedList where

import           Control.Monad.Reader
import           Control.Monad.ST
import qualified Data.List as List
import           Data.Maybe
import           Data.STRef
import           Prelude hiding (concat)

import           Debug.Trace

--------------------------------------------------------------------------------

-- | Cell in a doubly linked list
data Cell s a = Cell { _prev :: !(STRef s (Cell s a))
                     , _elem :: !a
                     , _next :: !(STRef s (Cell s a))
                     }

instance Show a => Show (Cell s a) where
  show (Cell _ x _) = "Cell " <> show x


type DLList s a = STRef s (Cell s a)



singleton   :: a -> ST s (DLList s a)
singleton x = do ref <- newSTRef undefined
                 let c = Cell ref x ref
                 writeSTRef ref c
                 pure ref

-- | If this cell has a next cell, return it
getNext              :: DLList s a -> ST s (Maybe (DLList s a))
getNext l = do Cell _ _ s  <- readSTRef l
               pure $ if l /= s then Just s else Nothing

-- | If this cell has a previous cell, return it
getPrev              :: DLList s a -> ST s (Maybe (DLList s a))
getPrev l = do Cell p  _ _ <- readSTRef l
               pure $ if l /= p then Just p else Nothing

-- | Test if this is the first cell in the list
isHead :: DLList s a -> ST s Bool
isHead = fmap isJust . getPrev

isLast :: DLList s a -> ST s Bool
isLast = fmap isJust . getNext

unfoldrM   :: Monad m => (b -> m (Maybe (a,b))) -> b -> m [a]
unfoldrM f = go
  where
    go i = f i >>= \case
             Nothing     -> pure []
             Just (x,j)  -> (x :) <$> go j


unfoldrM1   :: Monad m => (b -> m (a, Maybe b)) -> b -> m [a]
unfoldrM1 f = go
  where
    go i = f i >>= \case
             (x,Nothing) -> pure [x]
             (x,Just j)  -> (x:) <$> go j

-- | Get the list starting from the current cell
toListFrom :: DLList s a -> ST s [a]
toListFrom = unfoldrM1 (\l -> (\c -> (_elem c,)) <$> readSTRef l <*> getNext l)

-- | Get the elements before (and inclding me) in the list (in reverse
-- order).
beforeMe :: DLList s a -> ST s [a]
beforeMe = unfoldrM1 (\l -> (\c -> (_elem c,)) <$> readSTRef l <*> getPrev l)

toList   :: DLList s a -> ST s [a]
toList c = (\xs ys -> (reverse $ tail xs) <> ys) <$> beforeMe c <*> toListFrom c

-- | Convert a list into a doubly linked list. The result is a list,
-- with for every element a pointer to the list, focussed on that
-- element.
fromList    :: [a] -> ST s [DLList s a]
fromList xs = do refs <- mapM singleton xs
                 sequence_ $ zipWith concat refs (tail refs)
                 pure refs

-- | Make this the last element in the list
dropAfter   :: DLList s a -> ST s ()
dropAfter l = modifySTRef' l $ \(Cell p x _) -> Cell p x l

-- | Make this the first element in the list
dropBefore              :: DLList s a -> ST s ()
dropBefore l = modifySTRef' l $ \(Cell _ x s) -> Cell l x s

focus :: DLList s a -> ST s a
focus = fmap _elem . readSTRef

-- | 'insertAfter c y' inserts y after c in the linked list. Returns the
-- new cell containing y.
insertAfter                  :: DLList s a -> a -> ST s (DLList s a)
insertAfter rc y = do (Cell p x s) <- readSTRef rc
                      ms  <- getNext rc
                      ref <- newSTRef undefined
                      case ms of
                        Nothing  -> writeSTRef ref $ Cell rc y ref
                        Just _   -> writeSTRef ref $ Cell rc y s
                      let c' = Cell p x ref
                      writeSTRef rc  c'
                      pure ref

data DeletionResult s a = Empty              -- ^ the list is now empty
                        | Last !(DLList s a) -- ^ pointer to the last element in the list
                        | Succ !(DLList s a) -- ^ pointer to the
                                             -- element that was the
                                             -- the successor of the
                                             -- element that we
                                             -- removed.

-- deletes the current element from the list.
-- returns a pointer to the next  element in the list, if it exists
delete   :: DLList s a -> ST s (DeletionResult s a)
delete l = do mp <- getPrev l
              ms <- getNext l
              case (mp,ms) of
                (Nothing,Nothing) -> pure Empty
                (Nothing,Just s)  -> do modifySTRef' s $ \c -> c { _prev = s }
                                        pure $ Succ s
                (Just p, Nothing) -> do modifySTRef' p $ \c -> c { _next = p }
                                        pure $ Last p
                (Just p, Just s)  -> do modifySTRef' s $ \c -> c { _prev = p }
                                        modifySTRef' p $ \c -> c { _next = s }
                                        pure $ Succ s
  --TODO: should we reset the pointers of l?

-- | concat two lists a and b.
concat     :: DLList s a -> DLList s a -> ST s ()
concat a b = do modifySTRef' a $ \ca -> ca { _next = b }
                modifySTRef' b $ \cb -> cb { _prev = a }


--------------------------------------------------------------------------------

-- | For debugging purposes
mShow   :: Show a => DLList s a -> ST s [Char]
mShow l = do Cell p x s <- readSTRef l
             px <- focus p
             sx <- focus s
             pure $ mconcat [ "Cell "
                            , "(ref to: ", show px ,") "
                            , show x
                            , " (ref to: ", show sx, ")"
                            ]
