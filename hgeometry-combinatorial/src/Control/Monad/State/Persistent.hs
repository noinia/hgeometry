--------------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State.Persistent
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Control.Monad.State.Persistent( PersistentStateT
                                     , PersistentState
                                     , store
                                     , runPersistentStateT
                                     , runPersistentState
                                     ) where

import Control.Monad.State
import Control.Monad.Identity(Identity(..))
import Data.List.NonEmpty(NonEmpty(..),(<|),toList)

--------------------------------------------------------------------------------

-- | A State monad that can store earlier versions of the state.
newtype PersistentStateT s m a =
  PersistentStateT (StateT (NonEmpty s) m a)
  deriving (Functor,Applicative,Monad)
           -- We store all the versions in reverse order

-- | Create a snapshot of the current state and add it to the list of states
-- that we store.
store :: Monad m => PersistentStateT s m ()
store = PersistentStateT $ do
  ss@(s :| _) <- get
  put (s <| ss)


instance Monad m => MonadState s (PersistentStateT s m) where
  state f = PersistentStateT $ do
              (s :| os) <- get
              let (x,s') = f s
              put (s' :| os)
              return x

-- | Run a persistentStateT, returns a triplet with the value, the last state
-- and a list of all states (including the last one) in chronological order
runPersistentStateT :: Functor m => PersistentStateT s m a -> s -> m (a,s,[s])
runPersistentStateT (PersistentStateT act) initS = f <$> runStateT act (initS :| [])
  where
    f (x,ss@(s :| _)) = (x, s, reverse $ toList ss)


--------------------------------------------------------------------------------

-- | A State monad that can store earlier versions of the state.
type PersistentState s = PersistentStateT s Identity

-- | Run a persistentStateT, returns a triplet with the value, the last state
-- and a list of all states (including the last one) in chronological order
runPersistentState     :: PersistentState s a -> s -> (a,s,[s])
runPersistentState act = runIdentity . runPersistentStateT act
