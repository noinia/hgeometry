module Algorithms.Geometry.ConvexHull.Types where

import Control.Lens (Lens', _1, _2)
import Control.Monad.State.Strict (StateT)
import Data.Ext
import Data.Geometry.Point
import Data.Geometry.Triangle
import Data.IndexedDoublyLinkedList
import Data.List.NonEmpty (NonEmpty)
import Data.Util

--------------------------------------------------------------------------------

-- type ConvexHull d p r = [Three Index]
type ConvexHull d p r = [Triangle 3 p r]


--------------------------------------------------------------------------------


type HullM s r = DLListMonad s (Point 3 r)


--------------------------------------------------------------------------------

-- TODO: Define bridge as a data type so we can add UNPACK pragmas
type Bridge = Two Index

pattern Bridge     :: Index -> Index -> Bridge
pattern Bridge a b = Two a b
{-# COMPLETE Bridge #-}

leftBridgePoint, rightBridgePoint :: Lens' Bridge Index
leftBridgePoint  = _1
rightBridgePoint = _2

--------------------------------------------------------------------------------

-- | The Kinetic Simulation
type Simulation s r = StateT Bridge (HullM s r)

type Existing a = Either a a

--------------------------------------------------------------------------------

type Event r = r :+ NonEmpty Action

eventTime :: Lens' (Event r) r
eventTime = core

eventActions :: Lens' (Event r) (NonEmpty Action)
eventActions = extra

-- data Event r = Event { eventTime :: !r
--                      , eventKind :: !Action
--                      } deriving (Show,Eq)

data Action = InsertAfter  {-# UNPACK #-} !Index {-# UNPACK #-} !Index
            -- ^ current Index first, then the Item we insert
            | InsertBefore {-# UNPACK #-} !Index {-# UNPACK #-} !Index
              -- ^ current Index first, then the Item we insert
            | Delete {-# UNPACK #-} !Index
            deriving (Show,Eq,Ord)

isDelete :: Action -> Bool
isDelete = \case
  Delete _         -> True
  InsertAfter _ _  -> False
  InsertBefore _ _ -> False


getRightMost :: Action -> Index
getRightMost = \case
  InsertAfter _ j  -> j
  InsertBefore j _ -> j
  Delete j         -> j

getLeftMost :: Action -> Index
getLeftMost = \case
  InsertAfter j _  -> j
  InsertBefore _ j -> j
  Delete j         -> j


getInsertedOrDeleted :: Action -> Index
getInsertedOrDeleted = \case
  InsertAfter _ j  -> j
  InsertBefore _ j -> j
  Delete j         -> j


data MergeStatus r = MergeStatus { hd     :: {-# UNPACK #-} !Index -- ^ first item in the list
                                 , lst    :: {-# UNPACK #-} !Index -- ^ last item in the list
                                 , events :: ![Event r] -- ^ Events when this Hull changes
                                 } deriving (Show,Eq)




--------------------------------------------------------------------------------
