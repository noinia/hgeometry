module Algorithms.Geometry.ConvexHull.Types where

import           Control.Lens (Lens', _1, _2)
import           Control.Monad.State.Strict (StateT)
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Triangle
import           Data.IndexedDoublyLinkedList
import           Data.Util

--------------------------------------------------------------------------------

-- type ConvexHull d p r = [Three Index]
type ConvexHull d p r = [Triangle 3 p r]


--------------------------------------------------------------------------------


type HullM s r = DLListMonad s (Point 3 r)


--------------------------------------------------------------------------------

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

type Event' r  = r :+ EventKind
type MEvent' r = r :+ [EventKind]


type Handler s r = Simulation s r [Event r]

-- type BridgeEvent = SP Bridge EventKind


type Existing a = Either a a

--------------------------------------------------------------------------------

data Event r = Event { eventTime :: !r
                     , eventKind :: !EventKind
                     } deriving (Show,Eq)

data EventKind = InsertAfter  !Index !Index -- ^ current Index first, then the Item we insert
               | InsertBefore !Index !Index -- ^ current Index first, then the Item we insert
               | Delete !Index
               deriving (Show,Eq,Ord)

getRightMost :: EventKind -> Index
getRightMost = \case
  InsertAfter _ j  -> j
  InsertBefore _ j -> j
  Delete j         -> j

getLeftMost :: EventKind -> Index
getLeftMost = \case
  InsertAfter j _  -> j
  InsertBefore j _ -> j
  Delete j         -> j


data MergeStatus r = MergeStatus { hd     :: !Index -- ^ first item in the list
                                 , lst    :: !Index -- ^ last item in the list
                                 , events :: ![Event r] -- ^ Events when this Hull changes
                                 } deriving (Show,Eq)



--------------------------------------------------------------------------------
