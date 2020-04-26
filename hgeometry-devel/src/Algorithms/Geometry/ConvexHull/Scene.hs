{-# LANGUAGE UndecidableInstances #-}
module Algorithms.Geometry.ConvexHull.Scene where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Properties
import qualified Data.IndexedDoublyLinkedList.Bare as IDLList
import qualified Data.Map as Map
import           Prelude hiding (Either(..))

--------------------------------------------------------------------------------

-- type Scene p = [p]

type family Scene p

type instance Scene (Point 3 r)      = Map.Map r (Point 3 r)
type instance Scene (Point 3 r :+ p) = Map.Map r (Point 3 r :+ p)


newtype P s = P IDLList.Index deriving (Show,Eq)
type instance Scene (P s) = IDLList.IDLListMonad s ()

class HasScene p where
  insertBefore       :: p -> p -> Scene p -> Scene p
  insertAfter        :: p -> p -> Scene p -> Scene p
  delete             :: p -> Scene p -> Scene p
  singleton :: p -> Scene p

class HasNeighbours p where
  getPrev :: p -> Scene p -> Maybe p
  getNext :: p -> Scene p -> Maybe p


class AsAPoint p where
  asAPoint :: p -> Point 3 (NumType p)
instance AsAPoint (Point 3 r) where
  asAPoint = id
instance AsAPoint (Point 3 r :+ p) where
  asAPoint = view core


instance Ord r => HasScene (Point 3 r :+ p) where
  insertBefore _ p = Map.insert (p^.core.xCoord) p -- we just ingore information about
  insertAfter  _ p = Map.insert (p^.core.xCoord) p -- possible neighbours
  delete         p = Map.delete (p^.core.xCoord)
  singleton      p = Map.singleton (p^.core.xCoord) p

instance Ord r => HasNeighbours (Point 3 r :+ p) where
  getPrev p = fmap snd . Map.lookupLT (p^.core.xCoord)
  getNext p = fmap snd . Map.lookupGT (p^.core.xCoord)


-- instance HasScene (P s) where
--   insertBefore (P p) (P x) s = s >> IDLList.insertBefore p x


-- class CanApply p where
--   applyAction :: CanAquire f => Scene p -> f (Action p) -> Scene p

-- instance Eq p => CanApply p where
--   applyAction s fa = case acquire fa of
--                      InsertBefore p x -> insertBefore p x s
--                      InsertAfter  p x -> insertAfter  p x s
--                      Delete x         -> delete x s
