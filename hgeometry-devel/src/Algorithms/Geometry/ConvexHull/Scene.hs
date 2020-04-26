{-# LANGUAGE FunctionalDependencies #-}
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

class HasScene p where
  insertBefore       :: p -> p -> Scene p -> Scene p
  insertAfter        :: p -> p -> Scene p -> Scene p
  delete             :: p -> Scene p -> Scene p
  singleton :: p -> Scene p

class HasNeighbours p where
  getPrev :: p -> Scene p -> Maybe p
  getNext :: p -> Scene p -> Maybe p

class AsPoint p r | p -> r where
  asPoint :: (Ord r, Fractional r) => p -> Point 3 r


type instance Scene (Point 3 r)      = Map.Map r (Point 3 r)
type instance Scene (Point 3 r :+ p) = Map.Map r (Point 3 r :+ p)

--------------------------------------------------------------------------------

instance AsPoint (Point 3 r) r where
  asPoint = id
instance AsPoint (Point 3 r :+ p) r where
  asPoint = view core

instance Ord r => HasScene (Point 3 r :+ p) where
  insertBefore _ p = Map.insert (p^.core.xCoord) p -- we just ingore information about
  insertAfter  _ p = Map.insert (p^.core.xCoord) p -- possible neighbours
  delete         p = Map.delete (p^.core.xCoord)
  singleton      p = Map.singleton (p^.core.xCoord) p

instance Ord r => HasNeighbours (Point 3 r :+ p) where
  getPrev p = fmap snd . Map.lookupLT (p^.core.xCoord)
  getNext p = fmap snd . Map.lookupGT (p^.core.xCoord)


--------------------------------------------------------------------------------
-- * The fast monadic Implementation

newtype P s r = P IDLList.Index deriving (Show,Eq)
newtype T s t = T t  deriving (Show,Eq,Ord,Num,Fractional)

type instance NumType (P s r) = T s r

newtype SceneM s r a = SceneM { runSceneM :: IDLList.IDLListMonad s a }
                     deriving (Functor,Applicative,Monad)

type instance Scene (P s r) = SceneM s r ()


-- insertBefore' :: forall s r. P s r -> P s r -> SceneM s r () -> SceneM s r ()
-- insertBefore' (P p) (P x) s = s >> IDLList.insertBefore p x

-- instance HasScene (P s r) where
  -- insertBefore (P p) (P q) s = s >> (SceneM $ insertBefore p q)


-- class CanApply p where
--   applyAction :: CanAquire f => Scene p -> f (Action p) -> Scene p

-- instance Eq p => CanApply p where
--   applyAction s fa = case acquire fa of
--                      InsertBefore p x -> insertBefore p x s
--                      InsertAfter  p x -> insertAfter  p x s
--                      Delete x         -> delete x s
