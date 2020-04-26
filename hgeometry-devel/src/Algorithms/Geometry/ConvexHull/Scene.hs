{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Algorithms.Geometry.ConvexHull.Scene where

import           Control.Lens
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.Point
import           Data.Geometry.Properties
import qualified Data.IndexedDoublyLinkedList.Bare as IDLList
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Ord (comparing)
import           Prelude hiding (Either(..))

--------------------------------------------------------------------------------

-- type Scene p = [p]

type family Scene p = result | result -> p

class HasScene p where
  insertBefore       :: p -> p -> Scene p -> Scene p
  insertAfter        :: p -> p -> Scene p -> Scene p
  delete             :: p -> Scene p -> Scene p
  singleton          :: p -> Scene p

  fromNonEmpty       :: NonEmpty p -> Scene p

class HasNeighbours p where
  getPrev :: p -> Scene p -> Maybe p
  getNext :: p -> Scene p -> Maybe p

  -- gets all elements in the scene in increasing order, starting at the leftmost one
  fromLeftMost  :: Scene p -> NonEmpty p
  -- gets all elements in the scene in decreasing order, starting at the rightmost one
  fromRightMost :: Scene p -> NonEmpty p

class AsPoint p r | p -> r where
  asPoint :: p -> Point 3 r

class WithExtra p q where
  askExtra :: p -> q

-- type instance Scene (Point 3 r)      = Map.Map r (Point 3 r)
-- type instance Scene (Point 3 r :+ p) = Map.Map r (Point 3 r :+ p)
type instance Scene (PExt p r) = Map.Map r (PExt p r)

--------------------------------------------------------------------------------

instance AsPoint (Point 3 r) r where
  asPoint = id

newtype PExt p r = PExt (Point 3 r :+ p) deriving (Show)

unPExt :: PExt p r -> Point 3 r :+ p
unPExt (PExt x) = x

xC          :: PExt p r -> r
xC (PExt p) = p^.core.xCoord

instance Eq r => Eq (PExt p r) where
  p == q = xC p == xC q
instance Ord r => Ord (PExt p r) where
  p `compare` q = comparing xC p q


instance AsPoint (PExt p r) r where
  asPoint = view core . unPExt
instance WithExtra (PExt p r) p where
  askExtra = view extra . unPExt

instance Ord r => HasScene (PExt p r) where
  insertBefore _ p = Map.insert (xC p) p -- we just ingore information about
  insertAfter  _ p = Map.insert (xC p) p -- possible neighbours
  delete         p = Map.delete (xC p)
  singleton      p = Map.singleton (xC p) p

  fromNonEmpty ps  = Map.fromAscList [(xC p,p) | p <- toList ps]

instance Ord r => HasNeighbours (PExt p r) where
  getPrev p = fmap snd . Map.lookupLT (xC p)
  getNext p = fmap snd . Map.lookupGT (xC p)

  fromLeftMost  = NonEmpty.fromList . map snd . Map.toAscList
  fromRightMost = NonEmpty.fromList . map snd . Map.toDescList

--------------------------------------------------------------------------------
-- * The fast monadic Implementation

newtype P s r = P IDLList.Index deriving (Show,Eq)
newtype T s t = T t             deriving (Show,Eq,Ord,Num,Fractional)

-- type instance NumType (P s r) = T s r

newtype SceneM s r a = SceneM { runSceneM :: IDLList.IDLListMonad s a }
                     deriving (Functor,Applicative,Monad)

type instance Scene (P s r) = SceneM s r ()

-- insertBefore'               :: P s r -> P s r -> a -> Scene (P s r)
-- insertBefore' (P p) (P x) _ = SceneM (IDLList.insertBefore p x)

-- instance HasScene (P s r) where
--   insertBefore = insertBefore'


-- class CanApply p where
--   applyAction :: CanAquire f => Scene p -> f (Action p) -> Scene p

-- instance Eq p => CanApply p where
--   applyAction s fa = case acquire fa of
--                      InsertBefore p x -> insertBefore p x s
--                      InsertAfter  p x -> insertAfter  p x s
--                      Delete x         -> delete x s
