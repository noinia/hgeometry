module Algorithms.Geometry.ConvexHull.Helpers where

import           Algorithms.Geometry.ConvexHull.Types
import           Control.Applicative ((<|>))
import           Control.Lens (view)
import           Control.Monad.Trans
import           Data.Foldable (toList)
import           Data.Geometry.Point
import           Data.IndexedDoublyLinkedList
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import           Data.Semigroup.Foldable


import           Debug.Trace

----------------------------------------------------------------------------------
-- * Convienience Functions in the Hull Monad.

pointAt :: Index -> HullM s r (Point 3 r)
pointAt = valueAt

pointAt' :: Index -> Simulation s r (Point 3 r)
pointAt' = lift . pointAt

atTime     :: Num r => r -> Index -> HullM s r (Point 2 r)
atTime t i = atTime' t <$> pointAt i

-- | Computes the position of the given point at time t
atTime'                  :: Num r => r -> Point 3 r -> Point 2 r
atTime' t (Point3 x y z) = Point2 x (z - t*y)


-- | Applies the actual event, mutating the current representation of
-- the hulls.  returns an element that is on the hull (after applying
-- the operation).
applyEvent' :: Action -> HullM s r Index
applyEvent' = \case
  InsertAfter i j  -> j  <$ insertAfter i j
  InsertBefore i h -> h <$ insertBefore i h >> pure h
  Delete j         -> f <$> delete j
  where
    f (ma,mb) = fromMaybe (error "hull is now empty!?") (ma <|> mb)


applyEvent :: Event r -> HullM s r Index
applyEvent = fmap NonEmpty.last . mapM applyEvent' . view eventActions

--------------------------------------------------------------------------------
-- * Generic Helpers


groupOn   :: Eq b => (a -> b) -> [a] -> [NonEmpty a]
groupOn f = map fromL . List.groupBy (\a b -> f a == f b)
  where
    fromL xs = case NonEmpty.nonEmpty xs of
                 Nothing -> error "groupOn"
                 Just n   -> n



maximumOn1   :: (Foldable1 f, Ord b) => (a -> b) -> f a -> a
maximumOn1 f = List.maximumBy (comparing f) . toList

minimum' :: Ord a => [a] -> Maybe a
minimum' = minimumOn id

minimumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn f = \case
    [] -> Nothing
    xs -> Just $ List.minimumBy (comparing f) xs



takeWhileM   :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM p = go
  where
    go = \case
      []      -> pure []
      (x:xs) -> do b <- p x
                   if b then (x:) <$> go xs else pure []
