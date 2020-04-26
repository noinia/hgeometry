module Algorithms.Geometry.ConvexHull.Movie where

import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.ConvexHull.Helpers (groupOn)
import           Control.Lens
import           Data.Ext
import qualified Data.List as List
import           Data.List.Alternating
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Ord (comparing)
import           Data.Semigroup
import           Prelude hiding (Either(..))
import           Algorithms.Geometry.ConvexHull.Scene

--------------------------------------------------------------------------------

data Movie' f p t = Movie { initialScene :: Scene p
                          , events       :: [Event' f p t]
                          }
type Movie p t = Movie' Identity p t

----------------------------------------

-- event happens at time t, and at the time of an event some non-empty
-- list of actions happens.  all these events are wrapped in an f.
type Event' f p t = t :+ NonEmpty (f (Action p))
type Event p t = Event' Identity p t

eventTime :: Lens (Event' f p t) (Event' f p t') t t'
eventTime = core

actions :: Lens (Event' f p t) (Event' g q t) (NonEmpty (f (Action p))) (NonEmpty (g (Action q)))
actions = extra

----------------------------------------

data Action p = InsertAfter p p  -- ^ current Index first, then the Item we insert
              | InsertBefore p p   -- ^ current Index first, then the Item we insert
              | Delete p
              deriving (Show,Eq,Ord)

----------------------------------------

data Existing a = Left a | Right a deriving (Show,Eq,Ord,Functor)

class CanAquire f where
  acquire :: f a -> a

instance CanAquire Identity where
  acquire = runIdentity
instance CanAquire Existing where
  acquire = \case
    Left x  -> x
    Right x -> x
--------------------------------------------------------------------------------


-- | plays two movies side by side
sideBySide                           :: (Ord t, Semigroup (Scene p))
                                     => Movie p t -> Movie p t -> Movie' Existing p t
sideBySide (Movie l el) (Movie r er) = Movie (l <> r) (mergeEvents el er)

mergeEvents       :: Ord t
                  => [Event p t] -> [Event p t] -> [Event' Existing p t]
mergeEvents ls rs = map combine . groupOn (^.eventTime)
                  $ mergeSortedListsBy (comparing (^.eventTime)) (wrap Left ls)
                                                                 (wrap Right rs)
  where
    wrap f = let f' (Identity x) = f x
             in map (&actions %~ \acts -> f' <$> acts)
    combine ((t:+as):|es) = t :+ (sconcat $ as :| map (^.actions) es)


-- | Playing a movie produces an alternating list of scenes and events
-- at which the scene changes.
play               :: (CanAquire f, HasScene p)
                   => Movie' f p t -> Alternating (Scene p) (Event' f p t)
play (Movie h0 es) = Alternating h0 (view extra $ List.foldl' applyEvent (h0 :+ []) es)



applyEvent             :: (CanAquire f, HasScene p)
                       => Scene p :+ [Event' f p t :+ Scene p]
                       -> Event' f p t
                       -> Scene p :+ [Event' f p t :+ Scene p]
applyEvent (s :+ xs) e = let s' = applyActions (e^.actions) s
                         in s' :+ (e :+ s'):xs

applyActions        :: (CanAquire f, HasScene p) => NonEmpty (f (Action p)) -> Scene p -> Scene p
applyActions acts s = List.foldl' applyAction s acts

applyAction      :: (CanAquire f, HasScene p) => Scene p -> f (Action p) -> Scene p
applyAction s fa = case acquire fa of
                     InsertBefore p x -> insertBefore p x s
                     InsertAfter  p x -> insertAfter  p x s
                     Delete x         -> delete x s

--------------------------------------------------------------------------------

currentScene                   :: Alternating a b -> a
currentScene (Alternating x _) = x
