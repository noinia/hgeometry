{-# LANGUAGE UndecidableInstances #-}
module Algorithms.Geometry.ConvexHull.Movie where

import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.ConvexHull.Helpers (groupOn)
import           Algorithms.Geometry.ConvexHull.Scene
import           Control.Lens
import           Data.Ext
import qualified Data.List as List
import           Data.List.Alternating(Alternating(..))
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Ord (comparing)
import           Data.Semigroup
import           Data.Util
import           Prelude hiding (Either(..))

import Debug.Trace
--------------------------------------------------------------------------------


data Movie' f g p t = Movie { allPoints     :: NonEmpty p
                            , initialScene' :: f (Scene p)
                            , events        :: [Event' g p t]
                            }
type Movie p t = Movie' Identity Identity p t

deriving instance (Show (f (Scene p)), Show (Event' g p t), Show p) => Show (Movie' f g p t)



movie          ::  NonEmpty p -> Scene p -> [Event' g p t] -> Movie' Identity g p t
movie pts s es = Movie pts (Identity s) es

initialScene :: Movie' Identity g p t -> Scene p
initialScene = runIdentity . initialScene'


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
              | Replace p p -- ^ old one first, then new one
              deriving (Show,Eq,Ord)

leftMost :: Action p -> p
leftMost = \case
  InsertAfter l _  -> l
  InsertBefore _ l -> l
  Delete l         -> l
  Replace _ p      -> p

rightMost :: Action p -> p
rightMost = \case
  InsertAfter _ r  -> r
  InsertBefore r _ -> r
  Delete r         -> r
  Replace _ p      -> p


isADelete :: Action p -> Bool
isADelete = \case
  Delete _ -> True
  _        -> False

isAReplace :: Action p -> Bool
isAReplace = \case
  Replace _ _ -> True
  _           -> False

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
sideBySide                           :: Ord t
                                     => Movie p t -> Movie p t -> Movie' Two Existing p t
sideBySide (Movie pl l el) (Movie pr r er) = Movie (pl <> pr)
                                                   (Two (runIdentity l) (runIdentity r))
                                                   (mergeEvents el er)

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
play               :: (CanApply f g, HasScene p)
                   => Movie' f g p t -> Alternating (f (Scene p)) (Event' g p t)
play (Movie _ h0 es) = Alternating h0
                                   (reverse . view extra $ List.foldl' applyEvent (h0 :+ []) es)
                                   -- TODO: make this into a DList

class CanApply f g where
  applyAction :: (HasScene p, Show p) => f (Scene p) -> g (Action p) -> f (Scene p)

instance CanApply Identity Identity where
  applyAction (Identity s) (Identity a) = Identity $ applyAction' s a

instance CanApply Two Existing where
  applyAction (Two sl sr) = \case
    Left a  -> Two (applyAction' sl a) sr
    Right a -> Two sl (applyAction' sr a)


applyEvent             :: (CanApply f g, HasScene p)
                       => f (Scene p) :+ [Event' g p t :+ f (Scene p)]
                       -> Event' g p t
                       -> f (Scene p) :+ [Event' g p t :+ f (Scene p)]
applyEvent (s :+ xs) e = let s' = applyActions (e^.actions) s
                         in s' :+ (e :+ s'):xs
                         -- we want to snoc instead of cons here

applyActions        :: (CanApply f g, HasScene p)
                    => NonEmpty (g (Action p)) -> f (Scene p) -> f (Scene p)
applyActions acts s = List.foldl' applyAction s acts

applyAction'      :: (Show p, HasScene p) => Scene p -> Action p -> Scene p
applyAction' s = \case
    InsertBefore p x -> insertBefore p x s
    InsertAfter  p x -> insertAfter  p x s
    Delete x         -> delete x s
    Replace      p x -> replace p x s

--------------------------------------------------------------------------------

currentScene                   :: Alternating a b -> a
currentScene (Alternating x _) = x
