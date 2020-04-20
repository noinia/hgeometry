{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.ConvexHull.Movie where

import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.ConvexHull.Helpers (groupOn)
import           Control.Lens
import           Control.Monad.State.Class (get, put)
import           Control.Monad.State.Strict (evalStateT)
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Coerce
import           Data.Ext
import           Data.Functor.Identity
import           Data.Geometry.Point
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Ord (comparing)
import           Data.Semigroup
import           Data.Sequence (Seq(..))
import           Data.Util
import           Prelude hiding (Either(..))

--------------------------------------------------------------------------------

type Scene p = Seq p

-- event happens at time t, and at the time of an event some non-empty
-- list of actions happens.  all these events are wrapped in an f.
type Event' f p t = t :+ NonEmpty (f (Action p))
type Event p t = Event' Identity p t

eventTime :: Lens (Event' f p t) (Event' f p t') t t'
eventTime = core

actions :: Lens (Event' f p t) (Event' g q t) (NonEmpty (f (Action p))) (NonEmpty (g (Action q)))
actions = extra


data Existing a = Left a | Right a deriving (Show,Eq,Ord,Functor)

-- newtype Bridge a = Bridge a deriving (Show,Eq,Ord)

data Action p = InsertAfter p p  -- ^ current Index first, then the Item we insert
              | InsertBefore p p   -- ^ current Index first, then the Item we insert
              | Delete p
              deriving (Show,Eq,Ord)



data Movie' f p t = Movie { _initialScene :: Scene p
                          , _events       :: [Event' f p t]
                          }
type Movie p t = Movie' Identity p t

makeLenses ''Movie'


-- | plays two movies side by side
sideBySide                           :: Ord t => Movie p t -> Movie p t -> Movie' Existing p t
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


data Alternating a b = Alternating a [b :+ a] deriving (Show,Eq,Ord)

instance Bifunctor Alternating where
  bimap = bimapDefault
instance Bifoldable Alternating where
  bifoldMap = bifoldMapDefault
instance Bitraversable Alternating where
  bitraverse f g (Alternating a xs) = Alternating <$> f a <*> traverse (bitraverse g f) xs



currentScene                   :: Alternating a b -> a
currentScene (Alternating x _) = x


play               :: CanAquire f => Movie' f p t -> Alternating (Scene p) (Event' f p t)
play (Movie h0 es) = Alternating h0 (view extra $ List.foldl' applyEvent (h0 :+ []) es)

applyEvent             :: CanAquire f
                       => Scene p :+ [Event' f p t :+ Scene p]
                       -> Event' f p t
                       -> Scene p :+ [Event' f p t :+ Scene p]
applyEvent (s :+ xs) e = let s' = applyActions (e^.actions) s
                         in s' :+ (e :+ s'):xs

class CanAquire f where
  acquire :: f a -> a

instance CanAquire Identity where
  acquire = runIdentity
instance CanAquire Existing where
  acquire = \case
    Left x  -> x
    Right x -> x

applyActions        :: CanAquire f => NonEmpty (f (Action p)) -> Scene p -> Scene p
applyActions acts s = List.foldl' applyAction s acts

applyAction      :: CanAquire f => Scene p -> f (Action p) -> Scene p
applyAction s fa = case acquire fa of
                     InsertBefore p x -> insertBefore p x s
                     InsertAfter  p x -> insertAfter  p x s
                     Delete x         -> delete x s

insertBefore p x s = undefined
insertAfter  p x s = undefined
delete         x s = undefined

--------------------------------------------------------------------------------

type Bridge p = Two p
pattern Bridge u v = Two u v
{-# COMPLETE Bridge #-}

initialBridge :: Scene p -> Scene p -> SP (Bridge p) (Scene p)
initialBridge = undefined


merge     :: Ord t => Movie p t -> Movie p t -> Movie p t
merge l r = let anim   = play $ l `sideBySide` r
                SP b h = initialBridge (l^.initialScene) (r^.initialScene)
            in Movie h $ traceBridge b anim

traceBridge      :: Ord t
                 => Bridge p
                 -> Alternating (Scene p) (Event' Existing p t)
                 -> [Event p t]
traceBridge b0 m = trace0 b0 m





fromExistingEvent               :: Bridge p -> Event' Existing p t -> Maybe (Event p t)
fromExistingEvent b (t :+ acts) = fmap (t:+) . NonEmpty.nonEmpty . filterActions b $ acts

filterActions              :: Bridge p -> NonEmpty (Existing (Action p)) -> [Identity (Action p)]
filterActions (Bridge l r) = foldr f []
  where
    f (Left act)  xs = if act `occursLeftOf`  l then (Identity act):xs else xs
    f (Right act) xs = if act `occursRightOf` r then (Identity act):xs else xs


occursLeftOf  :: Action p -> p -> Bool
occursLeftOf = undefined
occursRightOf :: Action p -> p -> Bool
occursRightOf = undefined





-- outputEvents                                       :: Alternating (Scene p) (Event' f p t)
--                                                    -> Alternating (Bridge p) (Event p t)
--                                                    -> [Event p t]
-- outputevents = undefined

-- outputEvents (Alternating _ es) (Alternating b bs) = mergeWith b es bs
--   where
--     mergeWith _ []     []      = []
--     mergeWith b es     []      = mapMaybe (fromExistingEvent b) es
--     mergeWith _ []     bs      = bs
--     mergeWith b (e:es) (e':bs) =


--     mergeSortedListsBy (comparing (^.eventTime))

--     es bs
--   where
--     mergeLists = undefined





-- shouldKeep :: Bridge p -> Event' Existing p t
-- shouldKeep


  -- where
  --   evts = undefined







trace0          :: Ord t => Bridge p -> Alternating (Scene p) (Event' Existing p t) -> [Event p t]
trace0   b anim = trace' b anim (initialBridgeChange (currentScene anim) b)

trace          :: Ord t => t -> Bridge p -> Alternating (Scene p) (Event' Existing p t) -> [Event p t]
trace  t b anim = trace' b anim (nextBridgeChange t (currentScene anim) b)


data NextEvent' f p t = BridgeEvent
                      | ExistingEvent (Event' f p t) (Alternating (Scene p) (Event' f p t))
                      | CombinedEvent (Event' f p t) (Alternating (Scene p) (Event' f p t))
type NextEvent = NextEvent' Existing


nextEvent                    :: Ord t
                             => Alternating (Scene p) (Event' Existing p t) -> Maybe t
                             -> Maybe (t :+ NextEvent p t)
nextEvent (Alternating _ es) = go es
  where
    go []           Nothing  = Nothing
    go []           (Just t) = Just $ t:+ BridgeEvent
    go ((e:+s):es') Nothing  = let t' = e^.eventTime
                               in Just $ t' :+ ExistingEvent e (Alternating s es')
    go ((e:+s):es') (Just t) = let t' = e^.eventTime
                               in Just $ case t' `compare` t of
                                           LT -> t' :+ ExistingEvent e (Alternating s es')
                                           EQ -> t  :+ CombinedEvent e (Alternating s es')
                                           GT -> t  :+ BridgeEvent

trace'        :: Ord t
              => Bridge p -> Alternating (Scene p) (Event' Existing p t) -> Maybe t
              -> [Event p t]
trace' b a mt = case nextEvent a mt of
    Nothing        -> []
    Just (t :+ ne) -> let STR evt b' a' = continue t ne
                          mx ?: xs = maybe xs (:xs) mx
                      in evt ?: trace t b' a'
  where
    continue t = \case
      ExistingEvent e a' -> STR (fromExistingEvent b e)       b  a'
      BridgeEvent        -> let SP acts b' = handleBridgeEvent b t (currentScene a) in
                            STR (Just $ newEvent t acts)      b' a
      CombinedEvent e a' -> let SP acts b' = handleBridgeEvent b t (currentScene a') in
                            STR (Just $ combinedEvent e acts) b' a'

    newEvent t acts = t :+ (coerce acts)
    combinedEvent (t :+ eActs) (coerce -> ac:|acts) = t :+ (ac:| (acts <> filterActions b eActs))
    -- the combining is going to be more complicated I think

-- | Handles a bridge event; i.e. figures out if
handleBridgeEvent                    :: Bridge p -> t -> Scene p
                                     -> SP (NonEmpty (Action p)) (Bridge p)
handleBridgeEvent b@(Bridge l r) t s = SP (toNonEmpty . catMaybes $ [leftE, rightE]) b'
  where
    b'@(Bridge l' r') = findBridgeFrom b t s
    toNonEmpty = fromMaybe (error "Bridge event without producing output!") . NonEmpty.nonEmpty
    leftE = case l' `compareX` l of
              EQ -> Nothing
              LT -> Just $ Delete l
              GT -> Just $ InsertAfter l l'
    rightE = case r' `compareX` r of
               EQ -> Nothing
               LT -> Just $ Delete r
               GT -> Just $ InsertBefore r r'
  -- TODO: It may be possible to save the comparison here by keeping track of what type of
  -- actions we had to perform in 'nextBridgeChangeWith'

-- | Compares the x-coordinates of the points
compareX = undefined


findBridgeFrom       :: Bridge p -> t -> Scene p -> Bridge p
findBridgeFrom b t s = undefined
                       -- essentially: find the outermost points colinear with the bridge,
                       -- unless we are deleting those at this time.
                       --




initialBridgeChange :: Ord t => Scene p -> Bridge p -> Maybe t
initialBridgeChange = nextBridgeChangeWith id

nextBridgeChange :: Ord t => t -> Scene p -> Bridge p -> Maybe t
nextBridgeChange t  = nextBridgeChangeWith (filter (t <))

nextBridgeChangeWith                  :: Ord t
                                      => ([t] -> [t])
                                      -> Scene p -> Bridge p -> Maybe t
nextBridgeChangeWith f s (Bridge u v) = minimum1 . f . catMaybes $
                                        [ getPrev u s >>= \a -> colinearTime a u v
                                        , getNext u s >>= \b -> colinearTime u b v
                                        , getPrev v s >>= \c -> colinearTime u c v
                                        , getNext v s >>= \d -> colinearTime u v d
                                        ]

colinearTime :: p -> p -> p -> Maybe t
colinearTime = undefined

getPrev :: p -> Scene p -> Maybe p
getPrev = undefined

getNext :: p -> Scene p -> Maybe p
getNext = undefined


-- data Scene a = Scene (Seq a) a (Seq a)

-- data Slice a = Slice (Movie )






-- type Deque ascan = Seq a

-- -- | Slice of the two hulls
-- data Slice a = Slice (Deque a) a (Deque a) (Deque a) a (Deque a) deriving (Show,Eq)

-- asL

-- -- emptyLeft (Slice ll u lr rl v rr) = let (ll' :|> u') = u <| lr
-- --                                     in Slice (ll <> ll') u' empty rl v rr

-- -- emptyRight (Slice ll u lr rl v rr) = let (v' :<| rl') = rl |> v
-- --                                      in Slice ll u lr empty v' (rl' <> rr)

-- instance Semigroup (Slice a) where
--   l <> r = let

--     Slice lll lu _ rl v rr = emptyLeft l
--                Slice
--            in Slice


-- S
-- data SSlice a = SSlice (Deque a) a (Deque a) deriving (Show,Eq)
-- data Slice a = Slice (SSlice a) (SSlice a) deriving (Show,Eq)







-- data Movie a = Movie [Slice a] (Slice a) [Slice a] deriving (Show,Qq)
minimum1 :: Ord a => [a] -> Maybe a
minimum1 = \case
  [] -> Nothing
  xs -> Just $ List.minimum xs
