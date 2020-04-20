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
-- import           Data.Witherable.Class
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




data NextEvent' f p t = BridgeEvent   (Event p t)
                      | ExistingEvent (Event' f p t)             (Alternating (Scene p) (Event' f p t))
                      | CombinedEvent (Event' f p t) (Event p t) (Alternating (Scene p) (Event' f p t))
type NextEvent = NextEvent' Existing

nextTime = \case
  BridgeEvent   e     -> e^.eventTime
  ExistingEvent e   _ -> e^.eventTime
  CombinedEvent e _ _ -> e^.eventTime



nextEvent                    :: Ord t
                             => Alternating (Scene p) (Event' Existing p t)
                             -> Maybe (Event p t) -- ^ The first bridge event
                             -> Maybe (NextEvent p t)
nextEvent (Alternating _ es) = go es
  where
    go []           Nothing  = Nothing
    go []           (Just b) = Just $ BridgeEvent b
    go ((e:+s):es') Nothing  = Just $ ExistingEvent e (Alternating s es')
    go ((e:+s):es') (Just b) = Just $ case (e^.eventTime) `compare` (b^.eventTime) of
                                        LT -> ExistingEvent e   (Alternating s es')
                                        EQ -> CombinedEvent e b (Alternating s es')
                                        GT -> BridgeEvent b

trace'        :: Ord t
              => Bridge p -> Alternating (Scene p) (Event' Existing p t) -> Maybe (Event p t)
              -> [Event p t]
trace' b a mt = case nextEvent a mt of
    Nothing  -> []
    Just ne  -> let STR evt b' a' = continue ne
                    mx ?: xs = maybe xs (:xs) mx
                in evt ?: trace (nextTime ne) b' a'
  where
    continue = \case
      ExistingEvent e a'    -> STR (fromExistingEvent b e)     b  a'
      BridgeEvent e         -> let SP e' b' = handleBridgeEvent b e (currentScene a) in
                               STR (Just e')                   b' a
                               -- in this case there is not much to handle; since
                               -- we actually know what the bridge should look like now
      CombinedEvent e be a' -> let SP e' b' = handleBridgeEvent b e (currentScene a') in
                               STR (Just $ combinedEvent e e') b' a'

    newEvent t acts = t :+ (coerce acts)



handleOnlyBridgeEvent = undefined
-- it seems that in this case there is necessarily only one action
-- involving a bridge point i.e. if the left bridgepoint gets
-- overtaken by both its predecesor and successor, then we would have
-- an existing event in the left hull as well. Same for on the right
-- so the worst thing that can happen is that we have one event on the
-- left, and one on the right.

-- TODO: Figure out once more what happens if we have an event on the left *and* on the *right*
-- in particular, make sure that all four cases are ok:
--
-- insert, insert: does this still mean we have an insert event?
-- delete, delete:
-- insert, delete
-- delete, insert -- should be symemtric to the one above





combinedEvent = undefined
-- combinedEvent (t :+ eActs) (coerce -> ac:|acts) = t :+ (ac:| (acts <> filterActions b eActs))
    -- the combining is going to be more complicated I think

-- | Handles a bridge event; i.e. figures out if
handleBridgeEvent                             :: Bridge p -> Event' f p t -> Scene p
                                              -> SP (Event p t) (Bridge p)
handleBridgeEvent b@(Bridge l r) e@(t :+ _) s = SP e' b'
  where
    b'@(Bridge l' r') = undefined -- findBridgeFrom b e s

    e' = undefined

    toNonEmpty = fromMaybe (error "Bridge event without producing output!") . NonEmpty.nonEmpty
    -- leftE = case l' `compareX` l of
    --           EQ -> Nothing
    --           LT -> Just $ Delete l
    --           GT -> Just $ InsertAfter l l'
    -- rightE = case r' `compareX` r of
    --            EQ -> Nothing
    --            LT -> Just $ Delete r
    --            GT -> Just $ InsertBefore r r'
  -- TODO: It may be possible to save the comparison here by keeping track of what type of
  -- actions we had to perform in 'nextBridgeChangeWith'

-- | Compares the x-coordinates of the points
compareX = undefined


findBridgeFrom       :: Bridge p -> Event p t -> Scene p -> Bridge p
findBridgeFrom b t s = undefined
                       -- essentially: find the outermost points colinear with the bridge,
                       -- unless we are deleting those at this time.
                       --




initialBridgeChange :: Ord t => Scene p -> Bridge p -> Maybe (Event p t)
initialBridgeChange = nextBridgeChangeWith (const True)

nextBridgeChange :: Ord t => t -> Scene p -> Bridge p -> Maybe (Event p t)
nextBridgeChange t  = nextBridgeChangeWith (t <)



-- -- data BridgeE e p t = BridgeE (Bridge p) !(e Identity p t) deriving (Show,Eq)
-- data BridgeEvent p t = BE (Event p t) (Bridge p)


-- data These' l = None
--               | This l
--               | That l
--               | Both l l
--               deriving (Show,Eq,Functor,Foldable,Traversable)

-- embedMaybe   :: (l -> These' l) -> Maybe l -> These l
-- embedMaybe f = \case
--   Nothing -> None
--   Just x  -> f x

-- instance Filterable These' where
--   catMaybes = \case
--     None     -> None
--     This l   -> embedMaybe This l
--     That r   -> embedMaybe That r
--     Both l r -> case l of
--                   Nothing -> embedMaybe r
--                   Just x  -> case r of
--                                Nothing -> This l
--                                Just y  -> Both x y

-- minimaThese     :: (a -> a -> Ordering) -> These a -> These a
-- minimaThese cmp = \case
--   None     -> None
--   This l   -> This l
--   That r   -> That r
--   Both l r -> case l `cmp` r of
--                 LT -> This l
--                 EQ -> Both l r
--                 GT -> That r



nextBridgeChangeWith                  :: Ord t
                                      => (t -> Bool)
                                      -> Scene p -> Bridge p -> Maybe (Event p t)
nextBridgeChangeWith p s (Bridge u v) =
    fmap asEvent . NonEmpty.nonEmpty . minimaOn (^.core) . filter (p . (^.core)) . catMaybes $
      [ getPrev u s >>= \a -> colinearTime a u v <&> (:+ Delete u)
      , getNext u s >>= \b -> colinearTime u b v <&> (:+ InsertAfter u b)
      , getPrev v s >>= \c -> colinearTime u c v <&> (:+ InsertBefore v c)
      , getNext v s >>= \d -> colinearTime u v d <&> (:+ Delete v)
      ]
  where
    asEvent as@(a:|_) = a^.core :+ (coerce $ (^.extra) <$> as)

    -- TODO: IF we distinguish between deleteL and deleteR we can recover what the new
    -- bridge is supposed to be in the onlyBridgeEvent case
    -- we should also just store the a and d I guess



  -- where
  --   f = minimaThese (comparing (^.core)) . filter (p . (^.core)) . catMaybes

  --   left = case f leftCandidates of
  --            None                      -> Nothing
  --            This (t :+ a)             -> Just $ t :+ (a :+ (Delete u       ))
  --            That (t :+ b)             -> Just $ t :+ (b :+ (InsertAfter u b))
  --            Both (t :+ a) (_ :+ b)
  --                 | a `isFasterThan` b -> Just $ t :+ (a :+ (Delete u       ))
  --                 | otherwise
  --            -- it seems that what happens depends on which of a and b gets deleted
  --            -- moreover, it may depend on who is the right bridgepoint

  --              -> Just $ t :+ (fastest a b)
  --            a -> This $ a&extra %~ Delete


  --   leftCandidates  = Both (getPrev u s >>= \a -> colinearTime a u v <&> (:+ a))
  --                          (getNext u s >>= \b -> colinearTime u b v <&> (:+ b))



  --   -- leftCandidates  = Both (getPrev u s >>= \a -> colinearTime a u v <&> (:+ Delete u))
  --   --                        (getNext u s >>= \b -> colinearTime u b v <&> (:+ InsertAfter u b))



  --   -- rightCandidates = Both (getPrev v s >>= \c -> colinearTime u c v <&> (:+ $ InsertBefore v c))
  --   --                        (getNext v s >>= \d -> colinearTime u v d <&> (mk Right d $ Delete v))




  --     [
  --           ,
  --           ]







  -- I guess these insertions may be wrong; if there happens to be a faster guy
  -- the deletions seem ok though


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


minimaOn   :: Ord b => (a -> b) -> [a] -> [a]
minimaOn f = minimaBy (comparing f)

-- | computes all minima
minimaBy     :: (a -> a -> Ordering) -> [a] -> [a]
minimaBy cmp = \case
  []     -> []
  (x:xs) -> NonEmpty.toList $ List.foldl' (\mins@(m:|_) y -> case m `cmp` y of
                                                               LT -> mins
                                                               EQ -> (y NonEmpty.<| mins)
                                                               GT -> (y:|[])
                                          ) (x:|[]) xs

-- minimaBy'        :: Witherable f => (a -> a -> Ordering) -> f a -> f a
-- minimaBy' cmp xs =
