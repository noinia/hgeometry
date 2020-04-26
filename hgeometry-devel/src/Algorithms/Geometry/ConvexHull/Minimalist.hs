{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.ConvexHull.Minimalist
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(3\)-d convex hull algorithm. The implementation is based on
--
-- <http://tmc.web.engr.illinois.edu/ch3d/ch3d.pdf A Minimalist’s Implementationof the3-dDivide-and-ConquerConvex Hull Algorithm>
-- by Timothy M. Chan
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.ConvexHull.Minimalist where

import           Algorithms.DivideAndConquer
import           Algorithms.Geometry.ConvexHull.Movie
import           Algorithms.Geometry.ConvexHull.Scene
import           Control.Lens
import           Data.Coerce
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Triangle
import           Data.List.Alternating
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.Util (minimaOn)
import           Data.Maybe (catMaybes)
import           Data.Ord (Down(..))
import           Data.Semigroup
import           Data.Util
import           Prelude hiding (Either(..))


import           Data.Geometry.Ipe


--------------------------------------------------------------------------------

type LowerHull d p r = [Triangle 3 p r]

-- | Computes the lower hull, i.e. set of triangles s.t.
-- the points are above all supporting planes
lowerHull' :: forall r q. (Ord r, Fractional r, Show r, IpeWriteText r)
           => NonEmpty (Point 3 r :+ q) -> LowerHull 3 q r
lowerHull' = outputTriangles
           . divideAndConquer1With merge baseCase
           . NonEmpty.groupWith1 (^.core.xCoord)
           . NonEmpty.sortBy cmpXYZ

--------------------------------------------------------------------------------
-- * Producing the Output

outputTriangles :: Movie p t -> LowerHull 3 q r
outputTriangles = undefined


--------------------------------------------------------------------------------
-- * Merging

-- |
merge     :: (Ord t, Fractional t, HasScene p, Semigroup (Scene p), HasNeighbours p, AsPoint p t)
          => Movie p t -> Movie p t -> Movie p t
merge l r = let anim   = play $ l `sideBySide` r
                SP b h = initialBridge (initialScene l) (initialScene r)
            in Movie h $ traceBridge b anim


type Bridge p = Two p

pattern Bridge     :: p -> p -> Bridge p
pattern Bridge u v = Two u v
{-# COMPLETE Bridge #-}

type Hull p = Scene p

initialBridge :: Hull p -> Hull p -> SP (Bridge p) (Hull p)
initialBridge = undefined

findBridge :: t -> NonEmpty p -> NonEmpty p -> Bridge p
findBridge = undefined


-- | Traces the bridge
traceBridge         :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t)
                    => Bridge p -> Alternating (Scene p) (Event' Existing p t) -> [Event p t]
traceBridge b0 anim = trace' b0 anim (initialBridgeChange (currentScene anim) b0)
  where
    initialBridgeChange = nextBridgeChange (const True)
      -- initially, there is no requirement on the time of the next bridge event.

trace           :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t)
                => t -> Bridge p -> Alternating (Scene p) (Event' Existing p t) -> [Event p t]
trace  t b anim = trace' b anim (nextBridgeChange (> t) (currentScene anim) b)


trace'        :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t)
              => Bridge p -> Alternating (Scene p) (Event' Existing p t) -> Maybe (BridgeInfo p t)
              -> [Event p t]
trace' b a mt = case nextEvent a mt of
    Nothing  -> []
    Just ne  -> let STR evt b' a' = continue ne
                    mx ?: xs = maybe xs (:xs) mx
                in evt ?: trace (nextTime ne) b' a'
  where
    continue = \case
      ExistingEvent e a'    -> STR (fromExistingEvent b e)     b  a'
      BridgeEvent e         -> let SP e' b' = bridgeEventOnly b e in
                               STR (Just e')                   b' a
      CombinedEvent e bi a' -> let SP e' b' = combinedEvent b e bi (currentScene a)
                                                                   (currentScene a') in
                               STR (Just e') b' a'

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

----------------------------------------
-- ** Finding the Next event

data NextEvent' f p t =
    BridgeEvent   (BridgeInfo p t)
  | ExistingEvent (Event' f p t)                  (Alternating (Scene p) (Event' f p t))
  | CombinedEvent (Event' f p t) (BridgeInfo p t) (Alternating (Scene p) (Event' f p t))
type NextEvent = NextEvent' Existing

nextTime :: NextEvent' f p t -> t
nextTime = \case
  BridgeEvent   e     -> e^.bridgeEventTime
  ExistingEvent e   _ -> e^.eventTime
  CombinedEvent e _ _ -> e^.eventTime

nextEvent                    :: Ord t
                             => Alternating (Scene p) (Event' Existing p t)
                             -> Maybe (BridgeInfo p t) -- ^ The first bridge event
                             -> Maybe (NextEvent p t)
nextEvent (Alternating _ es) = go es
  where
    go []           Nothing  = Nothing
    go []           (Just b) = Just $ BridgeEvent b
    go ((e:+s):es') Nothing  = Just $ ExistingEvent e (Alternating s es')
    go ((e:+s):es') (Just b) = Just $ case (e^.eventTime) `compare` (b^.bridgeEventTime) of
                                        LT -> ExistingEvent e   (Alternating s es')
                                        EQ -> CombinedEvent e b (Alternating s es')
                                        GT -> BridgeEvent b


-- | Computes the next bridge change that satisifies the given
-- predicate.
nextBridgeChange                  :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t)
                                  => (t -> Bool)
                                  -> Scene p -> Bridge p -> Maybe (BridgeInfo p t)
nextBridgeChange p s (Bridge u v) =
    fmap asEvent . NonEmpty.nonEmpty . minimaOn (^.core) . filter (p . (^.core)) . catMaybes $
      [ getPrev u s >>= \a -> colinearTime a u v <&> (:+ DeleteL u a)
      , getNext u s >>= \b -> colinearTime u b v <&> (:+ InsertAfter' u b)
      , getPrev v s >>= \c -> colinearTime u c v <&> (:+ InsertBefore' v c)
      , getNext v s >>= \d -> colinearTime u v d <&> (:+ DeleteR v d)
      ]
  where
    asEvent as@(a:|_) = a^.core :+ ((^.extra) <$> as)

-- | compute the time at which r becomes colinear with the line through
-- p and q.
--
-- pre: p_x < q_x < r_x
colinearTime       :: (AsPoint p t, Ord t, Fractional t) => p -> p -> p -> Maybe t
colinearTime p q r = colinearTime' (asPoint p) (asPoint q) (asPoint r)

-- | compute the time at which r becomes colinear with the line through
-- p and q.
--
-- pre: p_x < q_x < r_x
colinearTime'  :: (Ord r, Fractional r) => Point 3 r -> Point 3 r -> Point 3 r -> Maybe r
colinearTime' (Point3 px py pz) (Point3 qx qy qz) (Point3 rx ry rz) =
    if b == 0 then Nothing else Just $ a / b
  where        -- by unfolding the def of ccw
    ux = qx - px
    vx = rx - px
    a = ux*(rz - pz)  - vx*(qz - pz)
    b = ux*(ry - py)  - vx*(qy - py)
  -- b == zero means the three points are on a vertical plane. This corresponds
  -- to t = -\infty.


data BridgeAction p = InsertAfter' p p | InsertBefore' p p
                    | DeleteL p p      | DeleteR p p            deriving (Show,Eq)

toAction :: BridgeAction p -> Action p
toAction = \case
  InsertAfter' a b  -> InsertAfter a b
  InsertBefore' a b -> InsertBefore a b
  DeleteL x _       -> Delete x
  DeleteR x _       -> Delete x

type BridgeInfo p t = t :+ NonEmpty (BridgeAction p)

bridgeEventTime :: Lens (BridgeInfo p t) (BridgeInfo p t') t t'
bridgeEventTime = core

bridgeActions :: Lens (BridgeInfo p t)            (BridgeInfo q t)
                      (NonEmpty (BridgeAction p)) (NonEmpty (BridgeAction q))
bridgeActions = extra


-- | returns the list of points involvedin the action. If it is a
-- "left" action, the list is returned in right-to-left order, whereas
-- if it is a right action it is in left-to-right order.
collect  :: BridgeAction p -> NonEmpty p
collect = NonEmpty.fromList . \case
  InsertAfter' l l'     -> [l',l]
  InsertBefore' r r'    -> [r',r]
  DeleteL l l'          -> [l,l']
  DeleteR r r'          -> [r,r']


bridgeFromAction              :: Bridge p -> BridgeAction p -> Bridge p
bridgeFromAction (Bridge l r) = \case
  InsertAfter' _ l'     -> Bridge l' r
  InsertBefore' _ r'    -> Bridge l  r'
  DeleteL _ l'          -> Bridge l' r
  DeleteR _ r'          -> Bridge l  r'

fromBridgeInfo :: BridgeInfo p t -> Event p t
fromBridgeInfo = extra %~ fmap (coerce . toAction)

----------------------------------------
-- ** Handling an event

bridgeEventOnly      :: (Num t)
                     => Bridge p -- ^ current bridge
                     -> BridgeInfo p t -- ^ the suggested event
                     -> SP (Event p t) (Bridge p) -- ^ the event to output and the new bridge
bridgeEventOnly b bi =
  case bi^.bridgeActions of
    (a  :| [])   -> SP (fromBridgeInfo bi) (bridgeFromAction b a)
    (al :| [ar]) -> let b' = findBridge (1 + bi^.bridgeEventTime) (collect al) (collect ar)
                    in SP undefined b' -- we need to filter which
                                       -- actions should actually
                                       -- occur, depending on b'
                       -- we can pick any time t' > t
    _            -> error "bridgeEventOnly: absurd, <=2 actions possible"
    -- TODO: make BridgeAction return a These or so.

combinedEvent             :: Num t
                          => Bridge p
                          -> Event' Existing p t -> BridgeInfo p t
                          -> Scene p -- ^ old scene
                          -> Scene p -- ^ new scene
                          -> SP (Event p t) (Bridge p)
combinedEvent (Bridge l r) e bi s s' = SP undefined b'   -- TODO: figure out which bridge actions to output
  where
    b' = findBridge (1 + bi^.bridgeEventTime) ls rs

    ls = colinears l
    rs = colinears r

colinears :: a
colinears = undefined

-- -- | Finds all points on the *new* hull, colinear with the bridge
-- colinears ::
-- colinears q z acts = case acts
--   where
--     p =


--   let p  = findOnHull
--                                 ls = colinears


--------------------------------------------------------------------------------
-- * Base case

baseCase     :: ( p ~ (Point 3 r :+ q), t ~ r
                , Ord r)
             => NonEmpty (Point 3 r :+ q) -> Movie p t
baseCase pts = Movie (singleton . NonEmpty.head $ pts) []
-- TODO: Do the right thing


--------------------------------------------------------------------------------
-- * Initialization

-- | Comparator for the points. We sort the points lexicographically
-- on increasing x-coordiante, decreasing y-coordinate, and increasing
-- z-coordinate. The extra data is ignored.
--
-- The divide and conquer algorithm needs the points sorted in
-- increasing order on x.

-- The choice of sorting order of the y and z-coordinates is such that
-- in a leaf (all points with the same x-coord). Are already
-- pre-sorted in the right way: in particular, increasing on their
-- "slope" in the "Time x Y'" space. This means that when we compute
-- the lower envelope of these lines (using the duality and upper
-- hull) we don't have to re-sort the points. See 'simulateLeaf'' for
-- details.
cmpXYZ :: Ord r => (Point 3 r :+ p) -> (Point 3 r :+ q) -> Ordering
cmpXYZ (Point3 px py pz :+ _) (Point3 qx qy qz :+ _) =
  compare px qx <> compare (Down py) (Down qy) <> compare pz qz


--------------------------------------------------------------------------------
-- * Helpers
