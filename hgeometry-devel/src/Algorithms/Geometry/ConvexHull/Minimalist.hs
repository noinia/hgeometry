{-# LANGUAGE UndecidableInstances #-}
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
import qualified Algorithms.Geometry.ConvexHull.GrahamScan as GrahamScan
import           Algorithms.Geometry.ConvexHull.JarvisMarch (steepestCcwFrom)
import           Algorithms.Geometry.ConvexHull.Movie
import           Algorithms.Geometry.ConvexHull.Scene
import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.Coerce
import           Data.Ext
import           Data.Foldable
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Convex (lowerTangent')
import           Data.Geometry.Triangle
import           Data.Geometry.Vector
import qualified Data.List as List
import           Data.List.Alternating(Alternating(..), withNeighbours)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.Util (minimaOn)
import           Data.Maybe (mapMaybe, catMaybes, maybeToList, fromMaybe)
import           Data.Monoid (Alt(..))
import           Data.Ord (comparing, Down(..))
import           Data.Semigroup
import           Data.Util
import           Prelude hiding (Either(..))


import           Algorithms.Geometry.ConvexHull.DebugMinimalist
import           Data.Geometry.Ipe
import           Data.RealNumber.Rational

import           Debug.Trace (traceShow)
-- traceShow _ = id

--------------------------------------------------------------------------------

type LowerHull d q r = [Triangle 3 q r]

-- | Computes the lower hull, i.e. set of triangles s.t.
-- the points are above all supporting planes
lowerHull'     :: (Ord r, Fractional r
                  , Show r, IpeWriteText r, q ~ Int
                  )
               => NonEmpty (Point 3 r :+ q) -> LowerHull 3 q r
lowerHull' pts = outputTriangles
               . divideAndConquer1With (merge t0) baseCase
               . NonEmpty.groupWith1 (^.core.xCoord)
               . NonEmpty.sortBy cmpXYZ
               $ pts
  where
    t0 = -10000000000000000000000000000000000000000000000000000000000000

--------------------------------------------------------------------------------
-- * Producing the Output

outputTriangles :: (HasNeighbours p, WithExtra p q, AsPoint p r, HasScene p)
                => Movie p t -> LowerHull 3 q r
outputTriangles = concatMap (\(s,(e :+ _)) -> reportTriangles s e) . withNeighbours . play
  where
    reportTriangles (Identity s) =
      foldMap (reportTriangle s . runIdentity) . toList . view actions

reportTriangle   :: forall p q r. (HasNeighbours p, WithExtra p q, AsPoint p r
                                  , Show p
                                  )
                 => Scene p -> Action p -> [Triangle 3 q r]
reportTriangle s = fmap toTriangle . reportTriplets
  where
    toPoint p        = asPoint p :+ askExtra p
    toTriangle       = view (re _TriangleThreePoints) . fmap toPoint
    reportTriplets a = case a of
      Replace i j      -> catMaybes [        Three i j    <$> getNext i s
                                    , (\l -> Three l i j) <$> getPrev i s
                                    ]
      _                -> maybeToList $ reportTriplet a

    reportTriplet  = \case
      InsertAfter i j  ->          Three i j     <$> getNext i s
      InsertBefore i h -> (\l   -> Three l h i)  <$> getPrev i s
      Delete j         -> (\l r -> Three l j r)  <$> getPrev j s <*> getNext j s
      Replace _ _      -> error "replace not supported in output"

    --     --
    -- replaceReplace :: Identity (Action p) -> [Action p]
    -- replaceReplace = runIdentity >>> \case
    --   Replace      a b -> [InsertAfter a b, Delete a]
    --   a                -> [a]
    -- fromBreakPoint (Two a b) = coerce <$> NonEmpty.fromList [InsertAfter a b, Delete a]



--------------------------------------------------------------------------------
-- * Merging

-- |
merge        :: ( Ord t, Ord p, Fractional t, AsPoint p t
                , HasScene p, Semigroup (Scene p), HasNeighbours p
                , IpeWriteText t, Show t, WithExtra p Int, Show p, Show (Scene p)
                )
             => t -> Movie p t -> Movie p t -> Movie p t
merge t0 l r = let b :+ h = tr "inital" $ initialBridge t0 (initialScene l) (initialScene r)
                   m@(Movie pts _ _)   = l `sideBySide` r
               in debugMovieTo ("merge_" <> rangeOfSS l r) ((^.eventTime) <$> events m) $
                  movie pts h $ tr ("events " <> rangeOfSS l r) $ traceBridge b (play $ m)



type Bridge p = Two p
pattern Bridge     :: p -> p -> Bridge p
pattern Bridge u v = Two u v
{-# COMPLETE Bridge #-}

type Hull p = Scene p

type Hulls p = Two (Hull p)


-- Given collected existing and bridge events, produce the
-- outputEvents :: Alternating (Two (Scene p), Bridge p, Scene p) (Event' Existing p t) -> [Event p t]




--------------------------------------------------------------------------------
-- * Computing the Initial Bridge

-- | Finds the initial bridge.
initialBridge         :: (AsPoint p t, Ord t, Num t, HasNeighbours p, HasScene p)
                      => t -> Hull p -> Hull p -> Bridge p :+ Hull p
initialBridge t0 lh rh = mkHull $ findBridge' t0 (fromRightMost lh) (fromLeftMost rh)
  where
    mkHull (Two (l:+ls) (r:+rs)) =
      Bridge l r :+ (fromNonEmpty . NonEmpty.fromList $ (reverse ls) <> [l,r] <> rs)

-- | Computing the bridge at time t. See 'lowerTangent'' for details.
findBridge'       :: (AsPoint p t, Ord t, Num t) => t -> NonEmpty p -> NonEmpty p -> Two (p :+ [p])
findBridge' t l r = strip $ lowerTangent' (atTime'' t <$> l) (atTime'' t <$> r)
  where
    atTime'' t' p = atTime t' p :+ p
    strip = fmap (bimap (^.extra) (fmap (^.extra)))

-- | Computes only the bridge (throwing away the remainders)
findBridge       :: (AsPoint p t, Ord t, Num t
                    , Show p, Show t
                    ) => t -> NonEmpty p -> NonEmpty p -> Bridge p
findBridge t l r | traceShow ("findBridge",t,l,r) False = undefined
findBridge t l r = (\(Two l' r') -> Bridge (l'^.core) (r'^.core)) $ findBridge' t l r

--------------------------------------------------------------------------------
-- * Tracing the Bridge

type Animation p t = Alternating (Two (Scene p)) (Event' Existing p t)

-- | Start Tracing the Bridge
traceBridge         :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t, Ord p
                       , Show p, Show t, Show (Scene p)
                       )
                    => Bridge p -> Animation p t -> [Event p t]
traceBridge b0 anim = trace b0 anim (initialBridgeChange (currentScene anim) b0)
  where
    initialBridgeChange = nextBridgeChange (const True)
      -- initially, there is no requirement on the time of the next bridge event.
  -- TODO: use a DList instead


-- | The actual tracing happens in this function. Given the current bridge, and the
-- animation representing the .
trace        :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t, Ord p
                 , Show p, Show t, Show (Scene p)
                 )
              => Bridge p -> Animation p t -> Maybe (BridgeInfo p t)
              -> [Event p t]
trace b a mt = case tr "trace, nextEvent: " $ nextEvent a mt of
    Nothing  -> []
    Just ne  -> let SP (SP evt b') a' = tr "trace, outputting" $ continue (tr "trace, ne: " ne)
                in evt ?: trace' (nextTime ne) b' a'
  where
    continue = \case
      ExistingEvent (t :+ acts) a'    ->
          let e'b' = combinedEvent b t acts [] (currentScene a) (currentScene a')
          in SP e'b' a'
          -- we need the handling done in combinedEvent since if this
          -- is a replace then we may actually have a simultaneous bridge event happening anyway.
          -- see buggy11S
      BridgeEvent e                   ->
          let z = tr "bridgeOnlyEvent: " $ bridgeEventOnly b e
          in SP z a
      CombinedEvent e (t :+ bActs) a' ->
          let e'b' = combinedEvent b t e bActs (currentScene a) (currentScene a')
          in SP e'b' a'

      -- ExistingEvent e a'    -> let b' = bridgeFromExisting b e
      --                              e' = fromExistingEvent b' e
      --                          in SP (SP e' b') a'
      --                          -- if the existing event is a replace, the bridge may change!
      -- BridgeEvent e         -> let z = tr "bridgeOnlyEvent: " $ bridgeEventOnly b e
      --                          in SP z a
      -- CombinedEvent e (t :+ bActs) a' ->
      --     let e'b' = combinedEvent b t e bActs (currentScene a) (currentScene a')
      --     in SP e'b' a'

-- | Continues tracing by finding a new birdge event
trace'          :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t, Ord p
                  , Show p, Show t, Show (Scene p)
                  )
                => t -> Bridge p -> Animation p t -> [Event p t]
trace' t b anim = trace b anim (nextBridgeChange (> t) (currentScene anim) b)

----------------------------------------
-- ** Finding the Next event

data NextEvent p t =
    BridgeEvent   (BridgeInfo p t)
  | ExistingEvent (Event' Existing p t)                             (Animation p t)
  | CombinedEvent (NonEmpty (Existing (Action p))) (BridgeInfo p t) (Animation p t)

deriving instance (Show p, Show t, Show (Scene p)) => Show (NextEvent p t)

nextTime :: NextEvent p t -> t
nextTime = \case
  BridgeEvent   e     -> e^.bridgeEventTime
  ExistingEvent e   _ -> e^.eventTime
  CombinedEvent _ e _ -> e^.bridgeEventTime

nextEvent                    :: Ord t
                             => Animation p t
                             -> Maybe (BridgeInfo p t) -- ^ The first bridge event
                             -> Maybe (NextEvent p t)
nextEvent (Alternating _ es) = go es
  where
    go []           Nothing  = Nothing
    go []           (Just b) = Just $ BridgeEvent b
    go ((e:+s):es') Nothing  = Just $ ExistingEvent e (Alternating s es')
    go ((e:+s):es') (Just b) = Just $ case (e^.eventTime) `compare` (b^.bridgeEventTime) of
                                        LT -> ExistingEvent e              (Alternating s es')
                                        EQ -> CombinedEvent (e^.actions) b (Alternating s es')
                                        GT -> BridgeEvent b

-- TODO: if the event is a replace, then maybe it coincides with a bridge event after all.




-- | Computes the next bridge change that satisifies the given
-- predicate.
nextBridgeChange                            :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t
                                               , Show t, Show p
                                               )
                                            => (t -> Bool)
                                            -> Hulls p -> Bridge p -> Maybe (BridgeInfo p t)
nextBridgeChange p (Two sl sr) (Bridge u v) =
    fmap asEvent . NonEmpty.nonEmpty . minimaOn (^.core) . filter (p . (^.core)) . catMaybes
    . tr ("nextBridgeChange, candidates" <> show (u,v))
    $
      [ getPrev u sl >>= \a -> colinearTime a u v <&> (:+ BA (Delete u)         (Left a))
      , getNext u sl >>= \b -> colinearTime u b v <&> (:+ BA (InsertAfter u b)  (Left b))
      , getPrev v sr >>= \c -> colinearTime u c v <&> (:+ BA (InsertBefore v c) (Right c))
      , getNext v sr >>= \d -> colinearTime u v d <&> (:+ BA (Delete v)         (Right d))
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

data BridgeAction p = BA { toAction       :: !(Action p)
                         , newBridgePoint :: !(Existing p)
                         } deriving (Show,Eq)

type BridgeInfo p t = t :+ NonEmpty (BridgeAction p)

bridgeEventTime :: Lens (BridgeInfo p t) (BridgeInfo p t') t t'
bridgeEventTime = core

partitionBridgeActions :: Foldable f => f (BridgeAction p) -> (Maybe (Action p), Maybe (Action p))
partitionBridgeActions = foldr f (Nothing,Nothing)
  where
    f (BA a p) ~(l,r) = case p of
                          Left  _ -> (Just a,r)
                          Right _ -> (l,Just a)



-- bridgeActions :: Lens (BridgeInfo p t)            (BridgeInfo q t)
--                       (NonEmpty (BridgeAction p)) (NonEmpty (BridgeAction q))
-- bridgeActions = extra


-- | returns the list of points involvedin the action. If it is a
-- "left" action, the list is returned in right-to-left order, whereas
-- if it is a right action it is in left-to-right order.
collect         :: Show p => BridgeAction p -> NonEmpty p
collect (BA a p) = NonEmpty.fromList . tr "collect" $ case a of
    InsertAfter  l l' -> [l',l]
    InsertBefore r r' -> [r',r]
    Delete q          -> case p of Left  l' -> [q,l']
                                   Right r' -> [q,r']
    Replace q q'      -> [q,q']      -- both these points  are at the same postion
                                     -- so left to right is the same as right to left

bridgeFromAction                       :: Bridge p -> BridgeAction p -> Bridge p
bridgeFromAction (Bridge l r) (BA a p) = case a of
    InsertAfter _ l'  -> Bridge l' r
    InsertBefore _ r' -> Bridge l  r'
    Delete _          -> case p of Left  l' -> Bridge l' r
                                   Right r' -> Bridge l  r'
    Replace _ _       -> case p of Left  l' -> Bridge l' r
                                   Right r' -> Bridge l  r'

--------------------------------------------------------------------------------
-- * Handling an event

----------------------------------------
-- ** Handle  Existing



existingEventOnly     :: Bridge p -> Event' Existing p t
                                 -> SP (Maybe (Event p t)) (Bridge p)
existingEventOnly b e = undefined -- case NonEmpty.filter isAReplace e^.actions of
--                           [] -> undefined -- procede as normal

-- handleExisting



bridgeFromExisting                :: Ord p => Bridge p -> Event' Existing p t -> Bridge p
bridgeFromExisting (Bridge l r) e = Bridge (fromMaybe l $ replaces l lActs)
                                           (fromMaybe r $ replaces r rActs)
  where
    (lActs,rActs) = partitionExisting (e^.actions)
    replaces p = alaf Alt foldMap $ \case
                                        Replace p' q | p' == p -> Just q
                                        _                      -> Nothing


fromExistingEvent               :: Ord p
                                => Bridge p -> Event' Existing p t -> Maybe (Event p t)
fromExistingEvent b (t :+ acts) = fmap (t:+) . NonEmpty.nonEmpty . filterActions b $ acts

filterActions              :: Ord p
                           => Bridge p -> NonEmpty (Existing (Action p)) -> [Identity (Action p)]
filterActions (Bridge l r) = map extract . NonEmpty.filter p
  where
    p = \case
      Left a  -> rightMost a <= l -- action occurs left of l
      Right a -> r <= leftMost a   -- action occurs right of r
    extract = Identity . \case
      Left a  -> a
      Right a -> a


    -- we should not allow deletions when they are not on the actual hull anymore
    -- more relatedly.
    -- moreover, I guess we should also delete things that died if the bridge jumped over them,
    -- i.e. the l -> l' type actions.
    -- hmm, although currently filterActions is used only for existing events.


----------------------------------------
-- ** Bridge Only

-- | Given the current bridge, and ifnormation about the bridge event,
-- handles that bridge event. This function assumes that there are no
-- existing events at the time of teh bridge event. The result is the
-- event that we should output and the new bridge.
bridgeEventOnly                  :: (Num t, Ord t, AsPoint p t, Eq p
                                    , Show p, Show t
                                    )
                                 => Bridge p -> BridgeInfo p t
                                 -> SP (Maybe (Event p t)) (Bridge p)
bridgeEventOnly b bi@(t :+ acts) = case tr "bridgeEventOnly: acts " $ acts of
    (a  :| [])   -> SP (Just $ t :+ acts') (bridgeFromAction b a)
    (ar :| [al]) -> let b' = findBridge (1 + t) (collect al) (collect ar)
                    in SP (mkNewEvent bi b') b'
    _            -> error "bridgeEventOnly: absurd, <=2 actions possible"
  where
    acts' = fmap (Identity . toAction) acts


----------------------------------------
-- ** Combined Events

combinedEvent                                     :: (Fractional t, Ord t, AsPoint p t
                                                     , HasNeighbours p, Ord p
                                                     , Foldable f
                                                     , Show (Scene p), Show p, Show t
                                                     )
                                                  => Bridge p
                                                  -> t
                                                  -> NonEmpty (Existing (Action p))
                                                  -> f (BridgeAction p)
                                                  -> Hulls p -- ^ old scene
                                                  -> Hulls p -- ^ the new scene
                                                  -> SP (Maybe (Event p t)) (Bridge p)
combinedEvent b@(Bridge l r) t acts bActs (Two sl sr)
                                          (Two sl' sr') = SP e' b'
  where
    b' = findBridge (1 + t) (NonEmpty.reverse ls) rs
    ls = colinears t l r sl sl' lAct leftActs
    rs = colinears t r l sr sr' rAct rightActs

    (leftActs,rightActs) = partitionExisting acts
    (lAct,rAct)          = partitionBridgeActions bActs

    outputActions = outputBridgeActions b b'<> outputExistingActions b b' leftActs rightActs
    e' = (t :+ ) <$> NonEmpty.nonEmpty outputActions

outputExistingActions                                   :: (Ord p, AsPoint p t, Ord t)
                                                        => Bridge p -> Bridge p
                                                        -> [Action p] -> [Action p]
                                                        -> [Identity (Action p)]
outputExistingActions (Bridge l r) (Bridge l' r') ls rs =
    coerce $ filter pLeft ls <> filter pRight rs
  where
    -- the predicate that we need to filter on the left
    pLeft | l == l'   = occursLeftOf l
          | otherwise = case l `compareX` l' of
                          LT -> \a -> occursLeftOf  l a ||
                                      (occursLeftOf l' a && not (isADelete a))
                            -- shifted right
                          EQ -> \a -> occursLeftOf l a && not (isAReplace a)
                            -- we had a replacement ( in this case
                            -- there cannot be any deletions in
                            -- between l and l') The only option is
                            -- that l itself was replaced by l'. In
                            -- that case, this is already reported as
                            -- a bridge event.
                          GT -> \a -> occursLeftOf l' a ||
                                      (occursLeftOf l a && isADelete a)
                            -- shifted to the left, so only events left of l'
                            -- the smaller one, and deletions between l' and l
                            --
                            -- actually, I guess we shoul delete
                            -- everything between l' and l, but
                            -- since we already know what the new
                            -- bridge b' is going to be we know
                            -- that there cannot be any existing
                            -- guys in between there. that are not
                            -- either inserted or removed.

    -- symmetric to pLeft
    pRight | r == r'   = occursRightOf r
           | otherwise = case r `compareX` r' of
                           LT -> \a -> occursRightOf r' a ||
                                       (occursRightOf r a && isADelete a)
                                -- shifted right
                           EQ -> \a -> occursRightOf r a && not (isAReplace a)
                           GT -> \a -> occursRightOf  r a ||
                                       (occursRightOf r' a && not (isADelete a))
                             -- shifted left
    occursLeftOf  p a = rightMost a <= p
    occursRightOf p a = p <= leftMost a


outputBridgeActions                             :: (Eq p, Ord t, AsPoint p t)
                                                => Bridge p -> Bridge p -> [Identity (Action p)]
outputBridgeActions (Bridge l r) (Bridge l' r') = catMaybes [actLeft, actRight]
  where
    actLeft  | l == l'   = Nothing
             | otherwise = Just . Identity
                          $ case l `compareX` l' of
                              LT -> InsertAfter l l'
                              EQ -> Replace  l l'
                              GT -> Delete l
    actRight | r == r'   = Nothing
             | otherwise = Just . Identity
                         $ case r `compareX` r' of
                             LT -> Delete r
                             EQ -> Replace  r r'
                             GT -> InsertBefore r r'


compareX :: (AsPoint p t, Ord t) => p -> p -> Ordering
compareX = comparing (view xCoord . asPoint)


mkNewEvent                :: Eq p => BridgeInfo p t -> Bridge p -> Maybe (Event p t)
mkNewEvent (t :+ acts) b' = fmap (t :+) . NonEmpty.nonEmpty . fmap (Identity . toAction)
                          . filter (shouldOutput b') . toList $ acts

shouldOutput                         :: Eq p => Bridge p -> BridgeAction p -> Bool
shouldOutput (Bridge l' r') (BA _ p) = case p of
    Left ll  -> ll == l'
    Right rr -> rr == r'
  -- if the bridge changes to something else than what the bridge event istself
  -- tells us. There must already be existing events that will happen anyway.


  -- where
    -- handle p' pp = case a of
    --                  InsertBefore _ _ -> p' == pp -- if the bridge is now different
    --                                               -- there already is an existing
    --                                               -- insertion/the element is already
    --                                               -- present anyway
    --                  InsertAfter  _ _ -> p' == pp
    --                  Delete       q   -> p' /= q -- as long as we are not deleting
    --                                              -- new bridge endpoint; delete it.
    --                  -- I think we should probably do this only when
    --                  -- there is no deletion of q already anyway

-- | Find all points in the scene colinear with the bridge and "connected" to p
colinears                  :: (AsPoint p t, HasNeighbours p, Ord p, Ord t, Fractional t
                              , Show t, Show p, Show (Scene p)
                              )
                           => t
                           -> p -- ^ The bridge point we are processing
                           -> p -- ^ the other birdge point
                           -> Scene p -- ^ The old scene
                           -> Scene p  -- ^ The new scene
                           -> Maybe (Action p) -- ^ the bridge action on p
                           -> [Action p] -- ^ Existing actions related to this side
                           -> NonEmpty p -- ^ in left to right order
colinears t p q s s' mba acts | traceShow ("colinears",t,p,q,mba,acts,s,s') False = undefined
colinears t p q s s' mba acts = colinearsFrom (findPointInScene t p q s mba acts) q t s'
  -- TODO, we also need the newly inserted points here, i.e. our point 36


-- | Given a time t, two points: 'p' the bridge point, and q the other
-- bridge point, the old scene, and the actions occuring at time t
-- finds a point in the scene that should remain on the scene/will be
-- in the new scene.
findPointInScene ::  (HasNeighbours p, AsPoint p t, Ord t, Fractional t, Ord p
                     , Show p, Show t, Show (Scene p)
                     ) =>
                     t -> p -> p -> Scene p -> Maybe (Action p) -> [Action p] -> p
findPointInScene t p q oldS mba acts | traceShow ("findPointInscene",t,p,q,oldS,mba,acts)                     False = undefined
findPointInScene t p q oldS mba acts = case mba of
    Nothing -> p
    Just a  -> case tr "empty?" $ insertedPoint a ?: newOnBridge of
                 (z:_) -> z  -- z is a newly inserted point on the bridge
                 []    -> deletedCandidate
  where
    newOnBridge = tr "col" . filter (areColinearAt t p q) . tr "pts" . mapMaybe insertedPoint $ acts
    insertedPoint = \case
      InsertAfter  _ z -> Just z
      InsertBefore _ z -> Just z
      Delete _         -> Nothing
      Replace      _ z -> Just z

    leftMost'  = minimumOf traverse -- = minimum that returns a Maybe
    rightMost' = maximumOf traverse

    -- If our current bridge point p is deleted, and all other events
    -- are also deletions, then we can find an existing point still on
    -- the hull, that is colinear with the bridge by starting at p,
    -- walking to the left and to the right until we find a point in
    -- the hull that is not deleted (and still colinear with the
    -- bridge). Since this the time of a bridge event, such a point
    -- must exist, either on the left or on the right.
    deletedCandidate  = fromMaybe (error "findPointInScene: absurd")
                      . uncurry (<|>)
                      . bimap leftMost' rightMost'
                      . over both (filter (areColinearAt t p q))
                      . partitionExisting
                      . (neighboursOfP <>)
                      . mapMaybe deletedNeighbour $ acts

    neighboursOfP = catMaybes [Left <$> getPrev p oldS, Right <$> getNext p oldS]

    deletedNeighbour = \case
      Delete a | a <= p    -> Left  <$> getPrev a oldS
               | otherwise -> Right <$> getNext a oldS
      _                    -> Nothing




areColinearAt         :: (AsPoint p t, Ord t, Fractional t, Show t, Show p) => t -> p -> p -> p -> Bool
areColinearAt t a b c | traceShow ("areColinearAt",t,a,b,c) False = undefined
areColinearAt t a b c = ccw (atTime t a) (atTime t b) (atTime t c) == CoLinear

-- | Starting from point p, find all the maximal consecutive
-- subsequence in s containing p of points that are colinear with pq
-- at time t. The result is returned in left-to right order.
colinearsFrom         :: (HasNeighbours p, AsPoint p t, Ord t, Fractional t, Show p, Show t)
                      => p -> p -> t -> Scene p -> NonEmpty p
colinearsFrom p q t s = exploreFrom p (areColinearAt t p q) s

--------------------------------------------------------------------------------
-- * Base case

baseCase     :: ( p ~ (PExt q r), t ~ r
                , Ord r, Fractional r, Show r, Show q
                )
             => NonEmpty (Point 3 r :+ q) -> Movie p t
baseCase pts = let SP p evts = simulateLeaf (PExt <$> pts)
               in Movie (PExt <$> pts) (Identity $ singleton p) evts

simulateLeaf :: (AsPoint p t, Ord t, Fractional t, Show t) => NonEmpty p -> SP p [Event p t]
simulateLeaf = (&_2 %~ toEvents) . lowerEnvelope . fmap (\p -> toDualPoint (asPoint p) :+ p)
  where
    toEvents = map (&extra %~ fromBreakPoint)
    -- Every point in R^3 maps to a non-vertical line: y' = -y*t + z
    -- which then dualizes to the the point (-y,-z)
    toDualPoint (Point3 _ y z) = Point2 (-1*y) (-1*z)
    -- at every breakpoint we replace a by b (insert b and delete a)
    fromBreakPoint (Two a b) = coerce <$> NonEmpty.fromList [Replace a b]

    -- fromBreakPoint (Two a b) = coerce <$> NonEmpty.fromList [InsertAfter a b, Delete a]

-- | Given a set of lines, represented by their dual points, compute
-- the lower envelope of those lines. Returns the associated value of
-- the leftmost line, and all breakpoints. For every breakpoint we
-- also return the associated values of the line just before the
-- breakpoint and the line just after the breakpoint.
--
-- running time: \(O(n \log n)\)
lowerEnvelope     :: (Ord r, Fractional r, Show r) => NonEmpty (Point 2 r :+ a) -> SP a [r :+ Two a]
lowerEnvelope pts = SP i $ zipWith f (toList h) tl
  where
    f (pa :+ a) (pb :+ b) = let Vector2 x y = pb .-. pa in y / x :+ Two a b
    h@((_ :+ i) :| tl) = GrahamScan.upperHullFromSorted' $ pts
    -- every edge of the upper hull corresponds to some line. In the
    -- primal this line represents a vertex of the lower envelope. The
    -- x-coordinate of this point is the slope of the line.

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

atTime     :: (Num r, AsPoint p r) => r -> p -> Point 2 r
atTime t p = atTime' t $ asPoint p

-- | Computes the position of the given point at time t
atTime'                  :: Num r => r -> Point 3 r -> Point 2 r
atTime' t (Point3 x y z) = Point2 x (z - t*y)

-- | cons, but with a maybe element that we are trying to cons
(?:)     :: Maybe a -> [a] -> [a]
mx ?: xs = maybe xs (:xs) mx



partitionExisting :: Foldable f => f (Existing a) -> ([a], [a])
partitionExisting = foldr (eitherAct left right) ([],[])
 where
  left  a ~(l, r) = (a:l, r)
  right a ~(l, r) = (l, a:r)
  eitherAct f g = \case
    Left x  -> f x
    Right x -> g x



--------------------------------------------------------------------------------


--        expected:

-- [Triangle (Point3 [5,5,0] :+ 2) (Point3 [1,1,10] :+ 1) (Point3 [0,10,20] :+ 0)
-- ,Triangle (Point3 [5,5,0] :+ 2) (Point3 [1,1,10] :+ 1) (Point3 [12,1,1] :+ 3)
-- ,Triangle (Point3 [5,5,0] :+ 2) (Point3 [0,10,20] :+ 0) (Point3 [22,20,1] :+ 4)
-- ,Triangle (Point3 [5,5,0] :+ 2) (Point3 [12,1,1] :+ 3) (Point3 [22,20,1] :+ 4)]


-- [Triangle (Point3 [0,10,20] :+ 0) (Point3 [1,1,10] :+ 1) (Point3 [12,1,1] :+ 3)
-- ,Triangle (Point3 [0,10,20] :+ 0) (Point3 [12,1,1] :+ 3) (Point3 [22,20,1] :+ 4)]

type R = RealNumber 10

--------------------------------------------------------------------------------
-- * Debugging stuff

rangeOfSS    :: (HasNeighbours p, Ord p, WithExtra p Int) => Movie p t -> Movie p t -> String
rangeOfSS l r = rangeOfS l <> "_" <> rangeOfS r

rangeOfS   :: (HasNeighbours p, Ord p, WithExtra p Int) => Movie p t -> String
rangeOfS m = let (a,b) = bimap (show @Int. askExtra) (show @Int . askExtra) $ rangeOf m
             in a <> "_" <> b

rangeOf   :: (HasNeighbours p, Ord p) => Movie p t -> (p,p)
rangeOf m = let s = fromLeftMost $ initialScene m
            in (minimum s, maximum s)
