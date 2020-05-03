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
import           Algorithms.Geometry.ConvexHull.Movie
import           Algorithms.Geometry.ConvexHull.Scene
import           Control.Lens
import           Data.Coerce
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.Point
import           Data.Geometry.Polygon.Convex (lowerTangent')
import           Data.Geometry.Triangle
import           Data.Geometry.Vector
import qualified Data.List as List
import           Data.List.Alternating
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.Util (minimaOn)
import           Data.Maybe (mapMaybe, catMaybes)
import           Data.Ord (Down(..))
import           Data.Semigroup
import           Data.Util
import           Prelude hiding (Either(..))


import           Algorithms.Geometry.ConvexHull.DebugMinimalist
import           Data.Geometry.Ipe
import           Data.RealNumber.Rational

--------------------------------------------------------------------------------

type LowerHull d q r = [Triangle 3 q r]

-- | Computes the lower hull, i.e. set of triangles s.t.
-- the points are above all supporting planes
lowerHull' :: forall r q. (Ord r, Fractional r
                          , Show r, IpeWriteText r, q ~ Int
                          )
           => NonEmpty (Point 3 r :+ q) -> LowerHull 3 q r
lowerHull' pts = outputTriangles
               . divideAndConquer1With (merge t0) baseCase
               . NonEmpty.groupWith1 (^.core.xCoord)
               . NonEmpty.sortBy cmpXYZ
               $ pts
  where
    t0 = -1000000000000000

--------------------------------------------------------------------------------
-- * Producing the Output

outputTriangles :: (HasNeighbours p, WithExtra p q, AsPoint p r, HasScene p)
                => Movie p t -> LowerHull 3 q r
outputTriangles = concatMap (\(s,(e :+ _)) -> reportTriangles s e) . withNeighbours . play
  where
    reportTriangles (Identity s) =
      mapMaybe (reportTriangle s . runIdentity) . toList . view actions


reportTriangle   :: (HasNeighbours p, WithExtra p q, AsPoint p r)
                 => Scene p -> Action p -> Maybe (Triangle 3 q r)
reportTriangle s = fmap toTriangle . reportTriplet
  where
    toPoint p     = asPoint p :+ askExtra p
    toTriangle    = view (re _TriangleThreePoints) . fmap toPoint
    reportTriplet = \case
      InsertAfter i j  ->          Three i j    <$> getNext i s
      InsertBefore i h -> (\l   -> Three l h i) <$> getPrev i s
      Delete j         -> (\l r -> Three l j r) <$> getPrev j s <*> getNext j s

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
               in debugMovieTo ("merge_" <> rangeOfSS l r) $
                  movie pts h $ tr ("events " <> rangeOfSS l r) $ traceBridge b (play $ m)



type Bridge p = Two p
pattern Bridge     :: p -> p -> Bridge p
pattern Bridge u v = Two u v
{-# COMPLETE Bridge #-}

type Hull p = Scene p

type Hulls p = Two (Hull p)

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
findBridge       :: (AsPoint p t, Ord t, Num t) => t -> NonEmpty p -> NonEmpty p -> Bridge p
findBridge t l r = (\(Two l' r') -> Bridge (l'^.core) (r'^.core)) $ findBridge' t l r

--------------------------------------------------------------------------------
-- * Tracing the Bridge

type Animation p t = Alternating (Two (Scene p)) (Event' Existing p t)

-- | Start Tracing the Bridge
traceBridge         :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t, Ord p
                       , Show p, Show t, Show (Scene p)
                       )
                    => Bridge p -> Animation p t -> [Event p t]
traceBridge b0 anim = trace' b0 anim (initialBridgeChange (currentScene anim) b0)
  where
    initialBridgeChange = nextBridgeChange (const True)
      -- initially, there is no requirement on the time of the next bridge event.
  -- TODO: use a DList instead


-- | The actual tracing happens in this function. Given the current bridge, and the
-- animation representing the .
trace'        :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t, Ord p
                 , Show p, Show t, Show (Scene p)
                 )
              => Bridge p -> Animation p t -> Maybe (BridgeInfo p t)
              -> [Event p t]
trace' b a mt = case nextEvent a mt of
    Nothing  -> []
    Just ne  -> let STR evt b' a' = continue (tr "trace'; ne: " ne)
                in evt ?: trace (nextTime ne) b' a'
  where
    continue = \case
      ExistingEvent e a'    -> STR (fromExistingEvent b e)     b  a'
      BridgeEvent e         -> let SP e' b' = tr "bridgeOnlyEvent: " $ bridgeEventOnly b e in
                               STR e'                          b' a
      CombinedEvent e bi a' -> let SP e' b' = combinedEvent b e bi (currentScene a) in
                               STR e' b' a'


trace           :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t, Ord p
                   , Show p, Show t, Show (Scene p)
                   )
                => t -> Bridge p -> Animation p t -> [Event p t]
trace  t b anim = trace' b anim (nextBridgeChange (> t) (currentScene anim) b)


fromExistingEvent               :: Ord p
                                => Bridge p -> Event' Existing p t -> Maybe (Event p t)
fromExistingEvent b (t :+ acts) = fmap (t:+) . NonEmpty.nonEmpty . filterActions b $ acts

filterActions              :: Ord p
                           => Bridge p -> NonEmpty (Existing (Action p)) -> [Identity (Action p)]
filterActions (Bridge l r) = foldr f []
  where
    f (Left act)  xs = if act `occursLeftOf`  l then (Identity act):xs else xs
    f (Right act) xs = if act `occursRightOf` r then (Identity act):xs else xs

    occursLeftOf  a p = rightMost a <= p
    occursRightOf a p = p <= leftMost a

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


-- | Computes the next bridge change that satisifies the given
-- predicate.
nextBridgeChange                            :: (Ord t, Fractional t, HasNeighbours p, AsPoint p t
                                               , Show t, Show p
                                               )
                                            => (t -> Bool)
                                            -> Hulls p -> Bridge p -> Maybe (BridgeInfo p t)
nextBridgeChange p (Two sl sr) (Bridge u v) =
    fmap asEvent . NonEmpty.nonEmpty . minimaOn (^.core) . filter (p . (^.core)) . catMaybes
    . tr "nextBridgeChange, candidates"
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

partitionBridgeActions :: NonEmpty (BridgeAction p) -> (Maybe (Action p), Maybe (Action p))
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



bridgeFromAction                       :: Bridge p -> BridgeAction p -> Bridge p
bridgeFromAction (Bridge l r) (BA a p) = case a of
    InsertAfter _ l'  -> Bridge l' r
    InsertBefore _ r' -> Bridge l  r'
    Delete _          -> case p of Left  l' -> Bridge l' r
                                   Right r' -> Bridge l  r'


----------------------------------------
-- ** Handling an event

-- | Given the current bridge, and ifnormation about the bridge event,
-- handles that bridge event. This function assumes that there are no
-- existing events at the time of teh bridge event. The result is the
-- event that we should output and the new bridge.
bridgeEventOnly                  :: (Num t, Ord t, AsPoint p t, Eq p
                                    , Show p
                                    )
                                 => Bridge p -> BridgeInfo p t
                                 -> SP (Maybe (Event p t)) (Bridge p)
bridgeEventOnly b bi@(t :+ acts) = case tr "bridgeEventOnly: acts " $ acts of
    (a  :| [])   -> SP (Just $ t :+ acts') (bridgeFromAction b a)
    (al :| [ar]) -> let b' = findBridge (1 + t) (collect al) (collect ar)
                    in SP (mkNewEvent bi b') b'
    _            -> error "bridgeEventOnly: absurd, <=2 actions possible"
  where
    acts' = fmap (Identity . toAction) acts

combinedEvent                                     :: (Fractional t, Ord t, AsPoint p t
                                                     , HasNeighbours p, Ord p)
                                                  => Bridge p
                                                  -> NonEmpty (Existing (Action p))
                                                  -> BridgeInfo p t
                                                  -> Hulls p -- ^ old scene
                                                  -> SP (Maybe (Event p t)) (Bridge p)
combinedEvent (Bridge l r) acts bi@(t :+ bActs) (Two sl sr) = SP (mkNewEvent bi b') b'
  where
    b' = findBridge (1 + t) (NonEmpty.reverse ls) rs
    ls = colinears t l r sl lAct leftActs
    rs = colinears t r l sr rAct rightActs

    (leftActs,rightActs) = partitionExisting acts
    (lAct,rAct)          = partitionBridgeActions bActs


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
colinears                  :: (AsPoint p t, HasNeighbours p, Ord p, Ord t, Fractional t)
                           => t
                           -> p -- ^ The bridge point we are processing
                           -> p -- ^ the other birdge point
                           -> Scene p
                           -> Maybe (Action p) -- ^ the bridge action on p
                           -> [Action p] -- ^ Existing actions related to this side
                           -> NonEmpty p -- ^ in left to right order
colinears t p q s mba acts = colinearsFrom (findPointInScene t p q s mba acts) q t s

findPointInScene ::  (HasNeighbours p, AsPoint p t, Ord t, Fractional t, Ord p) =>
                     t -> p -> p -> Scene p -> Maybe (Action p) -> [Action p] -> p
findPointInScene t p q oldS mba acts = case mba of
    Nothing -> p
    Just a  -> case insertedPoint a ?: newOnBridge of
                 (z:_) -> z  -- z is a newly inserted point on the bridge
                 []    -> maybeDeleted
  where
    newOnBridge = filter (areColinearAt t p q) . mapMaybe insertedPoint $ acts
    insertedPoint = \case
      InsertAfter  _ z -> Just z
      InsertBefore _ z -> Just z
      Delete _         -> Nothing

    leftMost' = minimum
    -- TODO: state why this is suposedly safe
    maybeDeleted = leftMost' . filter (areColinearAt t p q) . mapMaybe deletedNeighbour $ acts
    deletedNeighbour = \case
      Delete a         -> getPrev a oldS
      _                -> Nothing

areColinearAt         :: (AsPoint p t, Ord t, Fractional t) => t -> p -> p -> p -> Bool
areColinearAt t a b c = ccw (atTime t a) (atTime t b) (atTime t c) == CoLinear

colinearsFrom         :: (HasNeighbours p, AsPoint p t, Ord t, Fractional t)
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
    -- at every breakpoint we insert b and delete a.
    fromBreakPoint (Two a b) = coerce <$> NonEmpty.fromList [InsertAfter a b, Delete a]

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



--------------------------------------------------------------------------------

myPts :: NonEmpty (Point 3 R :+ Int)
myPts = NonEmpty.fromList $ [ Point3 5  5  0  :+ 2
                            , Point3 1  1  10 :+ 1
                            , Point3 0  10 20 :+ 0
                            , Point3 12 1  1  :+ 3
                            , Point3 22 20  1  :+ 4
                            ]


buggyPoints2 :: NonEmpty (Point 3 R :+ Int)
buggyPoints2 = fmap (bimap (10 *^) id) . NonEmpty.fromList $ [ Point3 (-5) (-3) 4 :+ 0
                                                             , Point3 (-5) (-2) 5 :+ 1
                                                             , Point3 (-5) (-1) 4 :+ 2
                                                             , Point3 (0) (2)   2 :+ 3
                                                             , Point3 (1) (-5)  4 :+ 4
                                                             , Point3 (3) (-3)  2 :+ 5
                                                             , Point3 (3) (-1)  1 :+ 6
                                                             ]
