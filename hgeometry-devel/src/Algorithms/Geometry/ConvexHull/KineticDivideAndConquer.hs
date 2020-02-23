{-# LANGUAGE BangPatterns #-}
module Algorithms.Geometry.ConvexHull.KineticDivideAndConquer where

import           Algorithms.DivideAndConquer
import           Control.Applicative (liftA2,(<|>))
import           Control.Lens ((^.), bimap, Lens', _1, _2)
import           Control.Monad ((<=<))
import           Control.Monad (replicateM)
import           Control.Monad.State.Class (gets, get, put)
import           Control.Monad.State.Strict (StateT, evalStateT)
import           Control.Monad.Trans
import           Data.Bitraversable
import           Data.Ext
import           Data.Foldable (forM_)
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Line(lineThrough, onSideUpDown, SideTestUpDown(..))
import           Data.Geometry.PolyLine
import           Data.Geometry.Polygon.Convex (lowerTangent')
import           Data.Geometry.Triangle
import           Data.Geometry.Vector
import           Data.IndexedDoublyLinkedList
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Ord (comparing)
import           Data.Semigroup (sconcat)
import           Data.UnBounded
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

import           Data.Geometry.Ipe
import           Data.Geometry.Ipe.Color
import           Data.Maybe (catMaybes)
import           Data.RealNumber.Rational


import           Control.Monad.Reader.Class
import           Debug.Trace
import           System.IO.Unsafe
import           System.Random
import qualified Data.Text as Text



--------------------------------------------------------------------------------

-- TODO: Replace EventKind by a partial function

-- TODO: We seem to assume that no four points are coplanar, and no
-- three points lie on a vertical plane. Figure out where we assume that exactly.

-- no four points coplanar:
--     - otherwise events may happen simultaneously
--     - output may not be a set of triangles
-- no three points on a vertical plane:
--     Otherwise the test for computing the next time t does not exist
--       (slope of the supp. plane of three such points is +infty)


-- FIXME: The kinetic sim. now treats three points on a vertical plane
-- as happening at time t=-\infty. That means we should find faces on
-- the lower envelope that have that property separately.
--
-- I think we can do that by projecting the points down onto the
-- xy-plane, and computing a 2D convex hull.


-- FIXME: We start with some arbitrary starting slope. Fix that


-- type ConvexHull d p r = [Three Index]
type ConvexHull d p r = [Triangle 3 p r]


-- lowerHull :: (Ord r, Fractional r) => [Point 3 r :+ p] -> ConvexHull 3 p r
-- lowerHull = maybe mempty lowerHull' . NonEmpty.nonEmpty


lowerHull'      :: forall r p. (Ord r, Fractional r, Show r, IpeWriteText r)
                => NonEmpty (Point 3 r :+ p) -> ConvexHull 3 p r
lowerHull' pts' = map withPt $ runDLListMonad pts computeHull
  where
    computeHull :: HullM s r [Three Index]
    computeHull = output <=< divideAndConquer1 mkLeaf $ NonEmpty.fromList [0..(n-1)]

    n = V.length pts
    (pts,exts) = bimap V.fromList V.fromList . unExt . NonEmpty.sortBy cmpXYZ $ pts'
    unExt = foldr (\(p :+ e) (ps,es) -> (p:ps,e:es)) ([],[])

    -- withPt = id
    withPt (Three a b c) = let pt i = pts V.! i :+ exts V.! i in Triangle (pt a) (pt b) (pt c)

    -- sort on x-coord first (and on equal y and z coordinate later)
    cmpXYZ = comparing (^.core)


-- | Creates a Leaf
mkLeaf   :: Int -> HullM s r (MergeStatus r)
mkLeaf i = pure $ MergeStatus i i []

type HullM s r = DLListMonad s (Point 3 r)

--------------------------------------------------------------------------------

-- | Computes a lowerbound on the z-value with which to start
-- pre: not all points on a vertical plane
lowerboundT     :: (Ord r, Fractional r) => NonEmpty (Point 3 r) -> r
lowerboundT pts = ((-1)*) . maximum . catMaybes
                $ zipWith slope (NonEmpty.toList pts') (NonEmpty.tail pts')
  where
    pts' = NonEmpty.sortBy (comparing (^.yCoord) <> comparing (^.zCoord)) pts

    slope p q = let d = q^.yCoord - p^.yCoord
                in if d == 0 then Nothing else Just $ (abs $ q^.zCoord - p^.zCoord) / d


instance (Ord r, Fractional r, Show r, IpeWriteText r)
         => Semigroup (HullM s r (MergeStatus r)) where
  lc <> rc = do l <- lc
                r <- rc


                d <- debugHull l r
                pts <- getPoints

                let esIn = traceShow d $ mergeEvents (events l) (events r)
                    t    = (-10000000) -- TODO; at what time value should we start?
                STR h u v <- traceShow ("before merge:",d,t,events l, events r,map (^.core) esIn
                                       ) <$> findBridge t l r
                let b = traceShow ("bridge:", u, v, "hull:",h) $ (Bridge u v)

                es <- runKinetic Bottom esIn
                      $ traceShow (drawDebug ("before_merge_" <> rangeS l r) Bottom h b pts)
                                        b
                writeList $ traceShow ("writing hull: ",h) h
                let !ms = MergeStatus (hd l) (lst r) es
                fp <- renderMovieIO ("movie_" <> rangeS l r) ms
                pure $ traceShow fp ms
                --
                -- pure $ traceShow (drawDebug "combined" ms (Bridge u v) pts) ms
    where
      rangeS l r = show (hd l) <> "-" <> show (lst r)


      debugHull l r = (\a b c d -> ( "MERGING ", hd l, "-",lst l, " WITH "
                                   , hd r, "-", lst r
                                   -- , a
                                   -- , b
                                   -- , c
                                   -- , d
                                   -- , events l
                                   -- , events r
                                   )
                      )
                      <$> (toListFrom $ hd l) <*> (toListFrom $ hd r)
                      <*> listHull l <*> listHull r

      listHull s = do xs <- NonEmpty.toList <$> toListFrom (hd s)
                      mapM (atTime t) xs
         where
           t = (-10000000)

--------------------------------------------------------------------------------
-- * Producing the Output Hull

-- | Reports all the edges on the CH
output    :: Show r => MergeStatus r -> HullM s r [Three Index]
output ms | traceShow ("output: ", events ms) False = undefined
output ms = catMaybes <$> mapM handle  (events ms)
  where
    handle e = do mt <- reportTriangle (eventKind e)
                  applyEvent e
                  pure mt

    -- u v w =  <$> pointAt u <*> pointAt v

  -- do h  <- toListFrom (hd ms)
  --              -- es  <- sequence $ zipWith mkEdge (NonEmpty.toList h) (NonEmpty.tail h)
  --              es' <- concat <$> mapM (handle . eventKind) (events ms)
  --              pure es' -- $ es <> es'
  -- where
  --   handle ek = do es <- reportEdges ek
  --                  applyEvent ek
  --                  mapM (\(Two u v) -> mkEdge u v) es

  --   mkEdge u v = Two <$> pure u <*> pure v
  --   -- mkEdge u v = Two <$> pointAt u <*> pointAt v

reportTriangle :: EventKind -> HullM s r (Maybe (Three Index))
reportTriangle = \case
    InsertAfter i j  -> fmap (\r   -> Three i j r)   <$> getNext i
    InsertBefore i h -> fmap (\l   -> Three l h i)   <$> getPrev i
    Delete j         -> liftA2 (\l r -> Three l j r) <$> getPrev j <*> getNext j

-- | Given an event, produces the list of (at most two) new edges on the hull.
reportEdges :: EventKind -> HullM s r [Two Index]
reportEdges = \case
    InsertAfter i j  -> comb [Two i j] (\r -> Two j r) <$> getNext i
    InsertBefore i j -> comb [Two j i] (\l -> Two l j) <$> getPrev i
    Delete j         -> asList Two <$> getPrev j <*> getNext j
  where
    comb xs f = maybe xs (\z -> f z : xs)
    asList g ml mr = maybeToList $ g <$> ml <*> mr

--------------------------------------------------------------------------------
-- * Finding the Bridge

type Bridge = Two Index

pattern Bridge     :: Index -> Index -> Bridge
pattern Bridge a b = Two a b
{-# COMPLETE Bridge #-}

leftBridgePoint, rightBridgePoint :: Lens' Bridge Index
leftBridgePoint  = _1
rightBridgePoint = _2

-- | Computes the Bridge of the Hulls (the Hulls currently encoded in
-- the underlying Doublylinkedlist)
--
-- running time: \(O(n)\)
findBridge       :: (Ord r, Fractional r, Show r)
                 => r
                 -> MergeStatus r
                 -> MergeStatus r
                 -> HullM s r (STR (NonEmpty Index) Index Index)
findBridge t l r = do lh <- mapM (atTime' t) =<< toListFromR (lst l)
                      rh <- mapM (atTime' t) =<< toListFrom  (hd r)
                      let Two (u :+ ls) (v :+ rs) = findBridge' lh rh
                      pure $ STR (NonEmpty.fromList $ reverse ls <> [u,v] <> rs) u v
  where
    atTime' t' i = (:+ i) <$> atTime t' i

    findBridge' l0 r0 = f <$> lowerTangent' l0 r0
    f (c :+ es) = c^.extra :+ ((^.extra) <$> es)

--------------------------------------------------------------------------------

-- | The Kinetic Simulation
type Simulation s r = StateT Bridge (HullM s r)

-- | For each of the events form the left and right hulls, construct
-- the appropriate event handler, and merge the two streams of events
-- into one stream.
--
-- Note that the event handler is a piece of code that, when run, may
-- produce an output event (i.e. an event that states that there is a
-- change in the complete hull). Note that whether or not this event
-- is produced depends on the position of the bridge at the time when
-- we handle the event.
mergeEvents       :: (Ord r, Show r)
                  => [Event r] -> [Event r] -> [r :+ Simulation s r (Maybe (Event r))]
mergeEvents ls rs = mergeSortedListsBy (comparing $ (^.core)) (f handleLeft  <$> ls)
                                                              (f handleRight <$> rs)
  where
    f handler e@(Event t _) = t :+ handler e

-- | run the kinetic simulation, computing the events at which the
-- hull changes. At any point during the simulation:
--
-- - the multable array in env represents the hulls L and R
-- - we maintain the current bridge u on L and v on R such that
-- - L[1..u] <> R[v..n] is the output hull H
runKinetic        :: (Ord r, Fractional r, Show r, IpeWriteText r )
                  => Bottom r          -- starting time
                  -> [r :+ Simulation s r (Maybe (Event r))]  -- existing events
                  -> Bridge -- initial bridge
                  -> HullM s r [Event r]
runKinetic t es b = evalStateT (handleEvent t es) b


fromBridge :: Bridge -> DLListMonad s b (NonEmpty Index)
fromBridge (Bridge l r) = (\lh rh -> NonEmpty.reverse lh <> rh
                             ) <$> toListFromR l <*> toListFrom r

-- | The actual code for handling an event in the kinetic
-- simulation. At every step, we recompute what the next bridge event
-- is, and apply the first event that occurs (either this bridge
-- event, or one of the existing events).
handleEvent        :: (Ord r, Fractional r, Show r, IpeWriteText r)
                   => Bottom r -> [r :+ Simulation s r (Maybe (Event r))] -> Simulation s r [Event r]
handleEvent now es | traceShow ("HandleEvent ", now, map (^.core) es) False = undefined
handleEvent now es = do -- nextBridgeEvent now >>= \mbe -> case (es, mbe) of
       mbe <- nextBridgeEvent now
       pts <- lift $ getPoints
       b <- get
       h <- lift $ fromBridge b
       case traceShow (drawDebug ("handleEvent_" <> show now) now h b pts)
            (es, mbe) of
         ([],Nothing)                     -> pure []
         ([],Just be)                     -> handleBridge be []
         (e:es', Nothing)                 -> handleExisting e es'
         (e:es', Just be) | e `before` be -> handleExisting e es'
                          | otherwise     -> handleBridge be es
  where
    cons me outEvents = maybeToList me <> outEvents

    before (a :+ _) (b :+ _) = a < b
      -- if bridge event and other event occur simultaneously, do the bridge event first
    handleExisting (t :+ h) es' = cons <$> h <*> handleEvent (ValB t) es'
    handleBridge   (t :+ h) es' = (:)  <$> h <*> handleEvent (ValB t) es'




----------------------------------------
-- * Handling the events in the Existing Hulls

handleLeft   :: (Ord r, Show r) => Event r -> Simulation s r (Maybe (Event r))
handleLeft e = handleExisting' e leftBridgePoint (<=) l
  where
    l = case eventKind e of -- find the rightmost point involved in the event
          InsertAfter _ j  -> j
          InsertBefore _ j -> j
          Delete j         -> j

handleRight   :: (Ord r, Show r)
              => Event r -> Simulation s r (Maybe (Event r))
handleRight e = handleExisting' e rightBridgePoint (>=) r
  where
    r = case eventKind e of -- find the leftmost point involved in the event
          InsertAfter j _  -> j
          InsertBefore j _ -> j
          Delete j         -> j

-- | Handler for an event on the right hull
--
handleExisting'  :: forall s r. (Show r)
                 => Event r -> Lens' Bridge Index -> (r -> r -> Bool) -> Index
                 -> Simulation s r (Maybe (Event r))
handleExisting' e bridgePoint cmp p =
    do lift $ applyEvent e
       v <- gets (^.bridgePoint)
       outputEvent <$> pointAt' p <*> pointAt' v
  where
    outputEvent pp bp = if (pp^.xCoord) `cmp` (bp^.xCoord) then Just e else Nothing


-- | Applies the actual event, mutating the current representation of the hulls.
applyEvent' :: EventKind -> HullM s r ()
applyEvent' = \case
  InsertAfter i j  -> insertAfter i j
  InsertBefore i h -> insertBefore i h
  Delete j         -> delete j

applyEvent   :: (Show r) => Event r -> HullM s r ()
applyEvent e = applyEvent' $ traceShow ("applyEvent ",e) (eventKind e)

----------------------------------------
-- * Bridge Events

-- | Given the current time, computes the next bridge event (if it
-- exists) and a handler to handle this bridge event.
nextBridgeEvent     :: (Ord r, Fractional r, Show r )
                    => Bottom r -> Simulation s r (Maybe (r :+ Simulation s r (Event r)))
nextBridgeEvent now = do b <- get
                         es <- nextBridgeEvents b
                         fmap mkHandler <$> nextBridgeEvent' now (debug es b)
  where
    mkHandler (t :+ (b',k)) = t :+ do put $ traceShow ("setting bridge to ",b'," at time ",t) b'
                                      pure $ Event t k

    debug es b = traceShow ("candidate events with ",b," ", es) b

-- evalNextBridgeEvents   :: Bridge -> Simulation s r [r :+ (Bridge,EventKind)]
-- evalNextBridgeEvents b = do cans <- nextBridgeEvents b
--                             mapM (\(computeT :+ z) -> (:+ z) <$> computeT
--                                  ) $ cans

-- | Finds the next event involving the current bridge.
-- The arguments are the current time, and the current bridge indices.
nextBridgeEvent'       :: (Ord r, Fractional r, Show r)
                       => Bottom r -> Bridge
                       -> Simulation s r (Maybe (r :+ (Bridge,EventKind)))
nextBridgeEvent' now b = tr . minimumOn (^.core) . dropBottoms . filter (\e -> now < e^.core)
                      <$> nextBridgeEvents b
  where
    tr x = traceShow ("nextBridgeEvent', next event found: ",(^.core) <$> x) x
    -- trz x = traceShow ("filtered events",now, (^.core) <$> x) x

    -- since e^.core > now, we now e^.core /= Bottom, so we can drop
    -- the 'Bottom' part, and work with actual 'r' values.
    dropBottoms = mapMaybe (bitraverse bottomToMaybe pure)

-- | Computes all candidate bridge events
nextBridgeEvents                  :: forall r s. (Ord r, Fractional r)
                                  => Bridge
                                  -> Simulation s r [Bottom r :+ (Bridge,EventKind)]
nextBridgeEvents (Bridge l r) = catMaybes <$> mapM runCand cands
  where
    runCand (c,f) = lift c >>= \case
      Nothing -> pure Nothing
      Just x  -> Just <$> bitraverse id pure (f x)

    -- candiate next events. Either one of the neighbours of the left bridge endpoint l
    -- becomes colinear with the bridge, or one of the neighbours of r becomes colinear
    -- with the bridge.
    cands :: [ ( HullM s r (Maybe Index), Index -> Simulation s r (Bottom r) :+ (Bridge, EventKind) )]
    cands = [ (getPrev l, \a -> nextTime a a l r :+ (Bridge a r, Delete l))
            , (getNext l, \b -> nextTime b l b r :+ (Bridge b r, InsertAfter  l b))
            , (getPrev r, \c -> nextTime c l c r :+ (Bridge l c, InsertBefore r c))
            , (getNext r, \d -> nextTime d l r d :+ (Bridge l d, Delete r))
            ]

    nextTime p a b c = colinearTime a b c -- >>= isValidCandidate l r p


    -- TODO: verify that nextTime a l r == nextTime l r a ?



----------------------------------------
-- * Helpers for computing the next interesting time in the simulation

isValidCandidate       :: (Num r, Ord r)
                       => Index -> Index -> Index -> Bottom r -> Simulation s r (Bottom r)
isValidCandidate l r p = lift . \case
   Bottom -> pure Bottom -- bottoms are already invalid?
   ValB t -> let t' = t + 1 in f t <$> atTime t' l <*> atTime t' r <*> atTime t' p
  where
    f t l' r' p' | (p' `onSideUpDown` (lineThrough l' r')) == Below = ValB t
                 | otherwise                                        = Bottom


-- | compute the time at which r becomes colinear with the line throuh
-- p and q.
colinearTime       :: (Ord r, Fractional r) => Index -> Index -> Index -> Simulation s r (Bottom r)
colinearTime p q r = colinearTime' <$> pointAt' p <*> pointAt' q <*> pointAt' r

-- | compute the time at which r becomes colinear with the line through
-- p and q.
--
-- pre: x-order is: p,q,r
colinearTime'  :: (Ord r, Fractional r) => Point 3 r -> Point 3 r -> Point 3 r -> Bottom r
colinearTime' (Point3 px py pz) (Point3 qx qy qz) (Point3 rx ry rz) =
    if b == 0 then Bottom else ValB $ a / b
  where        -- by unfolding the def of ccw
    ux = qx - px
    vx = rx - px
    a = ux*(rz - pz)  - vx*(qz - pz)
    b = ux*(ry - py)  - vx*(qy - py)
  -- b == zero means the three points are on a vertical plane. This corresponds
  -- to t = -\infty.


--------------------------------------------------------------------------------

data Event r = Event { eventTime :: !r
                     , eventKind :: !EventKind
                     } deriving (Show,Eq)

data EventKind = InsertAfter  !Index !Index -- ^ current Index first, then the Item we insert
               | InsertBefore !Index !Index -- ^ current Index first, then the Item we insert
               | Delete !Index
               deriving (Show,Eq,Ord)

data MergeStatus r = MergeStatus { hd     :: !Index -- ^ first item in the list
                                 , lst    :: !Index -- ^ last item in the list
                                 , events :: ![Event r] -- ^ Events when this Hull changes
                                 } deriving (Show,Eq)

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

--------------------------------------------------------------------------------
-- * Pure Helpers

minimumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn f = \case
    [] -> Nothing
    xs -> Just $ List.minimumBy (comparing f) xs

--------------------------------------------------------------------------------
-- * Testing stuff

myPts :: NonEmpty (Point 3 (RealNumber 10) :+ Int)
myPts = NonEmpty.fromList $ [ Point3 5  5  0  :+ 2
                            , Point3 1  1  10 :+ 1
                            , Point3 0  10 20 :+ 0
                            , Point3 12 1  1  :+ 3
                            , Point3 22 20  1  :+ 4
                            ]

-- myResult = [1 2 3
--             2 3 4
--             0 1 2
--             0 2 4
--            ]

myPts' :: NonEmpty (Point 3 (RealNumber 10) :+ Int)
myPts' = NonEmpty.fromList $ [ Point3 5  5  0  :+ 2
                             , Point3 1  1  10 :+ 1
                             , Point3 0  10 20 :+ 0
                             , Point3 12 1  1  :+ 3
                             ]

-- 1 2 3
-- 0 1 2
-- 0 2 3


test = mapM_ print $ lowerHull' myPts

test' = mapM_ print $ lowerHull' myPts'


--------------------------------------------------------------------------------

renderMovieIO s ms = do pgs <- renderMovie ms
                        pure $ unsafePerformIO $
                          do fp <- (\s1 -> "/tmp/out" <> s <> "_" <> s1 <> ".ipe") <$> randomS
                             writeIpeFile fp . IpeFile Nothing [basicIpeStyle] $ pgs
                             pure fp
  where
    randomS :: IO String
    randomS = replicateM 10 $ randomRIO ('a','z')


drawDebug             :: (IpeWriteText r, Ord r, Fractional r)
                      => String
                      -> Bottom r
                      -> NonEmpty Index
                      -> Bridge
                      -> V.Vector (Point 3 r)
                      -> FilePath
drawDebug s t' h blr pts = unsafePerformIO $
                       do fp <- (\s1 -> "/tmp/out" <> s <> "_" <> s1 <> ".ipe") <$> randomS
                          drawAllAt fp t h blr pts
                          pure fp
  where
    randomS :: IO String
    randomS = replicateM 10 $ randomRIO ('a','z')

    t = case t' of
          Bottom   -> -1000
          ValB t'' -> t''



drawAllAt fp t h blr pts = writeIpeFile fp . IpeFile Nothing [basicIpeStyle] $ draw t h blr pts


draw                       :: (Fractional r, Ord r, IpeWriteText r)
                           => r
                           -> NonEmpty Index
                           -> Bridge
                           -> V.Vector (Point 3 r) -> NonEmpty (IpePage r)
draw t h blr pts = fmap (\t' -> drawAt t' h blr pts)
                 $ NonEmpty.fromList [t-eps,t,t+eps]
  where
    eps = (1/10)


-- draw          :: (Fractional r, Ord r, IpeWriteText r)
--               => MergeStatus r -> Bridge -> V.Vector (Point 3 r) -> NonEmpty (IpePage r)
-- draw ms b pts = fmap (\t -> drawAt t ms b pts) times
--   where
--     times = NonEmpty.fromList . (initT :)
--           . concatMap (\e -> let t = eventTime e in [t-eps,t,t+eps]) $ events ms
--     eps = (1/1000)

--     initT = (-10000000)
--     -- we should recompute the bridge I guess

drawAt                       :: (Num r, Ord r, IpeWriteText r)
                             => r
                             -> NonEmpty Index
                             -> Bridge
                             -> V.Vector (Point 3 r) -> IpePage r
drawAt t h (Bridge l r) pts = fromContent $
   pts' <> [drawHull h pts2, drawBridge l r pts2, time]
  where
    pts2 = fmap (atTime' t) pts
    pts' = V.toList . V.imap (\i p -> iO $ labelled id defIO (p :+ i)) $ pts2
    time = iO $ ipeLabel ((fromJust $ ipeWriteText t) :+ Point2 (-50) (-50))

    -- lh = NonEmpty.head lh' NonEmpty.<| lh'
    -- rh = NonEmpty.head rh' NonEmpty.<| rh'



drawHull       :: NonEmpty Index -> V.Vector (Point 2 r) -> IpeObject r
drawHull h pts = iO . ipePolyLine . fromPoints $ [ (pts V.! i) :+ () | i <- NonEmpty.toList h ]

drawBridge         :: Index -> Index -> V.Vector (Point 2 r) -> IpeObject r
drawBridge l r pts = let s = ClosedLineSegment (ext $ pts V.! l) (ext $ pts V.! r)
                     in iO $ ipeLineSegment s ! attr SStroke red

getPoints :: DLListMonad s x (V.Vector x)
getPoints = asks values


-- | Reports all the edges on the CH
renderMovie    :: (Show r, Fractional r, Ord r, IpeWriteText r)
               => MergeStatus r -> HullM s r (NonEmpty (IpePage r))
-- renderMovie ms | traceShow ("output: ", events ms) False = undefined
renderMovie ms = do h0 <- toListFrom $ hd ms
                    res <- renderMovie'
                    writeList h0 -- restore the original list and state
                    pure $ res
  where
    renderMovie' = combine <$> mapM handle (events ms)
    combine xs = case NonEmpty.nonEmpty xs of
      Nothing  -> fromContent [] :| []
      Just pgs -> sconcat pgs

    handle e = do let t = eventTime e
                  pts <- getPoints
                  i <- fromEvent (eventKind e)
                  hBefore <- fromI i
                  applyEvent e
                  hAfter <- fromI i
                  let pages' = drawMovie t hBefore hAfter pts
                  pure pages'

fromI i = f <$> toListFromR i <*> toListFrom i
  where
    f (NonEmpty.toList -> l) r = NonEmpty.fromList $ reverse l <> NonEmpty.tail r

fromEvent = \case
    InsertAfter i j  -> pure i
    InsertBefore i h -> pure i
    Delete j         -> (\a b -> fromJust $ a <|> b) <$> getPrev j <*> getNext j


drawMovie             :: (Fractional r, Ord r, IpeWriteText r)
                      => r -> NonEmpty Index -> NonEmpty Index -> V.Vector (Point 3 r)
                      -> NonEmpty (IpePage r)
drawMovie t h0 h1 pts = NonEmpty.fromList
    [ fromContent $ d pts0 <> [ drawHull h0 pts0, time $ t - eps  ]
    , fromContent $ d pts2 <> [ drawHull h1 pts2, time $ t        ]
    , fromContent $ d pts1 <> [ drawHull h1 pts1, time $ t + eps  ]
    ]
  where
    pts0 = fmap (atTime' $ t - eps ) $ pts
    pts1 = fmap (atTime' $ t + eps ) $ pts
    pts2 = fmap (atTime' t) $ pts

    time x = iO $ ipeLabel ((fromJust $ ipeWriteText x) :+ Point2 (-50) (-50))

    eps = (1/10)


    d pts' = V.toList
           . V.slice s (1 + tt - s)
           . V.imap (\i p -> iO $ labelled id defIO (p :+ i))
           $ pts'

    hpts = [ (pts V.! i)^.xCoord | i <- NonEmpty.toList h0]

    -- xRange = ClosedRange (minimum hpts) (maximum hpts)
    (s,tt) = (NonEmpty.head h0, NonEmpty.last h0)



    -- atts =


--------------------------------------------------------------------------------



       -- uncaught exception: ArithException
       -- Ratio has zero denominator
       -- (after 2 tests)
       --   HI ((Point3 [0,-3,0] :+ 1) :| [Point3 [0,-1,0] :+ 2,Point3 [0,0,1] :+ 3,Point3 [3,0,1] :+ 4])

testPts :: NonEmpty (Point 3 (RealNumber 10) :+ Int)
testPts = NonEmpty.fromList $ [ Point3 0 (-3) 0 :+ 1
                              , Point3 0 (-1) 0 :+ 2
                              , Point3 0 0    1 :+ 3
                              , Point3 3 0    1 :+ 4
                              ]
-- looks like a vertical plane with three pts to me, figure out how to handle those

-- buggyPoints :: NonEmpty (Point 3 (RealNumber 10) :+ Int)
-- buggyPoints = NonEmpty.fromList $ [Point3 (-7) 2    4    :+ 1
--                                   ,Point3 (-4) 7    (-5) :+ 2
--                                   ,Point3 0    (-7) (-2) :+ 3
--                                   ,Point3 2    (-7) 0    :+ 4
--                                   ,Point3 2    (-6) (-2) :+ 5
--                                   ,Point3 2    5    4    :+ 6
--                                   ,Point3 5    (-1) 2    :+ 7
--                                   ,Point3 6    6    6    :+ 8
--                                   ,Point3 7    (-5) (-6) :+ 9
--                                   ]

buggyPoints :: NonEmpty (Point 3 (RealNumber 10) :+ Int)
buggyPoints = fmap (bimap (10 *^) id) . NonEmpty.fromList $ [Point3 (-7) 2    4    :+ 0
                                                            ,Point3 (-4) 7    (-5) :+ 1
                                                            ,Point3 0    (-7) (-2) :+ 2
                                                            ,Point3 2    (-7) 0    :+ 3
                                                            ,Point3 2    (-6) (-2) :+ 4
                                                            ,Point3 2    5    4    :+ 5
                                                            ,Point3 5    (-1) 2    :+ 6
                                                            ,Point3 6    6    6    :+ 7
                                                            ,Point3 7    (-5) (-6) :+ 8
                                                            ]


  -- 1) 3D ConvexHull tests same as naive quickcheck
  --      Falsifiable (after 8 tests):
  --        HI ((Point3 [-7,2,4] :+ 1) :| [Point3 [-4,7,-5] :+ 2,Point3 [0,-7,-2] :+ 3,Point3 [2,-7,0] :+ 4,Point3 [2,-6,-2] :+ 5,Point3 [2,5,4] :+ 6,Point3 [5,-1,2] :+ 7,Point3 [6,6,6] :+ 8,Point3 [7,-5,-6] :+ 9])
  --      expected: H [Triangle (Point3 [-7,2,4] :+ 1) (Point3 [-4,7,-5] :+ 2) (Point3 [0,-7,-2] :+ 3),Triangle (Point3 [-4,7,-5] :+ 2) (Point3 [0,-7,-2] :+ 3) (Point3 [7,-5,-6] :+ 9),Triangle (Point3 [-4,7,-5] :+ 2) (Point3 [6,6,6] :+ 8) (Point3 [7,-5,-6] :+ 9),Triangle (Point3 [0,-7,-2] :+ 3) (Point3 [2,-7,0] :+ 4) (Point3 [7,-5,-6] :+ 9)]
  --       but got: H [Triangle (Point3 [0,-7,-2] :+ 3) (Point3 [2,-7,0] :+ 4) (Point3 [7,-5,-6] :+ 9),Triangle (Point3 [-7,2,4] :+ 1) (Point3 [-4,7,-5] :+ 2) (Point3 [0,-7,-2] :+ 3),Triangle (Point3 [-4,7,-5] :+ 2) (Point3 [0,-7,-2] :+ 3) (Point3 [7,-5,-6] :+ 9)]
