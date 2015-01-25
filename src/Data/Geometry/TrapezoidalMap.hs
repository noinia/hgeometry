{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.TrapezoidalMap( Trapezoid(..)
                                   , Seg

                                   , TrapezoidId

                                   , TrapezoidalMap
                                   , buildTrapezoidalMap

                                   , locateTrapezoid


                                   , constructTour
                                   , tmap
                                   ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State.Persistent

import           Data.Ext

import           Data.ExplicitOrdSet(ExpSet)
import           Data.Map(Map)
import           Data.Maybe(listToMaybe, fromJust)
import           Data.Monoid
import           Data.Ord(comparing)
import           Data.Vector(Vector)

import qualified Data.ExplicitOrdSet as ES
import qualified Data.Map            as M
import qualified Data.Vector         as V
import qualified Data.List           as L

import           Data.Geometry.Properties
import           Data.Geometry.Point
import           Data.Geometry.Interval
import           Data.Geometry.Line


-- We bias to the RIGHT and to the TOP. i.e. if query point on the boundary of two traps
-- we report the right and/or topmost one

--------------------------------------------------------------------------------

newtype TrapezoidId = TrapezoidId Int deriving (Show,Read,Eq,Ord)


leftUnboundedId  = TrapezoidId 0
rightUnboundedId = TrapezoidId 1

firstId = TrapezoidId 2

nextId                 :: TrapezoidId -> TrapezoidId
nextId (TrapezoidId i) = TrapezoidId $ i + 1


type Seg e p r = LineSegment 2 p r :+ e


-- | A Trapezoid, the extra data on top and bottom is the line segment containing
-- the top and bottom edge of this trapezoid.
data Trapezoid e p r = Trapezoid           !TrapezoidId !r !r (Seg e p r) (Seg e p r)
                     | ToplessTrapezoid    !TrapezoidId !r !r (Seg e p r)
                     | BottomlessTrapezoid !TrapezoidId !r !r             (Seg e p r)
                     | LeftUnbounded !r -- right boundary
                     | RightUnbounded !r -- left boundary


trapezoidId                   :: Trapezoid e p r -> TrapezoidId
trapezoidId (Trapezoid           i _ _ _ _) = i
trapezoidId (ToplessTrapezoid    i _ _ _)   = i
trapezoidId (BottomlessTrapezoid i _ _ _)   = i
trapezoidId (LeftUnbounded             _)   = leftUnboundedId
trapezoidId (RightUnbounded          _)     = rightUnboundedId


leftX                                 :: Trapezoid e p r -> Maybe r
leftX (Trapezoid           _ l _ _ _) = Just l
leftX (ToplessTrapezoid    _ l _ _)   = Just l
leftX (BottomlessTrapezoid _ l _ _)   = Just l
leftX (LeftUnbounded           _)     = Nothing
leftX (RightUnbounded        l)       = Just l

rightX                                :: Trapezoid e p r -> Maybe r
rightX (Trapezoid           _ _ r _ _) = Just r
rightX (ToplessTrapezoid    _ _ r _)   = Just r
rightX (BottomlessTrapezoid _ _ r _)   = Just r
rightX (LeftUnbounded           r)     = Just r
rightX (RightUnbounded        _)       = Nothing



bottomSupport                                 :: Trapezoid e p r ->  Maybe (Seg e p r)
bottomSupport (Trapezoid           _ _ _ b _) = Just b
bottomSupport (ToplessTrapezoid    _ _ _ b)   = Just b
bottomSupport _                               = Nothing

topSupport                                 :: Trapezoid e p r ->  Maybe (Seg e p r)
topSupport (Trapezoid           _ _ _ _ t) = Just t
topSupport (BottomlessTrapezoid _ _ _   t) = Just t
topSupport _                               = Nothing


bottomLeftCorner t = bottom t ^? _Just.core.start

-- bottomRightCorner (Trapezoid _ _ r b _) = Just $ atXCoord r b

-- topLeftCorner (Trapezoid _ l _ _ t) = Just $ atXCoord l t

-- topRightCorner (Trapezoid _ _ r _ t) = Just $ atXCoord r t


atXCoord     :: (Ord r, Fractional r) => r -> LineSegment 2 p r -> Point 2 r
atXCoord x s = case s `intersect` verticalLine x of
  LineLineSegmentIntersection q -> q
  LineContainsSegment _         -> (s^.start.core) `min` (s^.end.core)
                                       -- s is a vertical segment, so report
                                       -- the lower endpoint
  _                             -> error "atXCoord"
                                       -- s was in the trapezoidal decompostion
                                       -- for xCoord x so by definition s
                                       -- should intersect the vertical line
                                       -- through x. Hence, this case should not occur.










bottom   :: (Ord r, Fractional r) => Trapezoid e p r ->  Maybe (Seg e () r)
bottom t = trim <$> leftX t <*> rightX t <*> bottomSupport t

top   :: (Ord r, Fractional r) => Trapezoid e p r ->  Maybe (Seg e () r)
top t = trim <$> leftX t <*> rightX t <*> topSupport t


trim              :: (Ord r, Fractional r)
                  => r -> r -> LineSegment 2 p r :+ e -> LineSegment 2 () r :+ e
trim l r (s :+ e) = LineSegment (atXCoord l s :+ ()) (atXCoord r s :+ ()) :+ e

--------------------------------------------------------------------------------

-- | A representation of a trapezoid that we store in our TrapezoidalMap.
-- The bototm segment is the original segment.
-- the leftOn is the x-coordinate through which the left boundary of this trapezoid
-- lies on.
--
-- If the bottomOn is empty, the trapezoid is unbounded from below.
data TrapezoidRep e p r = TrapezoidRep { _repId     :: !TrapezoidId
                                       , _repMinX   :: !r
                                       , _repBottom :: !(Maybe (LineSegment 2 p r :+ e))
                                       }
makeLenses ''TrapezoidRep


-- | When we run a query, we need the trapezoidRep found and the one above it
-- (if it exists) to reconstruct a Trapezoid
data TrapezoidQuery t e p r = TrapezoidQuery { _self  :: !(TrapezoidRep e p r :+ t)
                                             , _above :: !(Maybe (TrapezoidRep e p r :+ t))
                                             }
makeLenses ''TrapezoidQuery

----------------------------------------

-- | A vertical slab has a minimum x coordinate: minX s.t. the
-- slabData stored here is valid on the interval [minX, maxX), for some maxX >= minX
data VSlab t e p r = VSlab { _slabMinX :: !r
                           , _slabData :: ExpSet (TrapezoidRep e p r :+ t)
                           }
makeLenses ''VSlab


-- | Given a vertical slab of Trapezoids, locate the trapezoid containing the query point
findTrapezoidRep                :: (Ord r, Num r)
                                => Point 2 r -> VSlab t e p r -> TrapezoidQuery t e p r
findTrapezoidRep p (VSlab _ es) = TrapezoidQuery cur high
  where
    -- Since we explicitly store the bottomless and topless trapezoid, we are guaranteed
    -- to find a traprep.
    (belowT,aboveT) = ES.splitMonotone (maybe True pred . (^.core.repBottom)) es

    -- low         = ES.maximum belowT
    (cur:above) = ES.viewL aboveT
    high        = listToMaybe above

            -- Test if p lies above or on the segment s.
             -- if we turn clockwise then p is below the segment
             -- hence we should return False
    pred (s :+ _) = let (l,r) = orderedEndPoints s in ccw (l^.core) (r^.core) p /= CW
                   -- TODO: For degenereate cases, i.e. vertical segments we
                                                      -- may have to do some
                                                      -- additional check here.


----------------------------------------

-- | A TrapezoidalDecomposition is an sorted array of vertical slabs
newtype TrapezoidalDecomposition t e p r = TD { _unTD :: Vector (VSlab t e p r) }

-- | Given an x-coordinate, find the vertcal slab containing this x-coordinate.
-- The TrapezoidalDecomposition is 'valid' on a certain x-interval, as
-- precondition we require that the x-coordinate lies in this interval.
--
-- pre: x in xRange td
findSlab :: Ord r => r -- ^ The x-coordinate of the input point
         -> TrapezoidalDecomposition t e p r  -- ^ the td
         -> VSlab t e p r -- ^ The vertical slab containing the input point

findSlab x (TD v) = v V.! i
  where
    i   = binarySearch p 0 (V.length v - 1) -- the last one is guaranteed to be OK
    p i = x < _slabMinX (v V.! i)

-- | p, low, high, vec:
--  pre: p low = false
--       p high = true
-- result: smallest i fro which p i = true
binarySearch       :: (Int -> Bool) -> Int -> Int -> Int
binarySearch p l r = case (r - l) `compare` 1 of
  LT -> error "binarySearch"
  EQ -> r
  GT -> let h = l + r `div` 2 in
        if p h then binarySearch p l h else binarySearch p h r

----------------------------------------

-- | A trapezoidal map in which each trapezoid has extra data of type t,
-- each vertex data of type p, each edge of type e.
--
data TrapezoidalMap t e p r = TrapezoidalMap {
    _maxX               :: !r
  , _decomposition      :: TrapezoidalDecomposition t e p r
  , _rightMap           :: Map TrapezoidId r  -- ^ The x-coordinate of the
                                              -- right boundary of each
                                              -- trapezoid.
  , _leftUnboundedData  :: t
  , _rightUnboundedData :: t
  }
makeLenses ''TrapezoidalMap

_minX :: TrapezoidalDecomposition t e p r -> r
_minX = _slabMinX . V.head . _unTD


-- | An eventpoint may be a start or end point. Each eventpoint creates a new
-- vertical slab. If the line segments associated with an eventpoint are Just's
-- (which they normally are) that segment is inserted or deleted from the
-- structure.
--
-- In other words, if the segment associated with the data is a Nothing, we
-- create an extra slab without doing anything. This may be useful on some
-- ocasions. (Like computing Red-Blue Intersections).
data EventData e p r = Start !(Maybe (LineSegment 2 p r :+ e))
                     | End   !(Maybe (LineSegment 2 p r :+ e))
                     deriving (Show,Eq,Ord)



-- | Pre: All segments are oriented left to right
--- pre: List is non-empty
buildTrapezoidalMap      :: (Ord r, Monoid t)
                         => [LineSegment 2 p r :+ e] -> TrapezoidalMap t e p r
buildTrapezoidalMap segs = TrapezoidalMap mX (TD . V.fromList $ slabs) rMap mempty mempty
  where
    events         = L.sortBy (comparing fst) . concatMap f $ segs
    f seg@(s :+ _) = let p = s^.start.core
                         q = s^.end.core
                     in [(p,Start $ Just seg), (q, End $ Just seg)]

    (slabs,rMap,mX) = buildTrapezoidalMap' events





data SweepStatus t e p r = SweepStatus { _sSlab    :: VSlab t e p r
                                       , _sRMap    :: Map TrapezoidId r
                                       , _sMaxX    :: r
                                       , _lastUses :: TrapezoidId
                                       }
makeLenses ''SweepStatus


type Sweep t e p r = PersistentState (SweepStatus t e p r)


-- persistentSweep :: state




-- | pre: List is non-empty
buildTrapezoidalMap'        :: Ord r
                            => [(Point 2 r, EventData e p r)]
                            -> ([VSlab t e p r], Map TrapezoidId r, r)
buildTrapezoidalMap' (e:es) = f . runPersistentState initialState act
  where
    f (_, lastS, ss) =

    act = initialize >> mapM_ handleEvent es

    initialize = undefined
    initialState




-- modifyStatusStruct :: (ExpSet (TrapezoidRep e p r :+ t) -> ExpSet (TrapezoidRep e p r :+ t))
--                       -> Sweep t e p r ()


handleEvent        :: Sweep t e p r ()
handleEvent (p, d) = do
  set

  undefined


  -- f . L.foldl' sweep (initial, M.empty, Nothing, nextId') $ es
  -- where
  --

  --   initial = undefined
  --   nextId' = undefined



-- sweep                        :: (Ord r, Monoid t)
--                              => SweepStatus t e p r
--                              -> (Point 2 r, EventData e p r) -> SweepStatus t e p r
-- sweep (xDecomp@(slab:_), rMap, mX, i) (p,ed) = (slab':xDecomp, rMap', mX', nextId')
--   where
--     slab' = undefined
--     rMap' = undefined
--     mX'   = mX `max` (Just p^.xCoord)
--     nextId' = undefined

----------------------------------------
-- Queries

-- | Planar point location :): Running time: O(log n)
locateTrapezoid   :: (Fractional r, Ord r)
                  => Point 2 r -> TrapezoidalMap t e p r -> Trapezoid e p r :+ t
locateTrapezoid p (TrapezoidalMap maxX' xDecomp rMap lData rData)
    | px >= maxX' = RightUnbounded maxX' :+ rData
    | px <  minX' = LeftUnbounded  minX' :+ lData
    | otherwise   = reconstruct rMap . findTrapezoidRep p $ findSlab px xDecomp
  where
    px    = p^.xCoord
    minX' = _minX xDecomp

-- | Convert a TrapezoidalRep into a proper Trapezoid. This requires a TrapezoidQuery
-- as well as the x-coordinate of the right-window of the trapezoid. We store these
-- x-coordinates in a Map.
reconstruct         :: (Ord r, Fractional r)
                    => Map TrapezoidId r -> TrapezoidQuery t e p r -> Trapezoid e p r :+ t
reconstruct rMap tq = (:+ tData) $ case (mBottomSeg,mTopSeg) of
    (Just bottomSeg, Just topSeg) -> Trapezoid           tid leftX rightX bottomSeg topSeg
    (Just bottomSeg, Nothing)     -> ToplessTrapezoid    tid leftX rightX bottomSeg
    (Nothing,        Just topSeg) -> BottomlessTrapezoid tid leftX rightX           topSeg

  where
    ((TrapezoidRep tid leftX mBottomSeg) :+ tData) = tq^.self

    -- mTopSeg :: Maybe (LineSegment 2 p r :+ e)
    mTopSeg = tq ^? above._Just.core.repBottom._Just

    -- Every trapezoid should have its right coordinate stored
    rightX = fromJust $ M.lookup tid rMap


----------------------------------------
-- | Traversals

-- Construct a tour traversing the trapezoids s.t. every pair of consecutive trapezoids is
-- consecutive
constructTour :: TrapezoidalMap t e p r -> [Trapezoid e p r :+ t]
constructTour = undefined
  -- basic idea: For every slab, store the at most two indices i and j s.t. the trapezoids
  -- i and j

tmap :: Applicative f => (Trapezoid e p r :+ t -> f (Trapezoid e' p' r' :+ t'))
                           -> TrapezoidalMap t e p r -> f (TrapezoidalMap t' e' p' r')
tmap = undefined
