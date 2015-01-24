{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.TrapezoidalDecomposition where

import           Control.Applicative
import           Control.Lens

import           Data.Ext

import           Data.ExplicitOrdSet(ExpSet)
import           Data.Map(Map)
import           Data.Maybe(listToMaybe)
import           Data.Vector(Vector)

import qualified Data.ExplicitOrdSet as ES
import qualified Data.Map as M
import qualified Data.Vector as V

import           Data.Geometry.Point
import           Data.Geometry.Interval
import           Data.Geometry.Line


-- We bias to the RIGHT and to the TOP. i.e. if query point on the boundary of two traps
-- we report the right and/or topmost one

--------------------------------------------------------------------------------

newtype TrapezoidId = TrapezoidId Int deriving (Show,Read,Eq)


type Edge r = LineSegment 2 () r


-- | A Trapezoid, the extra data on top and bottom is the line segment containing
-- the top and bottom edge of this trapezoid.
data Trapezoid e p r = Trapezoid { _trapId :: TrapezoidId
                                 , _bottom :: Edge r :+ (LineSegment 2 p r :+ e)
                                 , _top    :: Edge r :+ (LineSegment 2 p r :+ e)
                                 }
                     | LeftUnbounded !r -- right boundary
                     | RightUnbounded !r -- left boundary

makeLenses ''Trapezoid


-- | A representation of a trapezoid that we store in our TrapezoidalMap.
-- The bototm segment is the original segment.
-- the leftOn is the x-coordinate through which the left boundary of this trapezoid
-- lies on.
--
-- Ifd the bottomOn is empty, the trapezoid is unbounded from below.
data TrapezoidRep e p r = TrapezoidRep { _repId     :: !TrapezoidId
                                       , _repMinX   :: !r
                                       , _repBottom :: !(Maybe (LineSegment 2 p r :+ e))
                                       }
makeLenses ''TrapezoidRep

leftUnboundedId  = TrapezoidId 0
rightUnboundedId = TrapezoidId 1

-- yCoordAt   :: r -> TrapezoidRep e p r -> Maybe r
-- yCoordAt x =


-- | A vertical slab has a minimum x coordinate: minX s.t. the
-- slabData stored here is valid on the interval [minX, maxX), for some maxX >= minX
data VSlab t e p r = VSlab { _minX     :: !r
                           , _slabData :: ExpSet (TrapezoidRep e p r :+ t)
                           }
makeLenses ''VSlab


data TrapezoidQuery t e p r = TrapezoidQuery { _below :: !(Maybe (TrapezoidRep e p r :+ t))
                                             , _self  :: !(TrapezoidRep e p r :+ t)
                                             , _above :: !(Maybe (TrapezoidRep e p r :+ t))
                                             }

findTrapezoidRep                :: (Ord r, Num r)
                                => Point 2 r -> VSlab t e p r -> TrapezoidQuery t e p r
findTrapezoidRep p (VSlab _ es) = TrapezoidQuery low cur high
  where
    -- Since we explicitly store the bottomless and topless trapezoid, we are guaranteed
    -- to find a traprep.
    (belowT,aboveT) = ES.splitMonotone (maybe True pred . (^.core.repBottom)) es

    low         = ES.maximum belowT
    (cur:above) = ES.viewL aboveT
    high        = listToMaybe above

            -- Test if p lies above or on the segment s.
             -- if we turn clockwise then p is below the segment
             -- hence we should return False
    pred (s :+ _) = let (l,r) = orderedEndPoints s in ccw (l^.core) (r^.core) p /= CW

-- -- |
-- compareByPoint :: Point TrapezoidRep e p r -> TrapezoidRep e p r -> Ordering



newtype TrapezoidalDecomposition t e p r = TD { _unTD :: Vector (VSlab t e p r) }

findSlab :: Ord r => r -- ^ The x-coordinate of the input point
         -> TrapezoidalDecomposition t e p r  -- ^ the td
         -> VSlab t e p r -- ^ The vertical slab containing the input point,
                                  -- if it exists
findSlab x (TD v) = v V.! i
  where
    i   = binarySearch p 0 (V.length v - 1) -- the last one is guaranteed to be OK
    p i = x < _minX (v V.! i)

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




-- | A trapezoidal map in which each trapezoid has extra data of type t,
-- each vertex data of type p, each edge of type e.
--
data TrapezoidalMap t e p r = TrapezoidalMap {
    _maxX          :: !r
  , _decomposition :: TrapezoidalDecomposition t e p r
  , _rightMap      :: Map TrapezoidId r  -- ^ The x-coordinate of the right boundary of
                                         -- each trapezoid.
  , _leftUnboundedData  :: t
  , _rightUnboundedData :: t
  }
-- makeLenses ''TrapezoidalMap

_minX' :: TrapezoidalDecomposition t e p r -> r
_minX' = _minX . V.head . _unTD


buildTrapezoidalMap :: [LineSegment 2 p r :+ e]
                       -> TrapezoidalMap () e p r
buildTrapezoidalMap = undefined


locateTrapezoid :: (Num r, Ord r) => Point 2 r -> TrapezoidalMap t e p r -> Trapezoid e p r :+ t
locateTrapezoid p (TrapezoidalMap maxX' xDecomp rMap lData rData)
    | px >= maxX' = RightUnbounded maxX' :+ rData
    | px <  minX' = LeftUnbounded  minX' :+ lData
    | otherwise   = reconstruct rMap . findTrapezoidRep p $ findSlab px xDecomp
  where
    px    = p^.xCoord
    minX' = _minX' xDecomp


reconstruct :: Map TrapezoidId r -> TrapezoidQuery t e p r -> Trapezoid e p r :+ t
reconstruct = undefined


-- Construct a tour traversing the trapezoids s.t. every pair of consecutive trapezoids is
-- consecutive
constructTour :: TrapezoidalMap t e p r -> [Trapezoid e p r :+ t]
constructTour = undefined
  -- basic idea: For every slab, store the at most two indices i and j s.t. the trapezoids
  -- i and j

tmap :: Applicative f => (Trapezoid e p r :+ t -> f (Trapezoid e' p' r' :+ t'))
                           -> TrapezoidalMap t e p r -> f (TrapezoidalMap t' e' p' r')
tmap = undefined
