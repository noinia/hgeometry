--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Direction.Cardinal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Cardinal and Intercardinal directions.
--
--------------------------------------------------------------------------------
module HGeometry.Direction.Cardinal
  ( CardinalDirection(..)
    -- , _North, _East, _South, _West
  , oppositeDirection

  , InterCardinalDirection(..)
    -- , _NorthWest, _NorthEast, _SouthEast, _SouthWest

  , interCardinalsOf
  ) where

import GHC.Generics (Generic)
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | The four cardinal directions.
data CardinalDirection = North | East | South | West deriving (Show,Read,Eq,Ord,Enum,Bounded)
-- makePrisms ''CardinalDirection

--------------------------------------------------------------------------------
-- * Functions on Cardinal Directions

-- | Computes the direction opposite to the given one.
oppositeDirection :: CardinalDirection -> CardinalDirection
oppositeDirection = \case
  North -> South
  East  -> West
  South -> North
  West  -> East

--------------------------------------------------------------------------------

-- | Intercardinal directions
data InterCardinalDirection = NorthWest | NorthEast | SouthEast | SouthWest
  deriving (Show,Read,Eq,Ord,Enum,Generic)
-- makePrisms ''InterCardinalDirection

--------------------------------------------------------------------------------
-- * Functions on InterCardinal Directions

-- | Get the two intercardinal directions, in increasing order,
-- corresponding to the cardinal direction.
interCardinalsOf :: CardinalDirection -> Vector 2 InterCardinalDirection
interCardinalsOf = \case
  North -> Vector2 NorthWest NorthEast
  East  -> Vector2 NorthEast SouthEast
  South -> Vector2 SouthEast SouthWest
  West  -> Vector2 SouthWest NorthWest



--------------------------------------------------------------------------------

-- | Classfiy the direction of the given vector
--
-- pre: the vector is non-zero
classifyDirection               :: (Ord r, Num r) => Vector 2 r
                                -> Either CardinalDirection InterCardinalDirection
classifyDirection = fromMaybe (error "classifyDirection: Direction vector is zero!")
                  . classifyDirection'

-- | Classfiy the direction of the given vector. Returns nothing if the vector is zero
classifyDirection'               :: (Ord r, Num r) => Vector 2 r
                                -> Maybe (Either CardinalDirection InterCardinalDirection)
classifyDirection' (Vector2 x y) = case x `compare` 0 of
  LT -> Just $ case y `compare` 0 of
                 LT -> Right SouthWest
                 EQ -> Left West
                 GT -> Right NorthWest
  EQ -> case y `compare` 0 of
          LT -> Just $ Left South
          EQ -> Nothing
          GT -> Just $ Left North
  GT -> Just $ case y `compare` 0 of
                 LT -> Right SouthEast
                 EQ -> Left East
                 GT -> Right NorthEast
