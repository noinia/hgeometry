{-# LANGUAGE TemplateHaskell  #-}
module Data.Geometry.Directions( CardinalDirection(..)
                               , _North, _East, _South, _West
                               , oppositeDirection

                                , InterCardinalDirection(..)
                                , _NorthWest, _NorthEast, _SouthEast, _SouthWest

                                , interCardinalsOf
                                ) where

import Control.Lens (makePrisms)
import Data.Util
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data CardinalDirection = North | East | South | West deriving (Show,Read,Eq,Ord,Enum,Bounded)
makePrisms ''CardinalDirection

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
makePrisms ''InterCardinalDirection

--------------------------------------------------------------------------------
-- * Functions on InterCardinal Directions

-- | Get the two intercardinal directions, in increasing order,
-- corresponding to the cardinal direction.
interCardinalsOf :: CardinalDirection -> Two InterCardinalDirection
interCardinalsOf = \case
  North -> Two NorthWest NorthEast
  East  -> Two NorthEast SouthEast
  South -> Two SouthEast SouthWest
  West  -> Two SouthWest NorthWest
