{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module PolyLineMode
  ( PolyLineModeData(PolyLineModeData), currentPoly

  , PolyLine'
  , PartialPolyLine(..)
  , _StartPoint, _PartialPolyLine

  , extendWith
  , extendPolyLine
  ) where

import           Base
import           Control.Lens hiding (view, element)
import           Data.Default
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Sequence as Sequence
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Sequence.NonEmpty
-- import           Modes

--------------------------------------------------------------------------------
type PolyLine' r = PolyLineF ViewR1 (Point 2 r)



data PartialPolyLine r = StartPoint (Point 2 r)
                       | PartialPolyLine (PolyLine' r)
                       deriving (Show,Read,Eq)
makePrisms ''PartialPolyLine


-- | Extend the current partial polyline with the given point (if it is on the canvas.)
extendWith          :: Eq r => Maybe (Point 2 r)
                    -> Maybe (PartialPolyLine r) -> Maybe (PartialPolyLine r)
extendWith mp state = case mp of
    Nothing -> state
    Just p  -> Just $ case state of
      Nothing                   -> StartPoint p
      Just (StartPoint s)       -> PartialPolyLine . polyLineFromPoints $ s :| [p]
      Just (PartialPolyLine pl) -> PartialPolyLine $ extendPolyLine pl p

extendPolyLine      :: Eq r => PolyLine' r -> Point 2 r -> PolyLine' r
extendPolyLine pl p = pl&_PolyLineF %~ \vs@(_ :>> q) -> if p /= q then vs |>> p else vs

--------------------------------------------------------------------------------


-- instance Default (ModeData PolyLineMode) where
--   def = PolyLineModeData Nothing

-- newtype instance ModeData PenMode      =
--   PenModeData (ModeData PolyLineMode) deriving (Show,Eq)

-- instance Default (ModeData PenMode) where
--   def = PenModeData def


newtype PolyLineModeData = PolyLineModeData (Maybe (PartialPolyLine R))
  deriving (Show,Read,Eq)


currentPoly :: Lens' PolyLineModeData (Maybe (PartialPolyLine R))
currentPoly = coerced

instance Default PolyLineModeData where
  def = PolyLineModeData Nothing
