{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module PolyLineMode
  ( PolyLine'
  , PartialPolyLine(..)
  , _StartPoint, _PartialPolyLine

  , ModeData(..)
  ) where

import           Base
import           Control.Lens hiding (view, element)
import           Data.Default.Class
import qualified Data.Sequence as Sequence
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Sequence.NonEmpty
import           Modes

--------------------------------------------------------------------------------
type PolyLine' r = PolyLineF ViewR1 (Point 2 r)



data PartialPolyLine r = StartPoint (Point 2 r)
                       | PartialPolyLine (PolyLine' r)
                       deriving (Show,Eq)
makePrisms ''PartialPolyLine


--------------------------------------------------------------------------------

data instance ModeData PolyLineMode =
  PolyLineModeData (Maybe (PartialPolyLine R))
  deriving (Show,Eq)


instance Default (ModeData PolyLineMode) where
  def = PolyLineModeData Nothing

newtype instance ModeData PenMode      =
  PenModeData (ModeData PolyLineMode) deriving (Show,Eq)

instance Default (ModeData PenMode) where
  def = PenModeData def
