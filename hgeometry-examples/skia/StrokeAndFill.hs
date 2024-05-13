{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module StrokeAndFill
  ( Status(..)
  , _InActive, _Active
  , Stroke(Stroke)
  , strokeStatus, currentStrokeColor
  , Fill(Fill)
  , fillStatus, currentFillColor
  , defaultStroke
  , defaultFill

  , toColoring
  ) where

import Color
import Control.Lens hiding (view, element)
import Data.Colour (black)
import Data.Colour.Names (blue)
import Data.Default.Class

--------------------------------------------------------------------------------

data Status = InActive | Active
  deriving (Show,Read,Eq,Ord)

makePrisms ''Status

data Stroke = Stroke { _strokeStatus       :: {-# UNPACK #-}!Status
                     , _currentStrokeColor :: {-# UNPACK #-}!Color
                     }
  deriving (Show,Eq)

makeLenses ''Stroke

data Fill = Fill { _fillStatus        :: {-# UNPACK #-}!Status
                 , _currentFillColor  :: {-# UNPACK #-}!Color
                 }
  deriving (Show,Eq)

makeLenses ''Fill

defaultStroke :: Stroke
defaultStroke = Stroke Active (Color black Opaque)

defaultFill :: Fill
defaultFill = Fill InActive (Color blue Opaque)

--------------------------------------------------------------------------------

-- | Given a stroke and a fill, computes a coloring
--
-- if neither stroke or fill are set, we use the default (which is stroke using black)
toColoring     :: Stroke -> Fill -> Coloring
toColoring s f = case (s^.strokeStatus, f^.fillStatus) of
  (InActive, InActive) -> def -- this case is kind of weird
  (InActive, Active)   -> FillOnly   (f^.currentFillColor)
  (Active,   InActive) -> StrokeOnly (s^.currentStrokeColor)
  (Active,   Active)   -> StrokeAndFill (s^.currentStrokeColor) (f^.currentFillColor)
