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

  , Color
  ) where

import Control.Lens hiding (view, element)
import Miso.String (MisoString)

--------------------------------------------------------------------------------

type Color = MisoString


data Status = InActive | Active
  deriving (Show,Read,Eq,Ord)

makePrisms ''Status

data Stroke = Stroke { _strokeStatus       :: {-# UNPACK #-}!Status
                     , _currentStrokeColor :: !Color
                     }
  deriving (Show,Eq)

makeLenses ''Stroke

data Fill = Fill { _fillStatus        :: {-# UNPACK #-}!Status
                 , _currentFillColor  :: !Color
                 }
  deriving (Show,Eq)

makeLenses ''Fill

defaultStroke :: Stroke
defaultStroke = Stroke Active "black"

defaultFill :: Fill
defaultFill = Fill InActive "blue"
