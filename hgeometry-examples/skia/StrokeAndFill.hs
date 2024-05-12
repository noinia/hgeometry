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
  ) where

import Control.Lens hiding (view, element)
import Data.Colour (black)
import Data.Colour.Names (blue)
import SkiaCanvas.CanvasKit.Core (Color, ColorF(..), Alpha(..))

--------------------------------------------------------------------------------


-- deriving instance Show (ColorF Float)

-- instance Show Color where
--   showsPrec k (Color c a) = showsPrec k $ Color (colourConvert c :: Colour Float) a

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
