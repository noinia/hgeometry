{-# LANGUAGE OverloadedStrings          #-}
module StrokeAndFill
  ( Status(..)
  , _InActive, _Active
  , StrokeFill(StrokeFill)
  , Stroke, Fill
  , HasColor(..)
  , HasStatus(..)

  , defaultStroke
  , defaultFill

  , toColoring

  , myBlue
  ) where

import Color
import Control.Lens hiding (view, element)
import Data.Colour (black)
import Data.Default
import Miso.Bulma.Modal (Status(..), HasStatus(..), _InActive,_Active)

--------------------------------------------------------------------------------

data StrokeFill = StrokeFill { _status :: {-# UNPACK #-}!Status
                             , _color  :: {-# UNPACK #-}!Color
                             } deriving (Show,Eq)

type Stroke = StrokeFill
type Fill = StrokeFill

instance HasStatus StrokeFill where
  status = lens _status (\s st -> s { _status = st })
  {-# INLINE status #-}

class HasColor t where
  -- | Lens to access the color
  color :: Lens' t Color


instance HasColor StrokeFill where
  color= lens _color (\s c -> s { _color = c })
  {-# INLINE color #-}


defaultStroke :: Stroke
defaultStroke = StrokeFill Active (Color black Opaque)

defaultFill :: Fill
defaultFill = StrokeFill InActive myBlue

--------------------------------------------------------------------------------

-- | Given a stroke and a fill, computes a coloring
--
-- if neither stroke or fill are set, we use the default (which is stroke using black)
toColoring     :: Stroke -> Fill -> Coloring
toColoring s f = case (s^.status, f^.status) of
  (InActive, InActive) -> def -- this case is kind of weird
  (InActive, Active)   -> FillOnly   (f^.color)
  (Active,   InActive) -> StrokeOnly (s^.color)
  (Active,   Active)   -> StrokeAndFill (s^.color) (f^.color)


--------------------------------------------------------------------------------

-- | My color blue
myBlue :: Color
myBlue = fromRGB24 53  121 246 -- blue
