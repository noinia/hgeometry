{-# LANGUAGE TemplateHaskell #-}
module Plane.RenderProps
  ( RenderProps(RenderProps), edgeAttrs, faceAttrs
  ) where

import Ipe
import Ipe.Color (black)
import Control.Lens
import Data.Default.Class

--------------------------------------------------------------------------------

data RenderProps = RenderProps { _edgeAttrs :: Maybe (IpeAttributes Path Double)
                               , _faceAttrs :: Maybe (IpeAttributes Path Double)
                               }

makeClassy ''RenderProps


instance Default RenderProps where
  def = RenderProps (Just $ attr SStroke black) Nothing
