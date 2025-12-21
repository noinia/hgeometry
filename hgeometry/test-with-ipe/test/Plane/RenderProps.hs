{-# LANGUAGE TemplateHaskell #-}
module Plane.RenderProps
  ( RenderProps(RenderProps)
  , HasRenderProps(..)
  ) where

import Ipe
import Ipe.Color (black)
import Control.Lens
import Data.Default

--------------------------------------------------------------------------------

data RenderProps = RenderProps { _edgeAttrs :: Maybe (IpeAttributes Path Double)
                               , _faceAttrs :: Maybe (IpeAttributes Path Double)
                               }
                   deriving (Show,Eq)

makeClassy ''RenderProps


instance Default RenderProps where
  def = RenderProps (Just $ attr SStroke black) Nothing
