{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module SkiaCanvas.CanvasKit
  ( CanvasKit
  , Surface
  , SkCanvasRef
  , InitializeSkCanvasAction(..)
  , initializeCanvasKitSub

  , requestAnimationFrame

  , mkWhite

  , clear, clearWith

  , withPath
  , withPaint
  ) where

import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Data.Functor.Apply (Apply(..))
import qualified Data.Map as Map
import           GHCJS.Marshal (fromJSVal, ToJSVal(..))
import           GHCJS.Types
import           HGeometry.Ball
import           HGeometry.Number.Radical (Radical)
import           HGeometry.Point
import           HGeometry.Vector
import           HGeometry.Viewport
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (jsg1, jsg2, jsf, js1, js4, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import           Miso
import           Miso.String (MisoString)
import           MouseExtra
import           SkiaCanvas.CanvasKit.Core

--------------------------------------------------------------------------------
-- * The drawing functions
