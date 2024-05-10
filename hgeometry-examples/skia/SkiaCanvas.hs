{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module SkiaCanvas
  (
  -- * Model
    Canvas
  , theViewport
  , blankCanvas
  , HasDimensions(..)

  , HasMousePosition(..)
  , mouseCoordinates

  , canvasKitRef
  , surfaceRef

  -- * Controller
  , InternalCanvasAction(..)
  , handleInternalCanvasAction
  , handleCanvasKitAction

  , CanvasResizeAction(..)
  , handleCanvasResize

  , ErrorAction(..)

  , acquireCanvasSize

  , withCanvasEvents

  -- * View
  , skiaCanvas_
  ) where

import SkiaCanvas.Core
import SkiaCanvas.Render
