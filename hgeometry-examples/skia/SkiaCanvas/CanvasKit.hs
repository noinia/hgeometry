{-# LANGUAGE OverloadedStrings          #-}
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
  , setAntiAlias
  , setStyle
  , setColor

  , mkPaintStyle
  , SkPaintStyle
  , Style(..)

  , ColorInt
  , mkColor4f
  , mkColor
  ) where

import SkiaCanvas.CanvasKit.Color
import SkiaCanvas.CanvasKit.Core
import SkiaCanvas.CanvasKit.Paint
import SkiaCanvas.CanvasKit.Path
import SkiaCanvas.CanvasKit.Picture
import SkiaCanvas.CanvasKit.PictureRecorder
import SkiaCanvas.CanvasKit.GeomPrims

--------------------------------------------------------------------------------
-- * The drawing functions
