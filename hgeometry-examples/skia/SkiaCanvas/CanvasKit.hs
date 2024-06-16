{-# LANGUAGE OverloadedStrings          #-}
module SkiaCanvas.CanvasKit
  ( CanvasKit
  , SurfaceRef
  , SkCanvas_
  , SkCanvasRef
  , InitializeSkCanvasAction(..)
  , initializeCanvasKitSub

  , requestAnimationFrame

  , mkWhite

  , clear

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
import SkiaCanvas.CanvasKit.Core hiding (clear, clearWith)
import SkiaCanvas.CanvasKit.GeomPrims
import SkiaCanvas.CanvasKit.Initialize
import SkiaCanvas.CanvasKit.Paint hiding (withPaint)
import SkiaCanvas.CanvasKit.Path
import SkiaCanvas.CanvasKit.Picture
import SkiaCanvas.CanvasKit.PictureRecorder
import SkiaCanvas.CanvasKit.Render

--------------------------------------------------------------------------------
-- * The drawing functions
