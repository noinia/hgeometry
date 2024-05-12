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

  , Color, ColorF(..)
  , fromRGB24
  , rgba
  , Alpha(..)
  , ColorInt
  , mkColor4f
  , mkColor
  ) where

import           SkiaCanvas.CanvasKit.Core

--------------------------------------------------------------------------------
-- * The drawing functions
