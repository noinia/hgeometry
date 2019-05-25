{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Algorithms.Geometry.ConvexHull.GrahamScan (convexHull)
import           Control.Exception (catch)
import           Control.Lens
import           Data.Ext
import           Data.GI.Base
import           Data.Geometry
import           Data.Geometry.Polygon.Convex
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           Graphics.Rendering.Cairo.Canvas (Canvas)
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Banana.GI.Gtk
import qualified RenderCanvas as Render
import           RenderUtil

--------------------------------------------------------------------------------

main :: IO ()
main = runGtk `catch` (\(e::Gtk.GError) -> Gtk.gerrorMessage e >>= putStrLn . T.unpack)
  where
    runGtk = do
      _ <- Gtk.init Nothing
      compile networkDescription >>= actuate
      Gtk.main

networkDescription :: MomentIO ()
networkDescription = do
    b <- Gtk.builderNew
    _ <- Gtk.builderAddFromFile b "interactive/canvas.ui"

    window   <- castB b "window" Gtk.Window
    destroyE <- signalE0 window #destroy
    reactimate $ Gtk.mainQuit <$ destroyE

    quitMenuItem <- castB b "quitMenuItem" Gtk.ImageMenuItem
    quitClickE <- signalE0 quitMenuItem #activate
    reactimate $ Gtk.mainQuit <$ quitClickE

    -- openFileMenuItem <- castB b "openFileMenuItem" Gtk.ImageMenuItem
    -- openFileClickE   <- signalE0 openFileMenuItem #activate

    mouseLabel <- castB b "mouseLabel" Gtk.Label

    -- colorButton       <- castB b "colorButton" Gtk.Button
    -- pressedE          <- signalE0 colorButton #clicked
    -- colorButtonStateB <- accumB False (not <$ pressedE)

    drawingArea   <- castB b "canvas" Gtk.DrawingArea

    Gtk.widgetAddEvents drawingArea (gflagsToWord [ Gdk.EventMaskPointerMotionMask
                                                  , Gdk.EventMaskButtonPressMask
                                                  ])

    -- handle mouse clicks
    mousePressedE <- signalE1' drawingArea #buttonPressEvent $ \e -> do
                      x <- Gdk.getEventButtonX e
                      y <- Gdk.getEventButtonY e
                      return $! Point2 x y

    -- our points that we collect by mouse clicks
    pointSetB <- accumB [] ((\p -> (p:)) <$> mousePressedE)

    -- show the mouse coordinates
    mouseMotionE <- signalE1' drawingArea #motionNotifyEvent $ \e -> do
                      x <- Gdk.getEventMotionX e
                      y <- Gdk.getEventMotionY e
                      return $! Point2 x y
    mouseMotionB  <- stepper ""                (showT <$> mouseMotionE)
    sink mouseLabel   [#label :== mouseMotionB]


    -- draw everything
    draw drawingArea (drawHull <$> pointSetB)

    #showAll window

drawHull     :: [Point 2 Double] -> Canvas ()
drawHull pts = do
    Canvas.background $ Canvas.gray 255
    Canvas.stroke $ Canvas.gray 0
    mapM_ Render.point pts
    Canvas.stroke $ Canvas.red 200
    Canvas.fill $ Canvas.red 200 Canvas.!@ 32
    case pts of
      [] -> pure ()
      _  -> Render.polygon $
             (convexHull . NonEmpty.fromList . map ext $ pts)^.simplePolygon
