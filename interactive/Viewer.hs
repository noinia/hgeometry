{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad(forM_)

import           Control.Exception (catch)
import           Control.Lens
import           Data.Ext
import           Data.GI.Base
import           Data.Geometry
import           Data.Geometry.Ipe
import           Data.Geometry.Box
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
import           Linear.V2 (V2(..))


main :: IO ()
main = runGtk `catch` (\(e::Gtk.GError) -> Gtk.gerrorMessage e >>= putStrLn . T.unpack)
  where
    runGtk = do
      _ <- Gtk.init Nothing
      compile networkDescription >>= actuate
      Gtk.main

data ArrowKey = UpKey | DownKey | LeftKey | RightKey deriving (Show,Read,Eq,Bounded,Enum)

toArrowKey         :: T.Text -> Maybe ArrowKey
toArrowKey "Up"    = Just UpKey
toArrowKey "Down"  = Just DownKey
toArrowKey "Left"  = Just LeftKey
toArrowKey "Right" = Just RightKey
toArrowKey _       = Nothing



toDirection UpKey    = Vector2 0    1
toDirection DownKey  = Vector2 0    (-1)
toDirection LeftKey  = Vector2 (-1) 0
toDirection RightKey = Vector2 1    0




networkDescription :: MomentIO ()
networkDescription = do
    b <- Gtk.builderNew
    _ <- Gtk.builderAddFromFile b "interactive/viewport.glade"

    window   <- castB b "window" Gtk.Window
    destroyE <- signalE0 window #destroy
    reactimate $ Gtk.mainQuit <$ destroyE

    -- mouseLabel <- castB b "mouseLabel" Gtk.Label

    drawingArea  <- castB b "canvas" Gtk.DrawingArea
    drawingAreaH <- realToFrac . fromIntegral . snd <$> #getPreferredHeight drawingArea
    drawingAreaW <- realToFrac . fromIntegral . snd <$> #getPreferredWidth  drawingArea

    Gtk.widgetAddEvents drawingArea (gflagsToWord [ Gdk.EventMaskPointerMotionMask
                                                  , Gdk.EventMaskButtonPressMask
                                                  , Gdk.EventMaskSmoothScrollMask
                                                  , Gdk.EventMaskKeyPressMask
                                                  ])

    -- scroll Events
    scrollE <- signalE1' drawingArea #scrollEvent $ \e ->
                     Gdk.getEventScrollDeltaY e

    -- events when we press a key
    keyPressedE <- signalE1' drawingArea #keyPressEvent $ \e -> do
                     v  <- Gdk.getEventKeyKeyval e
                     Gdk.keyvalName v
    -- events where we press an arrow key
    let arrowKeyE = filterJust . fmap (>>= toArrowKey) $ keyPressedE

    -- handle mouse clicks
    mousePressedE <- signalE1' drawingArea #buttonPressEvent $ \e -> do
                      x <- Gdk.getEventButtonX e
                      y <- Gdk.getEventButtonY e
                      return $! Point2 x ((-1*y) + drawingAreaH)

    lastMousePressB <- stepper undefined mousePressedE

    -- mouse release
    mouseReleasedE <- signalE1' drawingArea #buttonReleaseEvent $ \e -> do
                        x <- Gdk.getEventButtonX e
                        y <- Gdk.getEventButtonY e
                        return $! Point2 x ((-1*y) + drawingAreaH)

    -- mouse coordinates
    mouseMotionE <- signalE1' drawingArea #motionNotifyEvent $ \e -> do
                      x  <- Gdk.getEventMotionX e
                      y  <- Gdk.getEventMotionY e
                      st <- Gdk.getEventMotionState e
                      let !p = Point2 x ((-1*y) + drawingAreaH)
                      return (p,st)
    mouseMotionB  <- stepper undefined mouseMotionE

    -- difference between the current mouse position and where we clicked last
    let dragOffsetB = (\p (q,_) -> p .-. q) <$> lastMousePressB <*> mouseMotionB

        -- sample the displacement vector whenever we are have a move event
        -- and the moude button is still on

        dragOffsetE = dragOffsetB
                   <@ filterE ((Gdk.ModifierTypeButton1Mask `elem`) . snd) mouseMotionE
    -- dragViewPositionB ::
    -- dragViewPositionB =  <$> viewPosB <*> dragOffsetB








    -- (fst <$> mouseMotionE)
    -- sink mouseLabel   [#label :== mouseMotionB]

    -- let mouseDragE = filterE ((Gdk.ModifierTypeButton1Mask `elem`) . snd) mouseMotionE
    -- -- mouseDragB <- stepper origin (fst <$> mouseDragE)

    --     mouseDragInitialE = mousePressedB <@ mouseDragE

    --     -- An event that has the offset of a dragging event
    --     dragOffsetE = apply () mouseDragE

    zoomLevelB <- accumB 1 $ (\dy -> (+0.1*dy)) <$> scrollE


    let lastPosE = unions [ (\k -> (.+^ 2 *^ toDirection k)) <$> arrowKeyE   -- key event
                          , (\v -> (.+^ v))                  <$> dragOffsetE -- drag event
                          ]

    viewPortPosB <- accumB (Point2 (drawingAreaW/2) (drawingAreaH/2)) $ lastPosE



    let viewPortB = ViewPort <$> pure drawWorld'
                             <*> pure (V2 drawingAreaW drawingAreaH)
                             <*> (toV2 . toVec <$> viewPortPosB)
                             <*> zoomLevelB
                             <*> pure 0

    -- draw everything
    draw drawingArea (mirrored drawingAreaH render <$> viewPortB)

    #showAll window

data ViewPort a = ViewPort { drawWorld             :: Canvas a
                           , screenSize            :: V2 Double
                           , clippwingWindowCenter :: V2 Double
                           , zoomLevel             :: Double
                           , rotation              :: Double
                           }

-- drawScreen (ViewPort _ (V2 w h) (V2 cx cy)) = Canvas.rect $ Canvas.D (cx-w/2) (cy-h/2) w h


clippingWindow :: ViewPort a -> Canvas.Dim
clippingWindow (ViewPort _ (V2 w h) (V2 cx cy) z _) = let x = cx - z*w/2
                                                          y = cy - z*h/2
                                                      in Canvas.D x y (z*w) (z*h)

data World = World { screen :: Canvas.Dim }
             deriving (Show,Eq)

-- some drawing
drawWorld' :: Canvas ()
drawWorld' = do
    Canvas.background $ Canvas.gray 255
    Canvas.stroke $ Canvas.gray 0
    forM_ [1..20] $ \i ->
      forM_ [1..16] $ \j -> do
        Canvas.rect (Canvas.D (100*i) (100*j) 20 20)

-- | Mirror the canvas s.t. the bottom-left corner is the origin
mirrored       :: Double -> (a -> Canvas ()) -> a -> Canvas ()
mirrored h d x = do Canvas.scale     $ V2 1 (-1)
                    Canvas.translate $ V2 0 (-1*h)
                    d x

-- | Render the view
render    :: ViewPort a -> Canvas a
render vp = do
    let r@(Canvas.D x y _ _) = clippingWindow vp
        (V2 h w) = screenSize vp

    Canvas.scale $ V2 (1/zoomLevel vp) (1/zoomLevel vp) -- scale everything s.t. the
                                                        -- cippingWindow equals
                                                        -- the window size
    Canvas.translate $ V2 (-1*x) (-1*y) -- move screen to the origin
    drawWorld vp
