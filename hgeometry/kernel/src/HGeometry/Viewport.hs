{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Viewport
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Geometric viewport; i.e. some way of "viewing" only one
-- particular rectangular region of a larger 2d space.
--
--------------------------------------------------------------------------------
module HGeometry.Viewport
  ( Viewport(Viewport), mkViewport
  , viewPort, worldToHost, hostToWorld
  , toWorldIn, toHostFrom
  , fromSize, flipY
  , centeredOrigin
  , alignedOrigin
  , wrtCenter
  -- * ZoomConfiging
  , ZoomConfig(ZoomConfig), range, currentLevel
  )
  where

import Control.Lens
import HGeometry.Box
import HGeometry.Interval
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Transformation
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | Represents a viewport ; i.e. a rectangle through which we view
-- the world.
data Viewport r = Viewport { _viewPort    :: Rectangle (Point 2 r)
                             -- ^ in host world
                           , _worldToHost :: Transformation 2 r
                             -- ^ Transformation that turns world
                           -- coordinates into host coordinates.

                           -- _hostToWorld :: Transformation 2 r
                           }
                  deriving stock (Eq, Show)

-- | Lens to access the viewport rectangle
viewPort :: Lens' (Viewport r) (Rectangle (Point 2 r))
viewPort = lens _viewPort (\(Viewport _ t) vp -> Viewport vp t)

-- | The transformation
worldToHost :: Lens' (Viewport r) (Transformation 2 r)
worldToHost = lens _worldToHost (\(Viewport t _) -> Viewport t)


-- | Creates a viewport from the given rectangle and the
-- transformation. The transformation is applied with respect to the
-- center of the viewport.
mkViewport     :: ( Rectangle_ rectangle point, Point_ point 2 r, Fractional r
                  ) => rectangle -> Transformation 2 r -> Viewport r
mkViewport r t = centeredOrigin r & worldToHost %~ (|.| t)


-- | Host to world transformation, i.e. given a point in the host
-- coordinate system, we can compute the point in world coordinates
-- using this transformation.
hostToWorld :: (Fractional r)
            => Getter (Viewport r) (Transformation 2 r)
hostToWorld = worldToHost.to inverseOf

--------------------------------------------------------------------------------

-- | Convert some geometry in host coordinates to world coordinates in
-- the viewport
toWorldIn    :: ( IsTransformable g
                , NumType g ~ r, Dimension g ~ 2, Fractional r)
             => Viewport r -> g -> g
toWorldIn vp = transformBy (vp^.hostToWorld)

-- | Convert some geometry in world coordinates to host coordinates
-- according to the viewport
toHostFrom  :: (IsTransformable g, NumType g ~ r, Dimension g ~ 2, Num r)
             => Viewport r -> g -> g
toHostFrom vp = transformBy (vp^.worldToHost)


--------------------------------------------------------------------------------

-- | Given a vector with widht w and height h, crates a viewport with
  -- focussing on [0,w] x [0,h]
fromSize   :: ( Num r, Vector_ vector 2 r
              ) => vector -> Viewport r
fromSize v = Viewport (Box origin (Point $ v^._Vector)) identity

-- | Flips the y-coordinate so that the origin is in the bottom left.
--
flipY    :: ( Num r, Vector_ vector 2 r)
         => vector -- ^ the dimensions of the viewport
         -> Viewport r
flipY v = Viewport (Box origin (Point $ v^._Vector))
                   (flipY' $ v^.yComponent)

-- | Transformation that flips the y-axis and shifts by h, essenitally
-- moving the origin from the top-left facing downards to the
-- bottom-left and upwards.
flipY'   :: ( Num r
            ) => r -> Transformation 2 r
flipY' h = translation (Vector2 0 h) |.| scaling (Vector2 1 (-1))


--------------------------------------------------------------------------------

-- | Creates a viewport in which the origin is at the center of the viewport
centeredOrigin       :: ( Fractional r
                        , Rectangle_ rectangle point
                        , Point_ point 2 r
                        ) => rectangle -> Viewport r
centeredOrigin rect' = Viewport rect
                                (translation $ centerPoint rect' .-. origin)
  where rect = Box (rect'^.minPoint.asPoint) (rect'^.maxPoint.asPoint)



-- | Creates a viewport in which the origin at the bottom left of the viewport
alignedOrigin       :: ( Num r
                       ) => Rectangle (Point 2 r) -> Viewport r
alignedOrigin rect' = Viewport rect' (translation $ bottomLeft .-. origin)
  where
    bottomLeft = rect'^.minPoint



-- | make the transformation with respect to the center of the viewport
wrtCenter             :: ( Fractional r
                         ) => Viewport r -> Transformation 2 r -> Transformation 2 r
wrtCenter vp trans' = let v = centerPoint (vp^.viewPort) .-. origin
                      in translation v |.| trans' |.| translation ((-1) *^ v)

--------------------------------------------------------------------------------

-- | Data type representing possibilities for zooming in and out in a
-- viewport.
data ZoomConfig r = ZoomConfig { _range        :: ClosedInterval r
                               , _currentLevel :: r
                               }

deriving stock instance (Eq (ClosedInterval r), Eq r) => Eq (ZoomConfig r)
deriving stock instance (Show (ClosedInterval r), Show r) => Show (ZoomConfig r)

instance Functor ZoomConfig where
  fmap f (ZoomConfig r l) = ZoomConfig (fmap f r) (f l)
instance Foldable ZoomConfig where
  foldMap f (ZoomConfig r l) = foldMap f r <> f l
instance Traversable ZoomConfig where
  traverse f (ZoomConfig r l) = ZoomConfig <$> traverse f r <*> f l



-- | Lens to access the zoom-range
range :: Lens' (ZoomConfig r) (ClosedInterval r)
range = lens _range (\(ZoomConfig _ l) r' -> ZoomConfig r' l)

-- | Clamps the value to the right range on set
currentLevel :: Ord r => Lens' (ZoomConfig r) r
currentLevel = lens _currentLevel (\(ZoomConfig r _) l' -> ZoomConfig r (clampTo r l'))

-- instance Fractional r => Default (ZoomConfig r) where
--   def = ZoomConfig (ClosedRange 0.1 4) 1
