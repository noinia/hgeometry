{-# LANGUAGE OverloadedStrings          #-}
module HGeometry.Miso.Svg.StaticCanvas
  ( StaticCanvas
  , staticCanvas
  , HasDimensions(..)
  -- , center, dimensions, zoomLevel
  , staticCanvas_
  -- , textAt
  -- , realWorldCoordinates
  , ToSvgCoordinate


  , matrixToMisoString
  ) where

import           Control.Lens hiding (elements)
import qualified Data.Map as Map
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.Transformation
import           HGeometry.Vector
import           HGeometry.Viewport
import           Miso (Attribute, View, height_, width_)
import           Miso.String (MisoString, ToMisoString, ms)
import qualified Miso.String.Util as MisoString
import           Miso.Svg (svg_, g_, transform_)

--------------------------------------------------------------------------------
-- *A Canvas

-- | Svg Canvas that has a "proper" Coordinate system whose origin is in the bottom left.
data StaticCanvas r =
  Canvas { _theViewport :: !(Viewport r)
         -- ^ the viewport
         , _dimensions  :: !(Vector 2 Int)
         -- ^ dimensions (width,height) in pixels, of the canvas
         }
  deriving stock (Eq)

theViewport :: Lens (StaticCanvas r) (StaticCanvas s) (Viewport r) (Viewport s)
theViewport = lens _theViewport (\c vp -> c { _theViewport = vp })

class HasDimensions s a | s -> a where
  dimensions :: Lens' s a

instance HasDimensions (StaticCanvas r) (Vector 2 Int) where
  dimensions = lens _dimensions (\c d -> c { _dimensions = d})
  {-# INLINE dimensions #-}


-- class HasTheViewport s a | s -> a where
--   theViewport :: Lens' s a

-- instance HasTheViewport (Canvas r) (Viewport r) where
--   {-# INLINE theViewport #-}
--   theViewport f (Canvas vp d) = fmap (\ vp' -> Canvas vp' d) (f vp)

-- center     :: Lens' (Canvas r) (Point 2 r)
-- center     = lens _center     (\cv c -> cv { _center     = c } )

-- dimensions :: Lens' (Canvas r) (Vector 2 Int)
-- dimensions = lens _dimensions (\cv c -> cv { _dimensions = c } )

-- zoomLevel  :: Lens' (Canvas r) r
-- zoomLevel  = lens _zoomLevel      (\cv c -> cv { _zoomLevel      = c } )

--------------------------------------------------------------------------------
-- | Create a canvas
staticCanvas     :: Num r
                 => Int -> Int -> StaticCanvas r
staticCanvas w h = let v = Vector2 w h
                   in Canvas (flipY (fromIntegral <$> v)) v

--------------------------------------------------------------------------------
-- * The Controller


--------------------------------------------------------------------------------
-- * The View

type ToSvgCoordinate = ToMisoString


-- | Draws the actual canvas
staticCanvas_               :: (RealFrac r, ToSvgCoordinate r)
                            => StaticCanvas r
                            -> [Attribute action] -> [View action] -> View action
staticCanvas_ canvas ats vs =
    svg_ ([ width_   . ms $ w
          , height_  . ms $ h
            -- , viewBox_      $ outerVB
          ] <> ats)
         [ g_ [ transform_ ts ] vs
         ]
  where
    (Vector2 w h) = canvas^.dimensions
    ts = matrixToMisoString $ canvas^.theViewport.worldToHost.transformationMatrix

-- | Renders a matrix as a MisoString
matrixToMisoString   :: ToSvgCoordinate r => Matrix 3 3 r -> MisoString
matrixToMisoString m = "matrix(" <> MisoString.unwords [a,b,c,e,d,f] <> ")"
  where
    (Vector3 (Vector3 a b c)
             (Vector3 d e f)
             _              ) = (m&elements %~ ms :: Matrix 3 3 MisoString)^.rows
                              -- this last vector has to be 0 0 1




-- | To be used instead of the text_ combinator in Miso
-- textAt                     :: ToSvgCoordinate r
--                            => Point 2 r -- ^ position where to draw (in world coordinates)
--                            -> [Attribute action]
--                            -> MisoString -> View action
-- textAt (Point2 x y) ats t = g_ [ transform_ $ mconcat [ "translate("
--                                                        , ms x
--                                                        , ", "
--                                                        , ms y
--                                                        , ")scale(1,-1)"
--                                                        ]
--                                 ] [ Miso.text_ ats [text t] ]



-- class RenderWebSvg t where
--   renderWith :: t -> [Attribute action] -> [View action] -> View action

-- render       :: RenderWebSvg t => t -> [Attribute action] -> View action
-- render x ats = renderWith x ats []

-- instance ToSvgCoordinate r => RenderWebSvg (Point 2 r) where
--   renderWith (Point2 x y) ats = ellipse_ $ [ cx_ $ ms x, cy_ $ ms y
--                                            , rx_ "5", ry_ "5"
--                                            , fill_ "black"
--                                            ] <> ats
