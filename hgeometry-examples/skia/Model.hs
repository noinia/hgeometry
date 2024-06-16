{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Model
  ( Model(Model)
  , canvas, zoomConfig, points, polyLines, rectangles
  , diagram, _layers, mode, stroke, fill
  , currentModal

  , initialModel

  , Modal(..), currentStatus

  , R

  , Selected(..)

  , colorPresets

  , darkishGrey, lightGrey
  ) where

import           Attributes
import           Base
import           Color
import           Control.Lens hiding (view, element)
import           Data.Default.Class
import qualified Data.IntMap as IntMap
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Ext
import           HGeometry.Interval
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Point
import           HGeometry.Viewport (ZoomConfig(..))
import           Layers
import           Modes
import           PolyLineMode
import           RectangleMode
import qualified SkiaCanvas
import           StrokeAndFill

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------
-- * Type representing the currently active Modal (if any)

-- | Which modal is currently active (if any)
data Modal = StrokeModal
           | FillModal
           deriving (Show,Read,Eq,Ord)



-- | Tests if the current modal matches the given modal, and returns the appropriate status
currentStatus       :: Maybe Modal -> Modal -> Status
currentStatus m1 m2
  | m1 == Just m2  = Active
  | otherwise      = InActive

--------------------------------------------------------------------------------
-- * Data Type representing all our modal data

data Model = Model { _canvas       :: SkiaCanvas.Canvas R
                   , _zoomConfig   :: ZoomConfig Double
                   , _mode         :: Mode
                   , _points       :: IntMap.IntMap (Point 2 R :+ Attributes (Point 2 R))
                   , _polyLines    :: IntMap.IntMap (PolyLine' R :+ Attributes (PolyLine' R))
                   , _rectangles   :: IntMap.IntMap (Rectangle' R :+ Attributes (Rectangle' R))
                   , _diagram      :: Maybe [Point 2 R]
                   , __layers      :: Layers
                   , _stroke       :: !StrokeFill
                   , _fill         :: !StrokeFill
                   , _currentModal :: Maybe Modal
                   } deriving (Eq,Show)
makeLenses ''Model

instance HasLayers Model where
  layers = _layers -- lens __layers (\m lrs -> m { __layers = lrs })


--------------------------------------------------------------------------------

instance Default (Point 2 R :+ Int) where
  def = origin :+ 0

----------------------------------------

initialModel :: Model
initialModel = Model { _canvas       = SkiaCanvas.blankCanvas 1024 768
                     , _zoomConfig   = ZoomConfig (ClosedInterval 0.1 4) 1
                     , _points       = mempty
                     , _polyLines    = mempty
                     , _rectangles   = mempty
                     , _diagram      = Nothing
                     , __layers      = initialLayers
                     , _mode         = PointMode
                     , _stroke       = defaultStroke
                     , _fill         = defaultFill
                     , _currentModal = Nothing
                     }

--------------------------------------------------------------------------------


data Selected = NotSelected | Selected
  deriving (Show,Read,Eq,Ord)


--------------------------------------------------------------------------------


-- | default color presets in goodnotes
colorPresets :: NonEmpty Color
colorPresets = NonEmpty.fromList
               [ fromRGB24 0   0   0
               , darkishGrey
               , mediumGrey
               , lightGrey
               , fromRGB24 252 252 252

               , fromRGB24 119 41  135 -- purple
               , fromRGB24 192 40  27 -- darkish red
               , fromRGB24 229 95  90  -- lightish red
               , fromRGB24 241 156 153
               , fromRGB24 232 158 66 -- orange

               , myBlue
               , fromRGB24 28  68  138 -- darkblue
               , fromRGB24 49  113 86 -- darkgreen
               , fromRGB24 142 196 79 -- lightgreen
               , fromRGB24 254 255 149
               ]

darkishGrey :: Color
darkishGrey = fromRGB24 99  99  99

mediumGrey :: Color
mediumGrey = fromRGB24 155 155 155

lightGrey :: Color
lightGrey = fromRGB24 210 210 210
