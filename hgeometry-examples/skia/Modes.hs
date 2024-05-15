module Modes
  ( Mode(..)
  , ModeData(..)
  ) where


import Data.Default.Class

--------------------------------------------------------------------------------

data Mode = SelectMode
          | PanMode
          | PointMode
          | PenMode
          | LineMode
          | PolyLineMode
          | PolygonMode
          | RectangleMode
          | CircleMode
          | TextMode
          | MathMode
          deriving (Show,Read,Eq)


--------------------------------------------------------------------------------

data family ModeData (m :: Mode)


--------------------------------------------------------------------------------

data instance ModeData SelectMode =
  SelectModeData { _selection :: Maybe Int -- TODO;
                 } deriving (Show,Eq)


instance Default (ModeData SelectMode) where
  def = SelectModeData Nothing

--------------------------------------------------------------------------------

-- | In point data we don't cary anything useful
newtype instance ModeData PointMode =
  PointModeData () deriving (Show,Eq)

instance Default (ModeData PointMode) where
  def = PointModeData ()

--------------------------------------------------------------------------------

data instance ModeData LineMode     =
  LineModeData () deriving (Show,Eq)

instance Default (ModeData LineMode) where
  def = LineModeData ()

data instance ModeData PolygonMode  =
  PolygonModeData () deriving (Show,Eq)

instance Default (ModeData PolygonMode) where
  def = PolygonModeData ()


data instance ModeData RectangleMode=
  RectangleModeData () deriving (Show,Eq)

instance Default (ModeData RectangleMode) where
  def = RectangleModeData ()

data instance ModeData CircleMode   =
  CircleModeData () deriving (Show,Eq)

instance Default (ModeData CircleMode) where
  def = CircleModeData ()

data instance ModeData TextMode     =
  TextModeData () deriving (Show,Eq)

instance Default (ModeData TextMode) where
  def = TextModeData ()

data instance ModeData MathMode     =
  MathModeData () deriving (Show,Eq)

instance Default (ModeData MathMode) where
  def = MathModeData ()
