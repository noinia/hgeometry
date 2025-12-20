{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE PartialTypeSignatures            #-}
module Modes
  ( Mode(..)
  , _SelectMode
  , _PanMode
  , _PointMode
  , _PenMode
  , _LineMode
  , _PolyLineMode
  , _PolygonMode
  , _RectangleMode
  , _CircleMode
  , _TextMode
  , _MathMode


  , matches
  ) where


import Base
import Control.Lens
import Control.Lens.Extras (is)
import Data.Default
import PolyLineMode
import PolygonMode
import RectangleMode
import SelectMode

--------------------------------------------------------------------------------
data Mode = SelectMode {-# UNPACK #-}!SelectModeData
          | PanMode
          | PointMode
          | PenMode
          | LineMode
          | PolyLineMode {-# UNPACK #-}!PolyLineModeData
          | PolygonMode {-# UNPACK #-}!PolygonModeData
          | RectangleMode {-# UNPACK #-}!RectangleModeData
          | CircleMode
          | TextMode
          | MathMode
          deriving (Show,Read,Eq)
makePrisms ''Mode




-- | Returns if the two match; i.e if the modes are the same (ignoring any specific data
-- they may have.)
matches    :: Mode -> Mode -> Bool
matches m1 = \case
  SelectMode{}     -> is _SelectMode    m1
  PanMode          -> is _PanMode       m1
  PointMode        -> is _PointMode     m1
  PenMode          -> is _PenMode       m1
  LineMode         -> is _LineMode      m1
  PolyLineMode{}   -> is _PolyLineMode  m1
  PolygonMode{}    -> is _PolygonMode   m1
  RectangleMode{}  -> is _RectangleMode m1
  CircleMode       -> is _CircleMode    m1
  TextMode         -> is _TextMode      m1
  MathMode         -> is _MathMode      m1


--------------------------------------------------------------------------------

-- --------------------------------------------------------------------------------

-- data instance ModeData LineMode     =
--   LineModeData () deriving (Show,Eq)

-- instance Default (ModeData LineMode) where
--   def = LineModeData ()

-- data instance ModeData PolygonMode  =
--   PolygonModeData () deriving (Show,Eq)

-- instance Default (ModeData PolygonMode) where
--   def = PolygonModeData ()


-- data instance ModeData RectangleMode=
--   RectangleModeData () deriving (Show,Eq)

-- instance Default (ModeData RectangleMode) where
--   def = RectangleModeData ()

-- data instance ModeData CircleMode   =
--   CircleModeData () deriving (Show,Eq)

-- instance Default (ModeData CircleMode) where
--   def = CircleModeData ()

-- data instance ModeData TextMode     =
--   TextModeData () deriving (Show,Eq)

-- instance Default (ModeData TextMode) where
--   def = TextModeData ()

-- data instance ModeData MathMode     =
--   MathModeData () deriving (Show,Eq)

-- instance Default (ModeData MathMode) where
--   def = MathModeData ()
