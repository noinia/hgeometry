module Modes
  ( Mode(..)
  ) where

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
