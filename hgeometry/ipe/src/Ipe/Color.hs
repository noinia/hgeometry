{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.Color
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing colors in ipe as well as the colors available in
-- the standard ipe stylesheet.
--
--------------------------------------------------------------------------------
module Ipe.Color where

import Data.Colour.SRGB (RGB(..))
import Ipe.Value
import Data.Text
import Data.Traversable
--------------------------------------------------------------------------------

-- | Defines a color in Ipe. Colors are either RGB Values or Named
-- values.
newtype IpeColor r = IpeColor (IpeValue (RGB r)) deriving (Show,Read,Eq)

instance Ord r => Ord (IpeColor r) where
  (IpeColor c) `compare` (IpeColor c') = fmap f c `compare` fmap f c'
    where
      f (RGB r g b) = (r,g,b)

instance Functor IpeColor where
  fmap = fmapDefault
instance Foldable IpeColor where
  foldMap = foldMapDefault
instance Traversable IpeColor where
  traverse f (IpeColor v) = IpeColor <$> traverse traverseRGB v
    where
      traverseRGB (RGB r g b) = RGB <$> f r <*> f g <*> f b

-- | Creates a named color
named :: Text -> IpeColor r
named = IpeColor . Named

--------------------------------------------------------------------------------

-- | All basic named colors
basicNamedColors :: [IpeColor r]
basicNamedColors = [ black
                   , white
                   , red
                   , green
                   , blue
                   , yellow
                   , orange
                   , gold
                   , purple
                   , gray
                   , brown
                   , navy
                   , pink
                   , seagreen
                   , turquoise
                   , violet
                   , darkblue
                   , darkcyan
                   , darkgray
                   , darkgreen
                   , darkmagenta
                   , darkorange
                   , darkred
                   , lightblue
                   , lightcyan
                   , lightgray
                   , lightgreen
                   , lightyellow
                   ]

--------------------------------------------------------------------------------
-- * Basic Named colors

-- | a color named 'black'
black :: IpeColor r
black = named "black"

-- | a color named 'white'
white :: IpeColor r
white = named "white"

-- | a color named 'red'
red :: IpeColor r
red = named "red"

-- | a color named 'green'
green :: IpeColor r
green = named "green"

-- | a color named 'blue'
blue :: IpeColor r
blue = named "blue"

-- | a color named 'yellow'
yellow :: IpeColor r
yellow = named "yellow"

-- | a color named 'orange'
orange :: IpeColor r
orange = named "orange"

-- | a color named 'gold'
gold :: IpeColor r
gold = named "gold"

-- | a color named 'pruple'
purple :: IpeColor r
purple = named "purple"

-- | a color named 'gray'
gray :: IpeColor r
gray = named "gray"

-- | a color named 'brown'
brown :: IpeColor r
brown = named "brown"

-- | a color named 'navy'
navy :: IpeColor r
navy = named "navy"

-- | a color named 'pink'
pink :: IpeColor r
pink = named "pink"

-- | a color named 'seagreen'
seagreen :: IpeColor r
seagreen = named "seagreen"

-- | a color named 'turquoise'
turquoise :: IpeColor r
turquoise = named "turquoise"

-- | a color named 'violet'
violet :: IpeColor r
violet = named "violet"

-- | a color named 'darkblue'
darkblue :: IpeColor r
darkblue = named "darkblue"

-- | a color named 'darkcyan'
darkcyan :: IpeColor r
darkcyan = named "darkcyan"

-- | a color named 'darkgray'
darkgray :: IpeColor r
darkgray = named "darkgray"

-- | a color named 'darkgreen'
darkgreen :: IpeColor r
darkgreen = named "darkgreen"

-- | a color named 'darkmagenta'
darkmagenta :: IpeColor r
darkmagenta = named "darkmagenta"

-- | a color named 'darkorange'
darkorange :: IpeColor r
darkorange = named "darkorange"

-- | a color named 'darkred'
darkred :: IpeColor r
darkred = named "darkred"

-- | a color named 'lighbtblue'
lightblue :: IpeColor r
lightblue = named "lightblue"

-- | a color named 'lightcyan'
lightcyan :: IpeColor r
lightcyan = named "lightcyan"

-- | a color named 'lightgray'
lightgray :: IpeColor r
lightgray = named "lightgray"

-- | a color named 'lightgreen'
lightgreen :: IpeColor r
lightgreen = named "lightgreen"

-- | a color named 'lightyellow'
lightyellow :: IpeColor r
lightyellow = named "lightyellow"
