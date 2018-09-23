{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Ipe.Color where

import           Data.Colour.SRGB (RGB(..))
import           Data.Geometry.Ipe.Value
import           Data.Text
--------------------------------------------------------------------------------

newtype IpeColor r = IpeColor (IpeValue (RGB r))    deriving (Show,Read,Eq)

instance Ord r => Ord (IpeColor r) where
  (IpeColor c) `compare` (IpeColor c') = fmap f c `compare` fmap f c'
    where
      f (RGB r g b) = (r,g,b)

-- | Creates a named color
named :: Text -> IpeColor r
named = IpeColor . Named

--------------------------------------------------------------------------------
-- * Basic Named colors

red :: IpeColor r
red = named "red"

green :: IpeColor r
green = named "green"

blue :: IpeColor r
blue = named "blue"

yellow :: IpeColor r
yellow = named "yellow"

orange :: IpeColor r
orange = named "orange"

gold :: IpeColor r
gold = named "gold"

purple :: IpeColor r
purple = named "purple"

gray :: IpeColor r
gray = named "gray"

brown :: IpeColor r
brown = named "brown"

navy :: IpeColor r
navy = named "navy"

pink :: IpeColor r
pink = named "pink"

seagreen :: IpeColor r
seagreen = named "seagreen"

turquoise :: IpeColor r
turquoise = named "turquoise"

violet :: IpeColor r
violet = named "violet"

darkblue :: IpeColor r
darkblue = named "darkblue"

darkcyan :: IpeColor r
darkcyan = named "darkcyan"

darkgray :: IpeColor r
darkgray = named "darkgray"

darkgreen :: IpeColor r
darkgreen = named "darkgreen"

darkmagenta :: IpeColor r
darkmagenta = named "darkmagenta"

darkorange :: IpeColor r
darkorange = named "darkorange"

darkred :: IpeColor r
darkred = named "darkred"

lightblue :: IpeColor r
lightblue = named "lightblue"

lightcyan :: IpeColor r
lightcyan = named "lightcyan"

lightgray :: IpeColor r
lightgray = named "lightgray"

lightgreen :: IpeColor r
lightgreen = named "lightgreen"

lightyellow :: IpeColor r
lightyellow = named "lightyellow"
