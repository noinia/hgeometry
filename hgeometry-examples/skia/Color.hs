{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE OverloadedStrings      #-}
module Color
  ( Color, ColorF(..)
  , fromRGB24
  , rgba
  , Alpha(..)
  , fromAlpha

  , opacity

  , Coloring(..)
  ) where

import Control.Lens(Lens', lens)
import Data.Colour (black)
import Data.Colour.SRGB
import Data.Default.Class
import Data.Word (Word8)
import Miso.String (MisoString, ToMisoString(..), ms)

--------------------------------------------------------------------------------

data ColorF a = Color (Colour a) (Alpha Float)
           deriving (Eq)
deriving instance Show (Colour a) => Show (ColorF a)

type Color = ColorF Float

-- | Lens to access the opacity
opacity :: Lens' (ColorF a) (Alpha Float)
opacity = lens (\(Color _ a) -> a) (\(Color rgb _) a -> Color rgb a)


instance Num a => Default (ColorF a) where
  def = Color black Opaque

-- | Create a color from three word8 values (i.e. integer values in the range [0..255])
fromRGB24       :: Word8 -> Word8 -> Word8 -> Color
fromRGB24 r g b = Color (sRGB24 r g b) Opaque

-- | render as rgba(r,g,b,a)
rgba              :: Color -> MisoString
rgba (Color c al) = let RGB r g b = ms <$> toSRGB24 c
                        a         = ms $ fromAlpha 1 al
                    in "rgba(" <> r <> ", " <> g <> ", " <> b <> ", " <> a <> ")"

instance ToMisoString Word8 where
  toMisoString = toMisoString . show


data Alpha a = Alpha a | Opaque
  deriving (Show,Eq,Ord,Functor)

fromAlpha        :: a -> Alpha a -> a
fromAlpha opaque = \case
  Alpha x -> x
  Opaque -> opaque


--------------------------------------------------------------------------------

-- | Coloring type; we should have a stroke, a fill, or both
data Coloring = StrokeOnly    !Color
              | FillOnly             !Color
              | StrokeAndFill !Color !Color  -- ^ stroke and fill
              deriving (Show,Eq)

instance Default Coloring where
  def = StrokeOnly def
