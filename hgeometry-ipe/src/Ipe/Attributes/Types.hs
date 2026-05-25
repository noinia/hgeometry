{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Ipe.Attributes.Types
  ( PinType(..)
  , TransformationTypes(..)
  , HorizontalAlignment(..)
  , VerticalAlignment(..)
  , TeXStyle
  , TextSizeUnit(..)
  , IpeSize(..)
  , IpePen(..)
  , IpeDash(..)
  , FillType(..)
  , IpeOpacity
  , IpeTiling
  , IpeGradient
  , IpeArrow(..), arrowName, arrowSize
  , normalArrow
  , LineJoin(..), _Miter, _Round, _Bevel
  ) where

import Control.Lens
import Data.Text (Text)
import Ipe.Value
import Data.Default
import Data.Finitary
import GHC.Generics

--------------------------------------------------------------------------------
-- * Implementations for Common Attributes

-- | Possible values for Pin
data PinType = No | Yes | Horizontal | Vertical
             deriving (Eq,Show,Read,Generic)
             deriving anyclass (Finitary)

-- | Possible values for Transformation
data TransformationTypes = Affine | Rigid | Translations
                         deriving (Show,Read,Eq,Generic)
                         deriving anyclass (Finitary)

--------------------------------------------------------------------------------
-- * Text Attributes

-- these Attributes are speicifc to IpeObjects representing TextLabels
-- and MiniPages. The same structure as for the `CommonAttributes'
-- applies here.

data HorizontalAlignment = AlignLeft | AlignHCenter | AlignRight
                         deriving (Show,Read,Eq,Ord,Generic)
                         deriving anyclass (Finitary)


data VerticalAlignment = AlignTop | AlignVCenter | AlignBottom | AlignBaseline
                       deriving (Show,Read,Eq,Ord,Generic)
                       deriving anyclass (Finitary)

-- | Should be a symbolic name.
type TeXStyle = Text

-- | size of text in points
newtype TextSizeUnit r = TextSizeUnit r
                       deriving stock (Show,Read,Eq,Ord,Functor,Foldable,Traversable,Generic)

--------------------------------------------------------------------------------
-- * Symbol Attributes

-- | The optional Attributes for a symbol
-- data SymbolAttributeUniverse = SymbolStroke | SymbolFill | SymbolPen | Size
--                              deriving (Show,Eq)

-- | Size
newtype IpeSize  r = IpeSize  (IpeValue r)
                   deriving (Show,Eq,Ord,Functor,Foldable,Traversable,Generic)
-- | Pen/Thickness
newtype IpePen   r = IpePen   (IpeValue r)
                   deriving (Show,Eq,Ord,Functor,Foldable,Traversable,Generic)

-------------------------------------------------------------------------------
-- * Path Attributes

-- | Possible values for Dash
data IpeDash r = DashNamed Text
               | DashPattern [r] r
               deriving (Show,Eq,Functor,Foldable,Traversable,Generic)

-- | Allowed Fill types
data FillType = Wind | EOFill
              deriving (Show,Read,Eq,Generic)
              deriving anyclass (Finitary)

-- | IpeOpacity, IpeTyling, and IpeGradient are all symbolic values
type IpeOpacity  = Text
type IpeTiling   = Text
type IpeGradient = Text


-- | A type Representing possible LineJoin values
data LineJoin = Miter | Round | Bevel
  deriving (Show,Read,Eq,Generic)
  deriving anyclass (Finitary)
-- in ipe; they are converted to Int values in the range [0,2]

instance Default LineJoin where
  def = Round

makePrisms ''LineJoin

-- | Possible values for an ipe arrow
data IpeArrow r = IpeArrow { _arrowName :: Text
                           , _arrowSize :: IpeSize r
                           } deriving (Show,Eq,Functor,Foldable,Traversable,Generic)
makeLenses ''IpeArrow

-- | A normal arrow
normalArrow :: IpeArrow r
normalArrow = IpeArrow "normal" (IpeSize $ Named "normal")

--------------------------------------------------------------------------------
-- * Group Attributes

-- | The only group attribute is a Clip

-- A clipping path is a Path. Which is defined in Ipe.Types. To
-- avoid circular imports, we define GroupAttrElf and GroupAttribute there.
