{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module PolyLineMode
  ( PolyLine'
  , PartialPolyLine(..)
  , _StartPoint, _PartialPolyLine
  ) where

import           Control.Lens hiding (view, element)
import qualified Data.Sequence as Sequence
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Sequence.NonEmpty

--------------------------------------------------------------------------------
type PolyLine' r = PolyLineF ViewR1 (Point 2 r)



data PartialPolyLine r = StartPoint (Point 2 r)
                       | PartialPolyLine (PolyLine' r)
                       deriving (Show,Eq)
makePrisms ''PartialPolyLine


--------------------------------------------------------------------------------
