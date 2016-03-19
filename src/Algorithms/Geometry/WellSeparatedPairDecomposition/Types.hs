{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.WellSeparatedPairDecomposition.Types where

import Control.Lens
import Data.Ext
import Data.Geometry.Point

import qualified Data.Sequence as S

--------------------------------------------------------------------------------


type Idx = Int


data ShortSide = L | R deriving (Show,Eq)

data FindAndCompact d r p = FAC { _leftPart  :: !(S.Seq (Point d r :+ p))
                                , _rightPart :: !(S.Seq (Point d r :+ p))
                                , _shortSide :: !ShortSide
                                }
makeLenses ''FindAndCompact
