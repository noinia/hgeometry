{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.WellSeparatedPairDecomposition.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data types that can represent a well separated pair decomposition (wspd).
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.WellSeparatedPairDecomposition.Types where

import           Control.Lens hiding (Level)
import           Data.BinaryTree
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.Point
import           Data.Geometry.Vector
import qualified Data.LSeq as LSeq
import           Data.Measured.Class
import qualified Data.Sequence as S
import qualified Data.Traversable as Tr

--------------------------------------------------------------------------------

type SplitTree d p r a = BinLeafTree (NodeData d r a) (Point d r :+ p)

type PointSet d p r a = SplitTree d p r a

type WSP d p r a = (PointSet d p r a, PointSet d p r a)

-- | Data that we store in the split tree
data NodeData d r a = NodeData { _splitDim :: !Int
                               , _bBox     :: !(Box d () r)
                               , _nodeData :: !a
                               }
deriving instance (Arity d, Show r, Show a) => Show (NodeData d r a)
deriving instance (Arity d, Eq r,   Eq a)   => Eq   (NodeData d r a)

makeLenses ''NodeData

instance Semigroup v => Measured v (NodeData d r v) where
  measure = _nodeData

instance Functor (NodeData d r) where
  fmap = Tr.fmapDefault

instance Foldable (NodeData d r) where
  foldMap = Tr.foldMapDefault

instance Traversable (NodeData d r) where
  traverse f (NodeData d b x) = NodeData d b <$> f x

--------------------------------------------------------------------------------
-- * Implementation types

type PointSeq d p r = LSeq.LSeq 1 (Point d r :+ p)


data Level = Level { _unLevel   :: Int
                   , _widestDim :: Maybe Int
                   } deriving (Show,Eq,Ord)
makeLenses ''Level

nextLevel             :: Level -> Level
nextLevel (Level i _) = Level (i+1) Nothing



type Idx = Int


data ShortSide = L | R deriving (Show,Eq)

data FindAndCompact d r p = FAC { _leftPart  :: !(S.Seq (Point d r :+ p))
                                , _rightPart :: !(S.Seq (Point d r :+ p))
                                , _shortSide :: !ShortSide
                                }
deriving instance (Arity d, Show r, Show p) => Show (FindAndCompact d r p)
deriving instance (Arity d, Eq r,   Eq p)   => Eq   (FindAndCompact d r p)

makeLenses ''FindAndCompact
