{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.WellSeparatedPairDecomposition.Types where

import Control.Lens
import Data.Ext
import Data.Geometry.Point
import Data.Geometry.Box
import Data.BinaryTree

import qualified Data.Seq2 as S2
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr

--------------------------------------------------------------------------------

type SplitTree d p r a = BinLeafTree (NodeData d p r a) (Point d r :+ p)

type PointSet d p r a = SplitTree d p r a

type WSP d p r a = (PointSet d p r a, PointSet d p r a)

-- | Data that we store in the split tree
data NodeData d p r a = NodeData { _splitDim :: !Int
                                 , _bBox     :: Box d p r
                                 , _nodeData :: a
                                 }
makeLenses ''NodeData

instance Functor (NodeData d p r) where
  fmap = Tr.fmapDefault

instance F.Foldable (NodeData d p r) where
  foldMap = Tr.foldMapDefault

instance Tr.Traversable (NodeData d p r) where
  traverse f (NodeData d b x) = NodeData d b <$> f x



--------------------------------------------------------------------------------
-- * Implementation types

type PointSeq d p r = S2.ViewL1 (Point d r :+ p)


newtype Level = Level { unLevel :: Int } deriving (Show,Eq,Ord,Enum)

type Idx = Int


data ShortSide = L | R deriving (Show,Eq)

data FindAndCompact d r p = FAC { _leftPart  :: !(S.Seq (Point d r :+ p))
                                , _rightPart :: !(S.Seq (Point d r :+ p))
                                , _shortSide :: !ShortSide
                                }
makeLenses ''FindAndCompact
