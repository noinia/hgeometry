{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.QuadTree.Draw where

import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Ipe.Attributes
import           Data.Geometry.Ipe.IpeOut
import           Data.Geometry.Ipe.Types
import           Data.Geometry.QuadTree
import           Data.Geometry.QuadTree.Cell
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T
import           Data.Tree.Util (TreeNode(..))
import           Data.Vinyl.Notation
import           Data.Vinyl.Core
--------------------------------------------------------------------------------

drawCell :: Fractional r => IpeOut (Cell r) Path r
drawCell = \c -> ipeRectangle (toBox c)

drawQuadTree :: (Fractional r, Ord r) => IpeOut (QuadTree v p r) Group r
drawQuadTree = drawQuadTreeWith (\(_ :+ c) -> drawCell c)

drawQuadTreeWith           :: (ToObject i, Fractional r, Ord r)
                           => IpeOut (p :+ Cell r) i r -> IpeOut (QuadTree v p r) Group r
drawQuadTreeWith drawCell' = ipeGroup . fmap (iO . drawCell') . leaves . withCells

quadTreeLevels           :: forall i r v p. (ToObject i, Fractional r, Ord r
                                            )
                         => IpeOut (TreeNode v p :+ Cell r) i r -> IpeOut (QuadTree v p r) Group r
quadTreeLevels drawCell' = \qt -> let lvls = fmap (fmap flip') . perLevel . withCells $ qt
                                  in ipeGroup . fmap iO . zipWith drawLevel [1..] . F.toList $ lvls
  where
    flip' = \case
      InternalNode (v :+ c) -> InternalNode v :+ c
      LeafNode (l :+ c)     -> LeafNode l     :+ c

    -- drawLevel   :: Int -> IpeOut (NonEmpty (TreeNode v p :+ Cell r)) Group r
    drawLevel i = ipeGroup . fmap (\n -> iO $ ipeGroup [iO $ drawCell' n] ! attr SLayer (layer i))

    layer   :: Int -> LayerName
    layer i = LayerName $ "level_" <> (T.pack $ show i)
