module Data.Geometry.QuadTree.Draw where

import Data.Ext
import Data.Geometry.Ipe.IpeOut
import Data.Geometry.Ipe.Types
import Data.Geometry.QuadTree
import Data.Geometry.QuadTree.Cell

--------------------------------------------------------------------------------

drawCell :: Fractional r => IpeOut (Cell r) Path r
drawCell = \c -> ipeRectangle (toBox c)

drawQuadTree :: (Fractional r, Ord r) => IpeOut (QuadTree v p) Group r
drawQuadTree = drawQuadTreeWith (\(_ :+ c) -> drawCell c)

drawQuadTreeWith           :: (ToObject i, Fractional r, Ord r)
                           => IpeOut (p :+ Cell r) i r -> IpeOut (QuadTree v p) Group r
drawQuadTreeWith drawCell' = ipeGroup . fmap (iO . drawCell') . leaves . withCells
