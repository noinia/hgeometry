module Data.Geometry.Ipe.ZeroSet where

import           Data.Ext
import           Data.Foldable
import           Data.Geometry.Box
import           Data.Geometry.Ipe.IpeOut
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.Geometry.ZeroSet

import           Data.Geometry.Ipe.Writer

--------------------------------------------------------------------------------

drawCell :: Fractional r => IpeOut (Cell r) Path r
drawCell = \(Cell p i) -> let w = 2 `pow` i in ipeRectangle $ box (ext p) (ext $ p .+^ Vector2 w w)

-- drawQuadTree' ::

drawQuadTree :: Fractional r => IpeOut (QuadTree v p r) Group r
drawQuadTree = ipeGroup . fmap (\(c :+ _) -> iO $ drawCell c) . cells


-- test' = printAsIpeSelection $ drawQuadTree test
