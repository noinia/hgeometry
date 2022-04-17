module Data.Tree.Draw where

import           Data.Ext
import           Geometry.LineSegment
import           Geometry.Point
import           Ipe
import           Data.Tree

--------------------------------------------------------------------------------

-- | Draws a tree
drawTree' :: IpeOut (Tree (Point 2 r :+ p)) Group r
drawTree' = ipeGroup . map (iO . defIO  . uncurry ClosedLineSegment) . treeEdges


treeEdges              :: Tree a -> [(a,a)]
treeEdges (Node v chs) = map ((v,) . rootLabel) chs ++ concatMap treeEdges chs
