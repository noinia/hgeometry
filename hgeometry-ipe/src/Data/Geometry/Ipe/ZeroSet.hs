module Data.Geometry.Ipe.ZeroSet where

import           Control.Lens
import           Data.Ext
import           Data.Foldable
import           Data.Geometry.Box
import           Data.Geometry.Ipe.Attributes
import           Data.Geometry.Ipe.Color
import           Data.Geometry.Ipe.IpeOut
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Point
import           Data.Geometry.QuadTree
import           Data.Geometry.Vector
import           Data.Intersection
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty


import           Data.Geometry.Ipe.Writer

--------------------------------------------------------------------------------


drawCell :: IpeOut Cell Path R
drawCell = \c -> ipeRectangle (toBox c)

-- drawQuadTree' ::

drawQuadTree :: IpeOut (QuadTree v p) Group R
drawQuadTree = drawQuadTreeWith (\(_ :+ c) -> drawCell c)

drawQuadTreeWith           :: ToObject i => IpeOut (p :+ Cell) i r -> IpeOut (QuadTree v p) Group r
drawQuadTreeWith drawCell' = ipeGroup . fmap (iO . drawCell') . (^.tree.to leaves) . withCells

drawZeroCell :: IpeOut (Either v Sign :+ Cell) Path R
drawZeroCell = \(p :+ c) -> case p of
                              Left _     -> drawCell c ! attr SFill blue
                              Right Zero -> drawCell c ! attr SFill green
                              Right _    -> drawCell c


test' = writeIpeFile "/tmp/test.ipe" . singlePageFromContent . (:[]) . iO
      $ drawQuadTreeWith drawZeroCell testT


testT :: QuadTree (Quadrants Sign) (Either (Quadrants Sign) Sign)
testT = fromZeros (Cell 8 origin) (\q -> (r^2) - squaredEuclideanDist origin (realToFrac <$> q))
  where
    r = 90.5 :: Rational -- draw circle of radius r


-- trace           :: Cell
trace startP qt = explorePathWith (const True) start zCells
  where
    zCells = NonEmpty.filter (\(p :+ _) -> isZeroCell p)
           . (^.tree.to leaves) . withCells $ qt

    [start] = filter (\(_ :+ c) -> startP `intersects` c) zCells



testTrace = trace myStartP testT

myStartP = (Point2 0 (90.5 :: Rational))

[startX] = filter (\(_ :+ c) -> myStartP `intersects` c) zCells

findS :: [(p :+ Cell) :+ e] -> [(p :+ Cell) :+ e]
findS = filter (\((_ :+ c) :+ _) -> myStartP `intersects` c)


zCells = NonEmpty.filter (\(p :+ _) -> isZeroCell p)
           . (^.tree.to leaves) . withCells $ testT


-- testTrace =
