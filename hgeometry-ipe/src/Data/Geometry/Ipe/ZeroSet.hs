{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Ipe.ZeroSet where

import           Algorithms.BinarySearch
import           Control.Lens
import           Data.Ext
import           Data.Foldable
import           Data.Geometry.Box
-- import qualified Data.Geometry.CatmulRomSpline as CatmulRom
import           Data.Geometry.Ipe.Attributes
import           Data.Geometry.Ipe.Color
import           Data.Geometry.Ball
import           Data.Geometry.Ipe.IpeOut
import qualified Data.Geometry.Ipe.Types as Ipe
import           Data.Geometry.Ipe.Types (singlePageFromContent)
import           Data.Geometry.Point
import           Data.Geometry.LineSegment
import           Data.Geometry.ZeroSet
import           Data.Geometry.PolyLine (PolyLine)
import qualified Data.Geometry.PolyLine as PolyLine
import           Data.Geometry.QuadTree
import           Data.Geometry.QuadTree.Cell
import           Data.Geometry.QuadTree.Draw
import           Data.Geometry.QuadTree.Quadrants
import           Data.Geometry.QuadTree.Split
import           Data.Geometry.QuadTree.Tree (Tree(..))
import qualified Data.Geometry.QuadTree.Tree as Tree
import           Data.Geometry.Vector
import           Data.Intersection
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import           Data.Tree.Util (TreeNode(..))
import           Data.Geometry.Ipe.Writer

import           Debug.Trace

--------------------------------------------------------------------------------

type R = RealNumber 10

drawZeroCell' :: Fractional r => IpeOut (Either v Sign :+ Cell r) Ipe.Path r
drawZeroCell' = \(p :+ c) -> case p of
                               Left _     -> drawCell c ! attr SFill blue
                               Right Zero -> drawCell c ! attr SFill green
                               Right _    -> drawCell c


drawCorners :: Fractional r => IpeOut (Either (Corners Sign) p :+ Cell r) Ipe.Group r
drawCorners = \(p :+ c) -> ipeGroup $ case p of
                              Left ss -> toList $ draw <$> cellCorners c <*> ss
                              Right _ -> []
  where
    draw     :: Point 2 r -> Sign -> Ipe.IpeObject r
    draw q s = iO $ defIO q ! attr SStroke (toColor s)


drawCell' :: Fractional r => IpeOut (TreeNode (Corners Sign) p :+ Cell r) Ipe.Group r
drawCell' = \(tn :+ c) -> ipeGroup [ iO $ drawCell c, iO $ drawCorners (toEither tn :+ c)
                                   ]

toEither :: TreeNode v p -> Either v p
toEither = \case
  InternalNode v -> Left v
  LeafNode l     -> Right l

toColor :: Sign -> IpeColor r
toColor = \case
  Zero     -> purple
  Positive -> blue
  Negative -> red


drawZeroCell            :: Fractional r => IpeOut (Either (Corners Sign) Sign :+ Cell r) Ipe.Group r
drawZeroCell = \z -> ipeGroup [ iO $ drawZeroCell' z, iO $ drawCorners z]


addD    :: [Ipe.IpeObject R] -> [Ipe.IpeObject Double]
addD xs = map (fmap $ realToFrac @R @Double) xs
        <> [iO $ defIO $ Circle (ext origin) (r*r)]
  where
    r = 90.5

test' :: IO ()
test' = writeIpeFile "/tmp/test.ipe" . singlePageFromContent . addD $
        [ -- iO $ drawQuadTreeWith (drawZeroCell @R) qt ! attr SLayer "qt"
         iO $ defIO pl ! attr SLayer "pl"
        , iO $ quadTreeLevels drawCell' qt
        ]
  where
    f   :: Point 2 R -> R
    f q = (r^2) - squaredEuclideanDist origin (realToFrac <$> q)
    r = 90.5 :: R -- draw circle of radius r

    Just pl = traceZero' cfg (fromSignum f) Zero startSeg rect

    startSeg :: LineSegment 2 () R
    startSeg = ClosedLineSegment (ext $ origin) (ext $ Point2 0 100)
    rect     :: Rectangle () R
    rect     = box (ext $ origin) (ext $ Point2 300 300)
    qt = fromZerosWith' (limitWidthTo $ cfg^.maxDepth) (fitsRectangle rect) (fromSignum f)

    cfg = defaultZeroConfig
