{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Ipe.ZeroSet where

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
import           Data.Geometry.QuadTree
import           Data.Geometry.QuadTree.Cell
import           Data.Geometry.QuadTree.Draw
import           Data.Geometry.QuadTree.Split
import           Data.RealNumber.Rational
import           Data.Tree.Util (TreeNode(..), _TreeNodeEither)
import           Data.Geometry.Ipe.Writer


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
drawCell' = \(tn :+ c) -> ipeGroup [ iO $ drawCell c, iO $ drawCorners (tn^._TreeNodeEither :+ c)
                                   ]


toColor :: Sign -> IpeColor r
toColor = \case
  Zero     -> purple
  Positive -> blue
  Negative -> red


drawZeroCell            :: Fractional r => IpeOut (Either (Corners Sign) Sign :+ Cell r) Ipe.Group r
drawZeroCell = \z -> ipeGroup [ iO $ drawZeroCell' z, iO $ drawCorners z]

--------------------------------------------------------------------------------


addD    :: [Ipe.IpeObject R] -> [Ipe.IpeObject Double]
addD xs = map (fmap $ realToFrac @R @Double) xs
        <> [iO $ defIO $ Circle (ext origin) (r*r)]
  where
    r = 90.5

test' :: IO ()
test' = writeIpeFile "/tmp/test.ipe" . singlePageFromContent $
        [ -- iO $ drawQuadTreeWith (drawZeroCell @R) qt ! attr SLayer "qt"
          iO $ defIO d1 ! attr SLayer "d1"
        , iO $ defIO d2 ! attr SLayer "d2"
        , iO $ defIO pl ! attr SLayer "pl"
--        , iO $ quadTreeLevels drawCell' qt
        ]
  where
    -- f   :: Point 2 R -> R
    -- f q = (r^2) - squaredEuclideanDist origin (realToFrac <$> q)
    -- r = 90.5 :: R -- draw circle of radius r

    -- Just pl = traceZero' cfg (fromSignum f) Zero startSeg rect

    d1 = Disk (ext origin)          400
    d2 = Disk (ext $ Point2 100 50) 25

    Just pl = traceBisectorDisks cfg d1 d2 rect

    startSeg :: LineSegment 2 () R
    startSeg = ClosedLineSegment (ext $ origin) (ext $ Point2 100 50)
    rect     :: Rectangle () Double
    -- rect     = box (ext $ Point2 64 0) (ext $ Point2 96 32)
    rect     = box (ext $ Point2 (-100) (-100)) (ext $ Point2 100 100)
    -- qt = fromZerosWith' (limitWidthTo $ cfg^.maxDepth) (fitsRectangle rect) (fromSignum f)

    cfg = defaultZeroConfig
