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
-- import           Data.Geometry.ZeroSet
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
