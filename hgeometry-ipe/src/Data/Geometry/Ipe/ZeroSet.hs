module Data.Geometry.Ipe.ZeroSet where

import           Algorithms.BinarySearch
import           Control.Lens
import           Data.Ext
import           Data.Foldable
import           Data.Geometry.Box
-- import qualified Data.Geometry.CatmulRomSpline as CatmulRom
import           Data.Geometry.Ipe.Attributes
import           Data.Geometry.Ipe.Color
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

import           Data.Geometry.Ipe.Writer

import           Debug.Trace

--------------------------------------------------------------------------------

type R = RealNumber 10

drawZeroCell :: Fractional r => IpeOut (Either v Sign :+ Cell r) Ipe.Path r
drawZeroCell = \(p :+ c) -> case p of
                              Left _     -> drawCell c ! attr SFill blue
                              Right Zero -> drawCell c ! attr SFill green
                              Right _    -> drawCell c


test' :: IO ()
test' = writeIpeFile "/tmp/test.ipe" . singlePageFromContent $
        [ -- iO $ drawQuadTreeWith drawZeroCell testT
        -- ,
          iO $ defIO pl
        ]
  where
    f   :: Point 2 R -> R
    f q = (r^2) - squaredEuclideanDist origin (realToFrac <$> q)
    r = 90.5 :: R -- draw circle of radius r

    Just pl = traceZero' defaultZeroConfig (fromSignum f) Zero startSeg rect

    startSeg :: LineSegment 2 () R
    startSeg = ClosedLineSegment (ext $ origin) (ext $ Point2 0 100)
    rect     = box (ext $ origin) (ext $ Point2 300 300)



-- testT :: QuadTree (Quadrants Sign) (Either (Quadrants Sign) Sign)
-- testT = fromZeros (Cell 8 origin) f
--   where
--     r = 90.5 :: R -- draw circle of radius r


-- centerPoints :: (Functor f, Fractional r) => f (p :+ Cell r) -> f (Point 2 r :+ p)
-- centerPoints = fmap (\(p :+ c) -> midPoint c :+ p)


-- traceZeroFrom'            :: forall zero r b v. (Eq zero, Fractional r, Ord r)
--                          => zero -- ^ the zero value
--                          -> Point 2 r -> QuadTree v (Either b zero)
--                          -> Maybe (PolyLine 2 () r)
-- traceZeroFrom' = toPolyLineWith findZeroOnEdge

--



-- findZeroOnEdge :: Fractional r => LineSegment 2 sign r -> Point 2 r
-- findZeroOnEdge = interpolate (1 / 2)





-- startCell :: Maybe (Either (Quadrants Sign) Sign :+ Cell)
-- startCell = findLeaf myStartP testT

-- trace startP qt = case findLeaf startP qt of
--                     Nothing        -> []
--                     Just startCell -> NonEmpty.toList $ explorePathWith (const True) startCell zCells
--   where
--     zCells = NonEmpty.filter (\(p :+ _) -> isZeroCell Zero p)
--            . leaves $ qt'
--     qt' = withCells qt
    -- start =

    -- [start] = filter (\(_ :+ c) -> startP `intersects` c) zCells



-- testTrace = trace myStartP testT

-- myStartP = (Point2 0 (90.5 :: Rational))

-- [startX] = filter (\(_ :+ c) -> myStartP `intersects` c) zCells

-- findS :: [(p :+ Cell) :+ e] -> [(p :+ Cell) :+ e]
-- findS = filter (\((_ :+ c) :+ _) -> myStartP `intersects` c)


-- zCells = NonEmpty.filter (\(p :+ _) -> isZeroCell p)
--        . leaves . withCells $ testT




-- testTrace =
