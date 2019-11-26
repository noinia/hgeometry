{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module ThreeSum where

import Data.Ext
import Control.Applicative
import Data.Monoid
import Data.Function(on)
import Control.Lens hiding (only)
import Data.Geometry
import Data.Geometry.Ipe
import GHC.Exts
import Data.List(sortBy)

minInfty = -100
maxInfty =  100


data ThreeSum' = ThreeSum' [Integer] [Integer] [Integer]
                deriving (Show,Eq)

data GeomBase = GeomBase [Point 2 Double] [Point 2 Double] [Point 2 Double]
                deriving (Show,Eq)

type instance NumType GeomBase = Double

instance HasDefaultIpeOut GeomBase where
  type DefaultIpeOut GeomBase = Group

  defaultIpeOut = IpeOut $ \(GeomBase as bs cs) -> Group [f as, f bs, f cs] :+ mempty
    where
      f = asIpeGroup . map (flip asIpeObject mempty)


geomBase                      :: ThreeSum' -> GeomBase
geomBase (ThreeSum' as bs cs) = GeomBase aps bps cps
  where
    aps = map (\a -> Point2 (fromInteger a)     0) as
    bps = map (\b -> Point2 (fromInteger b)     2) bs
    cps = map (\c -> Point2 (fromInteger c / 2) 1) cs





data Separator1 = Separator1 [LineSegment 2 () Double]
                             [LineSegment 2 () Double]
                             [LineSegment 2 () Double]
                  deriving (Show,Eq)


type instance NumType Separator1 = Double


instance HasDefaultIpeOut Separator1 where
  type DefaultIpeOut Separator1 = Group

  defaultIpeOut = IpeOut $ \(Separator1 as bs cs) -> Group [f as, f bs, f cs] :+ mempty
    where
      f = asIpeGroup . map (flip asIpeObject mempty)


separator1 (GeomBase as bs cs) = Separator1 als bls cls
  where
    sort' = sortBy (compare `on` (^.xCoord))
    eps   = 1/4

    shiftL, shiftR :: Point 2 Double -> Point 2 Double
    shiftL p = p&xCoord %~ (subtract eps)
    shiftR p = p&xCoord %~ (+ eps)

    -- generate a single segment
    seg     :: Point 2 Double -> Point 2 Double -> LineSegment 2 () Double
    seg l r = ClosedLineSegment (only $ shiftR l) (only $ shiftL r)

    segs      :: Double -> [Point 2 Double] -> [LineSegment 2 () Double]
    segs y xs = let ys = Point2 minInfty y : (sort' xs) ++ [Point2 maxInfty y]
                in zipWith seg ys (tail ys)

    als = segs 0 as
    bls = segs 2 bs
    cls = segs 1 cs



-- data Strip r = Strip { _leftBoundary :: Line 2 r, _rightBoundary :: Line 2 r }
--                deriving (Show,Eq)

-- type instance NumType (Strip r) = r

-- -- instance Coordinate r => HasDefaultIpeOut (Strip r) where
-- --   type DefaultIpeOut (Strip r) = Path

-- --   defaultIpeOut = IpeOut $ \(Strip l r) -> asIpe polyLine pl :+ mempty
-- --     where
-- --       -- pl =

-- --       f = flip asIpeGroup . map (flip asIpeObject mempty)






-- data StripsCoverBox = StripsCoverBox [Strip Double] (Rectangle () Double)
--                       deriving (Show,Eq)

-- -- | dualize a horizontal segment by rotating 90 degrees and transforming it
-- -- into a strip
-- dualStrip (ClosedLineSegment (l :+ _) (r :+ _)) = Strip (dualLine . fl $ l)
--                                                         (dualLine . fl $ r)
--   where
--     fl p = Point2 (p^.yCoord) (- p^.xCoord)

-- stripsCoverBox :: Separator1 -> StripsCoverBox
-- stripsCoverBox (Separator1 as bs cs) = StripsCoverBox strips rect
--   where

--     strips = map dualStrip (as ++ bs ++ cs)
--     -- this is a bit of hack, but that should not matter too much.
--     r      = boundingBoxList [Point2 minInfty minInfty, Point2 maxInfty maxInfty]









main = do
  print "A:"
  as <- map read . words <$> getLine
  print "B:"
  bs <- map read . words <$> getLine
  print "C:"
  cs <- map read . words <$> getLine
  let threeSum' = ThreeSum' as bs cs
      f         = separator1 . geomBase
      prob      = flip asIpeObject mempty . f $ threeSum'
  printAsIpeSelection prob
