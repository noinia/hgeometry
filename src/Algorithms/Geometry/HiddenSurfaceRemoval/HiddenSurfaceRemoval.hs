module Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemoval where

import Control.Applicative
import Control.Lens
import Data.Ext
import Data.Geometry.Point
import Data.Geometry.LineSegment
import Data.Geometry.Triangle
import Data.Geometry.Line
import Data.Geometry.Arrangement
import Data.Geometry.PlanarSubdivision
import Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.List as List
import Data.Semigroup
--------------------------------------------------------------------------------

type PriQ a = [a]



render       :: (Ord r, Fractional r)
             => proxy s -> [Triangle 2 v r :+ (Triangle 3 q r :+ f)]
             -> Arrangement s () () (Maybe f) r
render px ts = undefined
  where
    arr = constructArrangement px $ concatMap f ts
    f t = map (\s -> supportingLine s :+ SP s t) . sideSegments $ t^.core


type T v q f r = SP (LineSegment 2 v r) (Triangle 2 v r :+ (Triangle 3 q r :+ f))

data EdgeSide = L | R | None deriving (Show,Eq)

--
--
-- >>> fromAssignments id 5 [0 :+ 0, 1 :+ 1, 3 :+ 3, 4 :+ 4]
-- [Just 0,Just 1,Nothing,Just 3,Just 4]
fromAssignments            :: (k -> Int) -> Int -> [k :+ v] -> V.Vector (Maybe v)
fromAssignments idxOf n as = V.create $ do
                               vec <- MV.replicate n Nothing
                               mapM_ (\(k :+ v) -> MV.modify vec (<|> Just v) (idxOf k)) as
                               pure vec

scanAll     :: (Ord r, Fractional r)
            => Arrangement s () (T v q f r) () r
            -> SP [Dart s :+ EdgeSide] [FaceId' s :+ Maybe (T v q f r)]
scanAll arr = mconcat . map (scanLine arr) $ arr^.inputLines

-- | Sweeps along line l in the arrangement, maintaining the first/topmost
-- visible triangle during the sweep. While doing this we thus figure out
-- 1) if  we should draw the current edge of the arrangmeent
-- 2) which triangle is the top-triangle just left of the line
-- 3) which triangle is the top-triangle just right of the line.
--
-- We collect this information in the form of assignments: i.e. for a
-- dart/faceId we simply remember what data to assign it to.
-- (we do this so that we can process all these assignments in a batch.)
scanLine              :: (Ord r, Fractional r)
                      => Arrangement s () (T v q f r) () r
                      -> Line 2 r :+ T v q f r
                      -> SP [Dart s :+ EdgeSide] [FaceId' s :+ Maybe (T v q f r)]
scanLine arr (l :+ t) = (\(STR ds fs _) -> SP ds fs)
                      . List.foldl' f (STR [] [] mempty) $ traverseLine l arr
  where
    isActive d = let (u,v) = bimap (^.location) (^.location) $ arr^.subdivision.endPointsOf d
                 in u `onSegment` (t^._1) && v `onSegment` (t^._1)

    f (STR ds fs st) d | isActive d = STR (da <> ds) (fa <> fs) st
                       | otherwise  = STR ds fs st
      where
        da = []
        fa = []
  -- TODO: collect the event points; do an actual sweep
