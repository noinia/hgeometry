{-# LANGUAGE QuasiQuotes #-}
module Main( main ) where

import Data.Maybe
import Data.Foldable
import Control.DeepSeq
import Data.Word
import HGeometry.Kernel
import Control.Lens
import System.Random
import Data.Foldable1
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Test.Tasty.Bench
import HGeometry.Plane.BatchPointLocation
import Data.Map.NonEmpty qualified as NEMap
import Data.Set qualified as Set
import HGeometry.Line.BatchPointLocation qualified as Line
import Data.Time
import Prelude hiding (lines)
-- import Data.Fixed

import HGeometry.Plane
import HGeometry.Combinatorial.Util
import Ipe
import System.OsPath
import R

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- | pre: n > 0
--
-- renerates n random points, taken uniformly at random.
randomPoints   :: ( Uniform point
                  ) => Int -> IO (NonEmpty point)
randomPoints n = NonEmpty.fromList . take n . points <$> getStdGen
  where
    points = List.unfoldr (Just . uniform)

-- | Generates random planes
randomPlanes   :: Int -> IO (NonEmpty (Plane R))
randomPlanes n = NonEmpty.fromList . take n . planes <$> getStdGen
  where
    planes = List.unfoldr (Just . genPlane)

    genPlane     :: RandomGen gen => gen -> (Plane R, gen)
    genPlane gen = let (pts, gen') = uniform gen
                   in case fromPoints (pts&mapped %~ fromPoint) of
                        Nothing -> genPlane gen'
                        Just h  -> (h,gen')

fromPoints :: forall r. (Ord r, Fractional r) => Vector 3 (Point 3 r) -> Maybe (Plane r)
fromPoints = asNonVerticalHyperPlane @(HyperPlane 3 r) . hyperPlaneThrough


--------------------------------------------------------------------------------

-- | Helper function to generate uniformly random points at integer
-- coordinates in an [0,100]^3 grid.
fromPoint   :: Point 3 Word -> Point 3 R
fromPoint p = p&coordinates %~ \x -> fromIntegral x / rescale
  where
    rescale = fromIntegral $ ((maxBound :: Word) `div` 100)


--------------------------------------------------------------------------------


naivePlanesAbove                :: forall queryPoint r plane set.
                                   ( Point_ queryPoint 3 r
                                   , Plane_ plane r
                                   , Foldable set
                                   , Ord r, Fractional r
                                   , Ord plane
                                   , Ord queryPoint
                                   )
                                => NonEmpty queryPoint
                                -> set plane
                                -> NEMap.NEMap queryPoint (Set.Set plane)
naivePlanesAbove queries planes =
    foldMap1 (\q -> NEMap.singleton q (Set.filter (q `liesBelow`) planes')) queries
  where
    planes' = foldMap Set.singleton planes

    liesBelow       :: queryPoint -> plane -> Bool
    q `liesBelow` h = verticalSideTest q h /= GT

--------------------------------------------------------------------------------

runExperiment     :: Int -> Int -> IO ()
runExperiment r n = do
    putStrLn $ "r = " <> show r <> ", n = " <> show n
    planes  <- force                      <$> randomPlanes r
    queries <- force . fmap fromPoint     <$> randomPoints n

    let lines = mapMaybe (\(Two h1 h2) -> projectedIntersectionLine h1 h2) $ uniquePairs planes
        ds    = Line.pointLocationStructureIn (Rect (-1) (-1) 128 128) lines
        -- res = Line.groupQueries (projectPoint @2 <$> queries) lines
    ds `seq` (pure ())
    -- print ds
    -- print res
    -- print "========="
    -- let grs = groupQueries queries planes
    -- print grs
    -- print "==== results ======"
    -- traverse_ (print . answerBatch planes) grs
    tBatched <- timed $ batchedPointLocation queries planes
    tNaive   <- timed $ naivePlanesAbove     queries planes
    putStrLn $ "batched point loc: " <> show tBatched
    putStrLn $ "naive: " <> show tNaive
    putStrLn "========="

timed    :: NFData a => a -> IO NominalDiffTime
timed x = do
    before <- getCurrentTime
    x `deepseq` (pure ())
    after <- getCurrentTime
    pure $ diffUTCTime after before


main :: IO ()
main = do
  setStdGen $ mkStdGen 12453
  traverse_ (\r -> runExperiment r (r^5)) [10] -- [10, 15, 20]


{-
  -- let vd = voronoiDiagram pts
  -- print vd
  defaultMain
    [ bgroup "Batched point location/computing conflict lists Benchmarks"
        [ bench "Via Batched PointLoc" $ nf (uncurry batchedPointLocation) (queries, planes)
        , bench "Brute Force"          $ nf (uncurry naivePlanesAbove)     (queries, planes)
        ]
    ]

-}
