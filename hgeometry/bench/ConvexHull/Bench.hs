module Algorithms.Geometry.ConvexHull.Bench (benchmark) where

-- import qualified HGeometry.ConvexHull.DivideAndConquer as DivideAndConquer
import qualified HGeometry.ConvexHull.GrahamScan as GrahamScan
-- import qualified HGeometry.ConvexHull.JarvisMarch      as JarvisMarch
-- import qualified HGeometry.ConvexHull.QuickHull        as QuickHull

import           System.Random
-- import           Data.Double.Approximate
import           Data.Ext
import           HGeometry.Point
import           HGeometry.Vector
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.RealNumber.Rational
import           Test.Tasty.Bench
import           Data.Hashable
import qualified Data.List as List

--------------------------------------------------------------------------------

type R = RealNumber 5

instance (Uniform core, Uniform extra) => Uniform (core :+ extra)

instance (UniformRange core, UniformRange extra) => UniformRange (core :+ extra) where
  -- uniformRM (lc :+ le, hc :+ he) g = (:+) <$> uniformRM (lc, hc) g <*> unfiormRM (le,he) g

-- instance Uniform r => Uniform (Vector d r)


randomPoints   :: Uniform point => Int -> [point]
randomPoints n = take n points
  where
    points = List.unfoldr (Just . uniform) gen

genPts   :: (Uniform r, OptVector_ 2 r, Uniform (VectorFamily 2 r))
         => Int -> NonEmpty (Point 2 r :+ ())
genPts n = NonEmpty.fromList $ randomPoints n

gen :: StdGen
gen = mkStdGen (hash "convex hull")

-- | Benchmark building the convexHull
benchmark    :: Benchmark
benchmark = bgroup "ConvexHull" $
      [ bgroup ("1e"++show i ++ "/RealNumber") (convexHullFractional $ genPts @R n)
      | i <- [3, 4::Int]
      , let n = 10^i
      ] ++
      [ bgroup ("1e"++show i ++ "/Int") (convexHullNum $ genPts @Int n)
      | i <- [4, 5::Int]
      , let n = 10^i
      ] ++
      -- [ bgroup ("1e"++show i ++ "/SafeDouble") (convexHullFractional $ genPts @SafeDouble n)
      -- | i <- [4, 5::Int]
      -- , let n = 10^i
      -- ] ++
      [ bgroup ("1e"++show i ++ "/Double") (convexHullFractional $ genPts @Double n)
      | i <- [4, 5::Int]
      , let n = 10^i ]
  where
    convexHullFractional pts =
                [ bench "GrahamScan" $ nf GrahamScan.convexHull pts
                -- , bench "DivideAndConquer" $ nf DivideAndConquer.convexHull pts
                -- , bench "QuickHull" $ nf QuickHull.convexHull pts
                -- , bench "JarvisMarch" $ nf JarvisMarch.convexHull pts
                ]
    convexHullNum pts =
                [ bench "GrahamScan" $ nf GrahamScan.convexHull pts
                -- , bench "DivideAndConquer" $ nf DivideAndConquer.convexHull pts
                -- , bench "JarvisMarch" $ nf JarvisMarch.convexHull pts
                ]
