module Algorithms.Geometry.ConvexHull.Bench (benchmark) where

import qualified Algorithms.Geometry.ConvexHull.DivideAndConquer as DivideAndConquer
import qualified Algorithms.Geometry.ConvexHull.GrahamScan       as GrahamScan
import qualified Algorithms.Geometry.ConvexHull.JarvisMarch      as JarvisMarch
import qualified Algorithms.Geometry.ConvexHull.QuickHull        as QuickHull

import           Control.Monad.Random
import           Data.Double.Approximate
import           Data.Ext
import           Data.Geometry.Point
import           Data.Hashable
import           Data.List.NonEmpty       (NonEmpty (..))
import qualified Data.List.NonEmpty       as NonEmpty
import           Data.RealNumber.Rational
import           Test.Tasty.Bench

type R = RealNumber 5

--------------------------------------------------------------------------------

genPts                 :: (Ord r, Random r, RandomGen g)
                       => Int -> Rand g (NonEmpty (Point 2 r :+ ()))
genPts n = NonEmpty.fromList <$> replicateM n (fmap ext getRandom)

-- genPts'      :: (Ord r, Random r, RandomGen g) => Int
--              -> Rand g ( NonEmpty (Point 2 r :+ ())
--                    , NonEmpty (Point 2 r Multi.:+ '[])
--                    )
-- genPts' n = (\pts -> (pts, fmap (\ ~(c :+ _) -> Multi.ext c) pts)
--                ) <$> genPts n

gen :: StdGen
gen = mkStdGen (hash "convex hull")

-- | Benchmark building the convexHull
benchmark    :: Benchmark
benchmark = bgroup "ConvexHull" $
      [ bgroup (show n ++ "/RealNumber") (convexHullFractional $ evalRand (genPts @R n) gen)
      | n <- [1000, 10000::Int]
      ] ++
      [ bgroup (show n ++ "/Int") (convexHullNum $ evalRand (genPts @Int n) gen)
      | n <- [10000::Int, 100000]
      ] ++
      [ bgroup (show n ++ "/SafeDouble") (convexHullFractional $ evalRand (genPts @SafeDouble n) gen)
      | n <- [10000::Int, 100000]
      ]
  where
    convexHullFractional pts =
                [ bench "GrahamScan" $ nf GrahamScan.convexHull pts
                , bench "DivideAndConquer" $ nf DivideAndConquer.convexHull pts
                , bench "QuickHull" $ nf QuickHull.convexHull pts
                , bench "JarvisMarch" $ nf JarvisMarch.convexHull pts
                ]
    convexHullNum pts =
                [ bench "GrahamScan" $ nf GrahamScan.convexHull pts
                , bench "DivideAndConquer" $ nf DivideAndConquer.convexHull pts
                , bench "JarvisMarch" $ nf JarvisMarch.convexHull pts
                ]
