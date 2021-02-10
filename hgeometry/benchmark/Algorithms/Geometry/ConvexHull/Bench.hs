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
      [ bgroup ("1e"++show i ++ "/RealNumber") (convexHullFractional $ evalRand (genPts @R n) gen)
      | i <- [3, 4::Int]
      , let n = 10^i
      ] ++
      [ bgroup ("1e"++show i ++ "/Int") (convexHullNum $ evalRand (genPts @Int n) gen)
      | i <- [4, 5::Int]
      , let n = 10^i
      ] ++
      [ bgroup ("1e"++show i ++ "/SafeDouble") (convexHullFractional $ evalRand (genPts @SafeDouble n) gen)
      | i <- [4, 5::Int]
      , let n = 10^i
      ] ++
      [ bgroup ("1e"++show i ++ "/Double") (convexHullFractional $ evalRand (genPts @Double n) gen)
      | i <- [4, 5::Int]
      , let n = 10^i ]
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
