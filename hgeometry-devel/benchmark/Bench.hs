module Main where

import qualified Algorithms.Geometry.ConvexHull.DivideAndConquer as CH2D
import qualified Algorithms.Geometry.ConvexHull.Minimalist as Minimalist
import           Control.DeepSeq
import           Control.Lens
import           Criterion.Main
import           Criterion.Types
import           Data.Ext
import           Data.Geometry.Point
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Generate
import           Ipe
import           Test.QuickCheck.Instances ()
import Data.RealNumber.Rational

--------------------------------------------------------------------------------

type R = RealNumber 5

main :: IO ()
main = defaultMainWith cfg [ benchmark ]
  where
    cfg = defaultConfig { reportFile = Just "bench.html" }

benchmark :: Benchmark
benchmark = bgroup "convexHullBench"
    [ env (genPoints @R 10000) benchBuild
    ]

--------------------------------------------------------------------------------


-- | Benchmark building the convexHull
benchBuild    :: (Ord r, Fractional r, NFData r
                 , Show r, IpeWriteText r
                 ) => NonEmpty (Point 3 r :+ ()) -> Benchmark
benchBuild ps = bgroup "build" [ bgroup (show n) (build $ take' n ps)
                               | n <- sizes' ps
                               ]
  where
    take' n = NonEmpty.fromList . NonEmpty.take n
    sizes' _ = [2000]

    build pts = [ bench "sort"                 $ nf NonEmpty.sort pts
                , bench "2D CH"                $ nf ch2d pts
                -- , bench "3D CH"                $ nf DivideAndConquer.lowerHull' pts
                , bench "3D CH Minim"          $ nf Minimalist.lowerHull' pts
                -- , bench "Div&Conq Old"         $ nf oldDivAndConquer            pts
                ]
      where
        ch2d = CH2D.lowerHull
             . fmap (&core %~ \(Point3 x _ z) -> Point2 x z)


-- oldDivAndConquer :: (Ord r, Num r) => NonEmpty.NonEmpty (Point 2 r :+ p) -> ConvexPolygon p r
-- oldDivAndConquer = DivideAndConquer.unMerge
--                  . foldMap1 (DivideAndConquer.Merge . ConvexPolygon . fromPoints . (:[]) . _unElem)
--                  . asBalancedBinLeafTree
--                  . NonEmpty.sortBy (compare `on` (^.core))
