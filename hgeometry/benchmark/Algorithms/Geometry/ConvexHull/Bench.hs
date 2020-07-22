module Algorithms.Geometry.ConvexHull.Bench where

import qualified Algorithms.Geometry.ConvexHull.DivideAndConquer as DivideAndConquer
import qualified Algorithms.Geometry.ConvexHull.GrahamScan as GrahamScan

-- | copies of the convex hull algo with different point types
import qualified Algorithms.Geometry.ConvexHull.GrahamV2   as GV
import qualified Algorithms.Geometry.ConvexHull.GrahamFam  as GFam
import qualified Algorithms.Geometry.ConvexHull.GrahamFamPeano  as GPeano
import qualified Algorithms.Geometry.ConvexHull.GrahamFam6  as GFam6
import qualified Algorithms.Geometry.ConvexHull.GrahamFixed as GFix
import qualified Data.Ext.Multi as Multi
import           Benchmark.Util
import           Control.DeepSeq
import           Control.Lens
import           Criterion.Main
import           Criterion.Types
import           Data.Ext
import           Data.Geometry.Point
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy
import           Test.QuickCheck


-- import           Data.Semigroup.Foldable
-- import Data.BinaryTree
-- import Data.Geometry.Polygon.Convex
-- import Control.Lens
-- import Data.Geometry.Polygon
-- import           Data.Function (on)

--------------------------------------------------------------------------------

main :: IO ()
main = do
         pts <- genPts' (Proxy :: Proxy Int) 10000
         print pts
         defaultMainWith cfg [ benchBuild pts ]
  where
    cfg = defaultConfig { reportFile = Just "bench.html" }

-- main :: IO ()
-- main = defaultMainWith cfg [ benchmark ]
--   where
--     cfg = defaultConfig { reportFile = Just "bench.html" }

benchmark :: Benchmark
benchmark = bgroup "convexHullBench"
    [ env (genPts' (Proxy :: Proxy Int) 10000) benchBuild
    ]

--------------------------------------------------------------------------------

genPts     :: (Ord r, Arbitrary r) => proxy r -> Int -> IO (NonEmpty (Point 2 r :+ ()))
genPts _ n = generate (NonEmpty.fromList <$> vectorOf n arbitrary)

genPts'      :: (Ord r, Arbitrary r) => proxy r -> Int
             -> IO ( NonEmpty (Point 2 r :+ ())
                   , NonEmpty (Point 2 r Multi.:+ '[])
                   )
genPts' px n = (\pts -> (pts, fmap (\ ~(c :+ _) -> Multi.ext c) pts)
               ) <$> genPts px n


-- | Benchmark building the convexHull
benchBuild    :: forall r. (Ord r, Num r, NFData r)
              => ( NonEmpty (Point 2 r :+ ())
                 , NonEmpty (Point 2 r Multi.:+ '[])
                 )
              -> Benchmark
benchBuild ps = bgroup "build" [ bgroup (show n) (build ps) -- $ take' n ps)
                               | n <- [10000]
                               ]
  where
    -- take'' n = NonEmpty.fromList . NonEmpty.take n

    -- take' n  = bimap (take'' n) (take'' n)

    sizes'   :: a -> [Int]
    sizes' _ = [10000]

    -- build            :: (Ord a, Ord b) => (NonEmpty a, NonEmpty b) -> [Benchmark]
    build (pts,pts') =
                [ bench "sort"                 $ nf NonEmpty.sort pts
                , bench "sort-Multi Ext"       $ nf NonEmpty.sort pts'
                -- , bench "sort_Linear.V2"       $ nf NonEmpty.sort ptsV2
                -- , bench "sort_FamPeano"        $ nf NonEmpty.sort ptsFamPeano
                -- , bench "sort_Family"          $ nf NonEmpty.sort ptsFam
                -- , bench "sort_Family6"         $ nf NonEmpty.sort ptsFam6
                -- , bench "sort_Fixed"           $ nf NonEmpty.sort ptsFix

                -- , bench "grahamScan"           $ nf GrahamScan.convexHull pts
                -- , bench "grahamScan_Linear.V2" $ nf GV.convexHull         ptsV2

                -- , bench "grahamScan_FamPeano"  $ nf GPeano.convexHull     ptsFamPeano
                -- , bench "grahamScan_Family"    $ nf GFam.convexHull       ptsFam
                -- , bench "grahamScan_Fixed"     $ nf GFix.convexHull       ptsFix

                -- , bench "Div&Conq"             $ nf DivideAndConquer.convexHull pts

                -- , bench "Div&Conq Old"         $ nf oldDivAndConquer            pts
                ]
      -- where
      --   ptsV2       = fmap (GV.fromP) pts
      --   ptsFamPeano = fmap (GPeano.fromP) pts
      --   ptsFam      = fmap (GFam.fromP) pts
      --   ptsFam6     = fmap (GFam6.fromP) pts
      --   ptsFix      = fmap (GFix.fromP) pts



-- oldDivAndConquer :: (Ord r, Num r) => NonEmpty.NonEmpty (Point 2 r :+ p) -> ConvexPolygon p r
-- oldDivAndConquer = DivideAndConquer.unMerge
--                  . foldMap1 (DivideAndConquer.Merge . ConvexPolygon . fromPoints . (:[]) . _unElem)
--                  . asBalancedBinLeafTree
--                  . NonEmpty.sortBy (compare `on` (^.core))
