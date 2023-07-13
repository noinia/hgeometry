module Main where

-- import           Control.DeepSeq (force)
-- import           Test.Tasty.Bench
import qualified ConvexHull.Bench as ConvexHull
-- import qualified ConvexHull.GrahamBench as Graham

--------------------------------------------------------------------------------

main :: IO ()
main = ConvexHull.runBenchmark
          -- ConvexHull.runProfile

allBenchmarks :: IO ()
allBenchmarks = do
                   -- Graham.runBenchmark
                   ConvexHull.runBenchmark
