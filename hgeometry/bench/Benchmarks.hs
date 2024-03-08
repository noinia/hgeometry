module Main where

-- import           Control.DeepSeq (force)
-- import           Test.Tasty.Bench
import qualified ConvexHull.Bench as ConvexHull
import qualified ConvexHull.GrahamBench
import           Test.Tasty.Bench (defaultMain)

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
         [
           ConvexHull.benchmark
         , ConvexHull.GrahamBench.benchmark
         ]
