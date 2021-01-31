{-# LANGUAGE PartialTypeSignatures #-}
module WSPDBench where

import Control.DeepSeq
import Control.Lens
import Criterion.Main
import Data.Ext
import Data.Geometry.Point
import Demo.ExpectedPairwiseDistance


readInput'     :: FilePath -> Int -> IO [Point 2 Double :+ _]
readInput' fp k = take k <$> readInput fp


benchWSPD :: Benchmark
benchWSPD = bgroup "Well-Separated Pair Decomposition"
    [ env (readInput' "pco.9420_convert.txt" 200) $ \pts -> bgroup "pco"
        [ bench "exact"     $ nf pairwiseDist                  pts
        , bench "wspd 0.05" $ nf (approxPairwiseDistance 0.05) pts
        , bench "wspd 0.10" $ nf (approxPairwiseDistance 0.10) pts
        , bench "wspd 0.20" $ nf (approxPairwiseDistance 0.20) pts
        ]
    ]

-- main = defaultMain [ benchWSPD ]
