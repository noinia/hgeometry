module Main where

import Criterion.Main
import Demo.TriangulateWorld


main = defaultMain [ bench "triangulateWorld" $ nfIO (mainWith $ Options "/Users/frank/tmp/antartica.ipe" "/tmp/out.ipe")
                   ]
