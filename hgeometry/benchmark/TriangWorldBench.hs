module Main where

import Criterion.Main
import Demo.TriangulateWorld


main = defaultMain [ bench "triangulateWorld" $ nfIO (mainWith $ Options "/home/frank/tmp/antarctica.ipe" "/tmp/out.ipe")
                   ]
