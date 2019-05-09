module Main where

import qualified Language.Javascript.JSaddle.Warp as JSaddle
import qualified Svg as Svg


main :: IO ()
main = JSaddle.run 8080 $ Svg.mainJSM
