import Test.Tasty.Bench


import qualified Data.List as List
import HGeometry.Vector
import HGeometry.Vector.Unpacked.V2
import System.Random.Stateful (UniformRange(..), Uniform(..))

--------------------------------------------------------------------------------

sortBy     :: ( Foldable f
              ) => (a -> a -> Ordering) -> f a -> f a
sortBy cmp = undefined

--------------------------------------------------------------------------------

randomPoints   :: Uniform point => Int -> IO [point]
randomPoints n = replicateM n uniformIO


fibo :: Int -> Integer
fibo n = if n < 2 then toInteger n else fibo (n - 1) + fibo (n - 2)

main :: IO ()
main = defaultMain
  [
    bgroup "sorting tests"
    [ bench "fifth"     $ nf fibo  5
    , bench "tenth"     $ nf fibo 10
    , bench "twentieth" $ nf fibo 20
    ]
  ]
