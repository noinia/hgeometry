module Benchmark.Util where



-- | Generates different size benchmarks
sizes    :: Foldable f => f a -> [Int]
sizes xs = let n = length xs in (\i -> n*i `div` 100) <$> [5,10..100]
