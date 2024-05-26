module LineSegment.BentheyOttmanNeighbourSpec(spec) where

import Data.Function (on)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "overlapsWithNeighborTest" $ do
         prop "sameAsSeparate" $
           \(xs :: [Int]) ->
             propSameAsSeparate (==) xs

propSameAsSeparate p xs = overlapsWithNeighbour p xs `shouldBe` overlapsWithNeighbour2 p xs


-- -- | Compute every element together with its neighbours
-- withNeighbours    :: [a] -> (a,[a])
-- withNeighbours xs = let xs'@(_:succs) = map (:[]) xs
--                     in List.zipWith3 (\p x s -> (x,p <> s)) ([]:xs') (succs <> repeat [])

-- testOverlapNext = [1,2,3,3,3,5,6,6,8,10,11,34,2,2,3]


--------------------------------------------------------------------------------

-- | Given a predicate, test and a list, annotate each element whether
-- it, together with one of its neighbors satisifies the predicate.
overlapsWithNeighbour   :: (a -> a -> Bool) -> [a] -> [(a,Bool)]
overlapsWithNeighbour p = go0
  where
    go0 = \case
      []     -> []
      (x:xs) -> go x False xs

    go x b = \case
      []     -> [(x,b)]
      (y:ys) -> let b' = p x y
                in (x,b || b') : go y b' ys


--------------------------------------------------------------------------------

overlapsWithNext'   :: (a -> a -> Bool) -> [a] -> [(a,Bool)]
overlapsWithNext' p = go
  where
    go = \case
      []           -> []
      [x]          -> [(x,False)]
      (x:xs@(y:_)) -> (x,p x y) : go xs

overlapsWithPrev'   :: (a -> a -> Bool) -> [a] -> [(a,Bool)]
overlapsWithPrev' p = go0
  where
    go0 = \case
      []     -> []
      (x:xs) -> (x,False) : go x xs

    go x = \case
      []     -> []
      (y:ys) -> (y,p x y) : go y ys


overlapsWithNeighbour2 p = map (\((a,b),b') -> (a, b || b'))
                         . overlapsWithNext' (p `on` fst)
                         . overlapsWithPrev' p
