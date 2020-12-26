module Algorithms.Geometry.SoS.SortSpec where

import           Algorithms.Geometry.SoS
import           Control.Lens
import           Control.Monad (forM_)
import           Data.Geometry.Vector
import qualified Data.List as List
import           Data.Proxy
import           Data.Util
import           GHC.TypeNats
import           Test.Hspec
import           Test.QuickCheck
--------------------------------------------------------------------------------

spec :: Spec
spec = do describe "SoS Sorting" $ do
            specSort (Proxy @1)
            specSort (Proxy @2)
            specSort (Proxy @3)
            specSort (Proxy @4)



specSort    :: forall proxy d. (Arity d, SortI d) => proxy d -> Spec
specSort pd = describe ("sorting " <> show d) $ do
                forM_ (List.permutations [1..d]) $ \xs -> do
                  let vs          = vectorFromListUnsafe @d xs
                      (n,sortedV) = sortI vs
                  describe (show xs) $ do
                    it "sorted "             $ sortedV `shouldBe` idV
                    it "number of exchanges" $ Just n  `shouldBe` exchangesNeeded idV vs
  where
    d   = natVal pd
    idV = vectorFromListUnsafe [1..d]

exchangesNeeded     :: forall a d. (Eq a, Arity d) => Vector d a -> Vector d a -> Maybe Int
exchangesNeeded u v = List.find (\i -> any (== u) $ exchanges i v) [0..d]
  where
    d   = fromIntegral $ natVal (Proxy @d)

-- | Generates all vectors we can create by i exchanges.
exchanges                 :: (Eq a, Arity d) => Int -> Vector d a -> [Vector d a]
exchanges i v | i == 0    = [v]
              | otherwise = concatMap (exchanges $ i-1) $ singleExchange v


-- | Generate single echanges
singleExchange   :: forall a d. (Eq a, Arity d) => Vector d a -> [Vector d a]
singleExchange v = map (singleSwap v) $ uniquePairs [0..(d-1)]
  where
    d   = fromIntegral $ natVal (Proxy @d)

singleSwap             :: (Eq a, Arity d) => Vector d a -> Two Int -> Vector d a
singleSwap v (Two i j) = imap f v
  where
    f k x | k == i    = v^?!ix j
          | k == j    = v^?!ix i
          | otherwise = x
