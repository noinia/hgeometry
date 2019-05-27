{-# LANGUAGE ScopedTypeVariables #-}
module Data.OrdSeqSpec where

import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.OrdSeq (OrdSeq)
import qualified Data.OrdSeq as OrdSeq
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "OrdSeq tests" $ do
    it "fromListBy" $
      property $ \(xs :: [Int]) ->
          F.toList (OrdSeq.fromListBy compare xs) `shouldBe` List.sort xs
    it "splitOn, <" $
      property $ \x (xs :: OrdSeq Int) ->
          let (l,_,_) = OrdSeq.splitOn id x xs
          in all (< x) l
    it "splitOn, ==" $
      property $ \x (xs :: OrdSeq Int) ->
          let (_,m,_) = OrdSeq.splitOn id x xs
          in all (== x) m
    it "splitOn, >=" $
      property $ \x (xs :: OrdSeq Int) ->
          let (_,_,r) = OrdSeq.splitOn id x xs
          in all (> x) r
    it "join" $
      property $ \x (xs :: [Int]) -> let (ys,zs) = List.partition (<= x) $ xs in
          (F.toList $ OrdSeq.fromListByOrd ys <> OrdSeq.fromListByOrd zs)
          `shouldBe`
          List.sort (ys <> zs)
    it "positive member" $
      property $ \(xs :: OrdSeq Int) ->
         all (\x -> OrdSeq.memberBy compare x xs) xs
    it "member" $
      property $ \x (xs :: OrdSeq Int) ->
         OrdSeq.memberBy compare x xs
         `shouldBe`
         F.elem x (F.toList xs)
    it "lookupMin" $
       property $ \(xs :: OrdSeq Int) ->
         OrdSeq.lookupMin xs
         `shouldBe`
         (safe minimum $ F.toList xs)
    it "lookupMax" $
       property $ \(xs :: OrdSeq Int) ->
         OrdSeq.lookupMax xs
         `shouldBe`
         (safe maximum $ F.toList xs)


safe      :: ([t] -> a) -> [t] -> Maybe a
safe _ [] = Nothing
safe f xs = Just . f $ xs
