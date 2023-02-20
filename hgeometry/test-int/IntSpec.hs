{-# LANGUAGE AllowAmbiguousTypes #-}
module IntSpec where

import           Data.Proxy
import           Data.Semigroup
import           Data.Typeable
import qualified Data.Vector.Unboxed as UV
import           HGeometry.Vector.Class
import           HGeometry.Vector.Instances ()
import           HGeometry.Vector.Unpacked
import           R
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.Read (readMaybe)
--------------------------------------------------------------------------------

myVec :: Vector 2 R
myVec = Vector2 5 11

vecList :: [Vector 2 R]
vecList = [ myVec
          , Vector2 100 20
          ]

spec :: Spec
spec = describe "Int vector spec" $ do
         it "showtest" $
           show myVec `shouldBe` "Vector2 5 11"
         -- it "division" $
         --   myVec ^/ 2 `shouldBe` Vector2 2 5 -- this should not compile :)
         it "unboxed vector test" $
           let myVecVec :: UV.Vector (Vector 2 R)
               myVecVec = UV.fromList vecList
           in do UV.toList myVecVec `shouldBe` vecList
                 show myVecVec `shouldBe` "[Vector2 5 11,Vector2 100 20]"
                 (UV.toList (myVecVec UV.// [(0, Vector2 200 20)]))
                   `shouldBe`
                   ((Vector2 200 20) : tail vecList)
         describe "show/read tests" $ do
           readShow @(Vector 2 R)
           readShow @(Vector 3 R)
           readShow @(Vector 4 R)
         prop "dot implemented as foldMapZip" $ \(u :: Vector 3 R) v ->
           (getSum $ foldMapZip (\x x' -> Sum $ x * x') u v) == (u `dot` v)


readShow :: forall t. (Read t, Show t, Eq t, Arbitrary t, Typeable t) => Spec
readShow = prop ("Read after show @" <> show (typeRep $ Proxy @t)) $
             \(x :: t) -> readMaybe @t (show x) == Just x
