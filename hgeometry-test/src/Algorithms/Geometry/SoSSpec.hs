module Algorithms.Geometry.SoSSpec where

import           Algorithms.Geometry.SoS.Symbolic
-- import           Control.Lens
-- import           Control.Monad (forM_)
import           Data.Foldable
-- import           Data.Geometry.Vector
-- import qualified Data.List as List
-- import           Data.Proxy
-- import           Data.RealNumber.Rational
-- import           Data.Util
-- import           Data.Word
-- import           GHC.TypeNats
import           Test.Hspec
import           Test.QuickCheck
--------------------------------------------------------------------------------



-- data Eps = Eps
--

-- \(\prod_{0 \leq i \leq j} \varepsilon(i)^c > \varepsilon(k)\), for all \(1 \leq j < k\)


-- unfortunately, it seems that this is not really feasilbe to explicitly evaluate thee things.
spec :: Spec
spec = pure ()

-- spec :: Spec
-- spec = describe "SoS Symoblic" $ do
--          let eps' = (0.1 :: RealNumber 10)
--          it "comparing eps-folds " $ property $
--            \(ef1 :: EpsFold SmallInt) ef2 ->
--              let b = suitableBase ef1 `max` suitableBase ef2
--              in compare ef1 ef2 == compare (evalEps b eps' ef1) (evalEps b eps' ef2)

         -- forM_ [1..10] $ \(j :: Integer) ->
         --   forM_ [1..5] $ \c -> it "math" $
         --   let d = c+1
         --   in c * sum [ d^i | i <- [0..j]] < d ^ (j+1) `shouldBe` True
