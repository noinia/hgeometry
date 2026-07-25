module AlternatingSpec
  (spec
  ) where

import           Test.Hspec
import qualified Data.Sequence as Seq
import           HGeometry.Sequence.Alternating
import           Control.Lens

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Alternating tests" $ do
          it "reversing list" $
            reversing (Alternating "a" [(3, "c"), (5, "e"), (7, "g")])
            `shouldBe`
            Alternating "g" [(7,"e"),(5,"c"),(3,"a")]
          it "reversing singleton" $
            reversing (Alternating "s" [(5,"t")])
            `shouldBe`
            Alternating "t" [(5,"s")]
          it "reversing Seq" $
            reversing (Alternating "a" (Seq.fromList [(3, "c"), (5, "e"), (7, "g")]))
            `shouldBe`
            Alternating "g" (Seq.fromList [(7,"e"),(5,"c"),(3,"a")])
