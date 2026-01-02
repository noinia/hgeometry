{-# LANGUAGE QuasiQuotes #-}
module Line.LowerEnvelopeSpec(spec) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as Vector
import           HGeometry.Kernel.Instances ()
import           HGeometry.Line
import           HGeometry.Line.LowerEnvelope
import           R
import           HGeometry.Point
import           HGeometry.Sequence.Alternating
-- import           Paths_hgeometry
-- import           System.OsPath
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck ((===))
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "lower envelope of lines tests" $ do
         it "myLines test" $
           lowerEnvelope myLines `shouldBe` theAnswer
         it "myLines 2 test" $
           lowerEnvelope myLines2 `shouldBe` theAnswer2
         prop "lower envelope correct" $
           \(lines' :: NonEmpty (LineEQ R)) (queries :: NonEmpty R) ->
             let env = lowerEnvelope lines' :: LowerEnvelope (Point 2 R) (LineEQ R)
             in fmap (\q -> evalAt' q $ lineAt q env) queries
                ===
                fmap (naiveQuery lines') queries

-- | naively answer a query by trying every line.
naiveQuery          :: NonEmpty (LineEQ R) -> R -> R
naiveQuery lines' q = minimum $ fmap (evalAt' q) lines'

--------------------------------------------------------------------------------
-- * Some manual testcases


myLines :: NonEmpty (LineEQ R)
myLines = NonEmpty.fromList [ LineEQ 1    1, LineEQ (-1) 2]

theAnswer :: LowerEnvelope (Point 2 R) (LineEQ R)
theAnswer = LowerEnvelope
          $ Alternating (myLines NonEmpty.!! 0)
                        (Vector.fromList $ [ (Point2 (1/2) (3/2), myLines NonEmpty.!! 1)
                                           ]
                        )
----------------------------------------

myLines2 :: NonEmpty (LineEQ R)
myLines2 = NonEmpty.fromList
          [ LineEQ 1    1
          , LineEQ (-1) 2
          , LineEQ 3    3
          ]

theAnswer2 :: LowerEnvelope (Point 2 R) (LineEQ R)
theAnswer2 = LowerEnvelope
           $ Alternating (myLines2 NonEmpty.!! 2)
                         (Vector.fromList $ [ (Point2 (-1) 0,      myLines2 NonEmpty.!! 0)
                                            , (Point2 (1/2) (3/2), myLines NonEmpty.!! 1)
                                            ]
                         )

--------------------------------------------------------------------------------
