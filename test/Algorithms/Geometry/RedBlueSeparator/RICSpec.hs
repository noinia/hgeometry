{-# LANGUAGE OverloadedStrings #-}
module Algorithms.Geometry.RedBlueSeparator.RICSpec where

import           Algorithms.Geometry.RedBlueSeparator.RIC
import           Control.Lens
import           Control.Monad.Random.Strict (evalRand)
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Ipe
import           Data.Geometry.Ipe.Color
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           System.Random (mkStdGen)
import           Test.Hspec
-- import           Util

--------------------------------------------------------------------------------

spec :: Spec
spec = testCases "test/Algorithms/Geometry/RedBlueSeparator/manual.ipe"

testCases    :: FilePath -> Spec
testCases fp = (runIO $ readInput fp) >>= \case
    Left e    -> it "reading RedBlue Separator file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


data TestCase r = TestCase { _redSet    :: NonEmpty (Point 2 r :+ ())
                           , _blueSet   :: NonEmpty (Point 2 r :+ ())
                           , _seperable :: Bool
                           }
                  deriving (Show,Eq)

toSpec                    :: (Fractional r, Ord r, Show r) => TestCase r -> Spec
toSpec (TestCase reds blues sol) = it "Red Blue Separator tests" $ do
          (flip evalRand (mkStdGen 5) $
            isJust <$> separatingLine reds blues)
          `shouldBe`
          sol

-- | Point sets per color, Crosses form the solution
readInput    :: FilePath -> IO (Either ConversionError (NonEmpty (TestCase Rational)))
readInput fp = fmap (fmap f . view pages) <$> readIpeFile fp
  where
    f      :: IpePage Rational -> TestCase Rational
    f page = TestCase blues reds sol
      where
        -- interpret any point as the solution
        sol = True
          -- (^.core.symbolPoint) <$> firstOf (content.traverse._IpeUse) page

        syms = page^..content.traverse._IpeUse
        -- extract red and blue points
        blues = g (named "blue") syms
        reds  = g (named "red") syms

        g c = NonEmpty.fromList
            . map (\p -> ext $ p^.core.symbolPoint)
            . filter (\p -> p^.extra.attrLens SStroke == Just c)
