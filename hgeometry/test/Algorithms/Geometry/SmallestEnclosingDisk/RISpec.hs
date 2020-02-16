{-# LANGUAGE OverloadedStrings #-}
module Algorithms.Geometry.SmallestEnclosingDisk.RISpec where

import qualified Algorithms.Geometry.SmallestEnclosingBall.Naive as Naive
import qualified Algorithms.Geometry.SmallestEnclosingBall.RIC as RIC
import           Algorithms.Geometry.SmallestEnclosingBall.Types
import           Control.Lens
import           Control.Monad (when)
import           Control.Monad.Random.Strict (evalRand)
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Ball (fromDiameter, disk, Disk)
import           Data.Geometry.Ipe
import           Data.Maybe
import           Data.Proxy
import           System.Random (mkStdGen)
import           Test.Hspec
import           Test.Util


spec :: Spec
spec = testCases "test/Algorithms/Geometry/SmallestEnclosingDisk/manual.ipe"

testCases    :: FilePath -> Spec
testCases fp = (runIO $ readInput fp) >>= \case
    Left e    -> it "reading Smallest enclosing disk file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


data TestCase r = TestCase { _pointSet :: [Point 2 r :+ ()]
                           , _solution :: Maybe (TwoOrThree (Point 2 r))
                           }
                  deriving (Show,Eq)


toSpec                    :: (Fractional r, Ord r, Show r) => TestCase r -> Spec
toSpec (TestCase pts sol) =
    describe ("testing point set with solution " ++ show sol) $ do
      it "comparing with naive solution" $
        (flip evalRand (mkStdGen 2123) $
          view enclosingDisk <$> RIC.smallestEnclosingDisk pts)
        `shouldBe`
        ((Naive.smallestEnclosingDisk pts)^.enclosingDisk)
      when (isJust sol) $
        it "manal solution" $
          (flip evalRand (mkStdGen 5) $
            view enclosingDisk <$> RIC.smallestEnclosingDisk pts)
          `shouldBe`
          (diskOf $ fromJust sol)


diskOf               :: (Fractional r, Eq r)
                     => TwoOrThree (Point 2 r) -> Disk () r
diskOf (Two p q)     = fromDiameter p q
diskOf (Three p q r) = fromMaybe (error "Wrong manual disk") $ disk p q r


-- | Point sets per color, Crosses form the solution
readInput    :: FilePath -> IO (Either ConversionError [TestCase Rational])
readInput fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ TestCase [p^.core.symbolPoint :+ () | p <- pSet] (solutionOf pSet)
             | pSet <- byStrokeColour syms
             ]
      where
        syms = page^..content.traverse._IpeUse

        -- | Crosses form a solution
        isInSolution s = s^.core.symbolName == "mark/cross(sx)"

        right = either (const Nothing) Just
        solutionOf = right . fromList . map (^.core.symbolPoint) . filter isInSolution
