{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Algorithms.Geometry.SmallestEnclosingDisk.RISpec where

import Control.Monad(when)
import System.Random(mkStdGen)
import Data.Vinyl
import Control.Lens
import Data.Ext
import Data.Maybe
import Data.Proxy
import Data.Function(on)
import Test.Hspec
import qualified Data.List as L
import Data.Geometry
import Data.Geometry.Ball(fromDiameter, disk, Disk)
import Data.Geometry.Ipe hiding (disk)

import Algorithms.Geometry.SmallestEnclosingBall.Types
import qualified Algorithms.Geometry.SmallestEnclosingBall.RandomizedIncrementalConstruction as RIC
import qualified Algorithms.Geometry.SmallestEnclosingBall.Naive as Naive


import Debug.Trace

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
toSpec (TestCase pts sol)  | traceShow pts False = undefined
toSpec (TestCase pts sol) =
    describe ("testing point set with solution " ++ show sol) $ do
      it "comparing with naive solution" $
        ((RIC.smallestEnclosingDisk (mkStdGen 2123) pts)^.enclosingDisk)
        `shouldBe`
        ((Naive.smallestEnclosingDisk pts)^.enclosingDisk)
      when (isJust sol) $
        it "manal solution" $
          ((RIC.smallestEnclosingDisk (mkStdGen 5) pts)^.enclosingDisk)
          `shouldBe`
          (diskOf $ fromJust sol)


diskOf               :: (Fractional r, Eq r)
                     => TwoOrThree (Point 2 r) -> Disk () r
diskOf (Two p q)     = fromDiameter p q
diskOf (Three p q r) = fromMaybe (error "Wrong manual disk") $ disk p q r


-- | Point sets per color
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



byStrokeColour :: (Stroke ∈ IpeObjectAttrF g) => [IpeObject' g r] -> [[IpeObject' g r]]
byStrokeColour = map (map fst) . L.groupBy ((==) `on` snd) . L.sortOn snd
               . map (\x -> (x,lookup' x))
  where
    lookup' (_ :+ ats) = lookupAttr (Proxy :: Proxy Stroke) ats
