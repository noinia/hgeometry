{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Algorithms.Geometry.LinearProgramming.LinearProgrammingRICSpec where

import Algorithms.Geometry.LinearProgramming.LP2DRIC
import Algorithms.Geometry.LinearProgramming.Types
import Data.Maybe(mapMaybe)
import Control.Lens hiding (below)
import Data.Ext
import Data.Geometry
import Data.Geometry.Ipe
import Data.Geometry.Ipe.Color(named)
import Data.Geometry.HalfSpace
import Data.Geometry.Properties
import Data.List.NonEmpty(NonEmpty)
import Data.Ratio
import Test.Hspec
-- import Util

--------------------------------------------------------------------------------

testLP :: LinearProgram 2 Rational
testLP = LinearProgram (Vector2 0 (-1)) [ above $ Line (Point2 0 0) (Vector2 2    1)
                                        , above $ Line (Point2 0 3) (Vector2 1    (-1))
                                        , above $ Line (Point2 0 1) (Vector2 3    1)
                                        , above $ Line (Point2 0 5) (Vector2 2    (-1))
                                        ]


test :: IO (Maybe (Point 2 Rational))
test = solveBoundedLinearProgram testLP

--------------------------------------------------------------------------------

inAllHalfpaces       :: (Arity d, Ord r, Num r)
                     => Maybe (Point d r) -> LinearProgram d r -> Bool
inAllHalfpaces mp lp = case mp of
    Nothing -> True
    Just p  -> all (p `intersects`) $ lp^.constraints

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "2D LP Tests" $ do
         it "in all hafspaces" $
           inAllHalfpaces (solveBoundedLinearProgram' testLP) testLP
           `shouldBe` True
         -- prop "in all hafspaces" $
         --   \(lp :: LinearProgram 2 Rational) ->
         --   inAllHalfpaces (solveBoundedLinearProgram' lp) lp

         testCases "test/Algorithms/Geometry/LinearProgramming/manual.ipe"


--------------------------------------------------------------------------------


testCases    :: FilePath -> Spec
testCases fp = (runIO $ readInput fp) >>= \case
    Left e    -> it "reading 2D Linear Programming file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


data TestCase r = TestCase { _aboves :: [Line 2 r]
                           , _belows :: [Line 2 r]
                           , _solution :: Maybe (Point 2 r)
                           }
                  deriving (Show,Eq)


toLP                    :: Num r => TestCase r -> LinearProgram 2 r
toLP (TestCase as bs _) = LinearProgram (Vector2 0 (-1)) ((map above as) <> (map below bs))


toSpec                       :: (Fractional r, Ord r, Show r, Real r) => TestCase r -> Spec
toSpec tc@(TestCase _ _ sol) =
    describe ("testing 2D LP with solution " ++ show sol) $ do
     it "manal solution" $
       (fmap (fmap Approx) . solveBoundedLinearProgram' $ toLP tc)
       `shouldBe`
       (fmap (fmap Approx) sol)

-- ipe's intersection business is not exact, so a quick hack that calls things equal
-- if they are very close
newtype Approx r = Approx r deriving (Show)
instance (Ord r, Real r, Fractional r) => Eq (Approx r) where
  (Approx a) == (Approx b) = realToFrac (abs (a - b)) < (1%1000)


-- | Point sets per color, Crosses form the solution
readInput    :: FilePath -> IO (Either ConversionError (NonEmpty (TestCase Rational)))
readInput fp = fmap (fmap f . view pages) <$> readIpeFile fp
  where
    f      :: IpePage Rational -> TestCase Rational
    f page = TestCase blues reds sol
      where
        -- interpret any point as the solution
        sol = (^.core.symbolPoint) <$> firstOf (content.traverse._IpeUse) page

        paths = page^..content.traverse._IpePath
        -- blue segs interpreted as halfplanes above the blue seg
        blues = g (named "blue") paths
        -- red segs are interpreted as below the red seg
        reds  = g (named "red") paths

        g   :: IpeColor Rational -> [Path Rational :+ IpeAttributes Path Rational]
            -> [Line 2 Rational]
        g c = mapMaybe (\p -> p^?core._asLineSegment.to supportingLine)
            . filter (\p -> p^?extra._Attr SStroke == Just c)
