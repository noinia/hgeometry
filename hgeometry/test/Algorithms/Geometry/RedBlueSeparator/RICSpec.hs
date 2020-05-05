{-# LANGUAGE OverloadedStrings #-}
module Algorithms.Geometry.RedBlueSeparator.RICSpec where

import           Algorithms.Geometry.RedBlueSeparator.RIC
import           Control.Lens
import           Control.Monad.Random.Strict (evalRand)
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Ipe
import           Data.Geometry.Ipe.Color
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           System.Random (mkStdGen)
import           Test.Hspec
-- import           Test.Util

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


-- | reports the points ont hte other side as p
differentSide     :: (Ord r, Num r)
                  => Point 2 r -> Line 2 r -> [Point 2 r] -> [Point 2 r]
differentSide p l = let s = p `onSideUpDown` l
                    in filter (\q -> not $ (q `onSideUpDown` l) `elem` [s,On])

allSameSide             :: (Ord r, Num r)
                        => NonEmpty (Point 2 r :+ extra) -> Line 2 r -> [Point 2 r]
allSameSide pts l = case f pts of
                         []     -> [] -- all on the line
                         (p:ps) ->  differentSide p l ps
  where
    f = NonEmpty.filter (\p -> p `onSideUpDown` l /= On) . fmap (^.core)


-- -- | Returns the list of points on the wrong side; so the result should be empty
-- separates :: NonEmpty (Point 2 r :+ ()) -> NonEmpty (Point 2 r :+ ()) ->
--   Line 2 r -> [Point 2 r]
-- separates reds@(r:|_) blues@(b:|_) l = filter (differentSideAs r l)  reds
--                                      <>  filter (differentSideAs r l)  reds

-- | Make sure reds and blues are on opposite sides
onDifferentSides              :: (Ord r, Num r)
                              => NonEmpty (Point 2 r :+ extra)
                              -> NonEmpty (Point 2 r :+ extra) -> Line 2 r
                              -> [Point 2 r]
onDifferentSides reds blues l = case (f reds, f blues) of
                                  (r:_, b:_) | r `onSideUpDown` l == b `onSideUpDown` l -> [r,b]
                                  _                                                     -> []
  where
    f = NonEmpty.filter (\p -> p `onSideUpDown` l /= On) . fmap (^.core)

-- | Reports points that are on the wrong side
separates                                    :: (Ord r, Num r)
                                             => NonEmpty (Point 2 r :+ extra1)
                                             -> NonEmpty (Point 2 r :+ extra1)
                                             -> Maybe (Line 2 r)
                                             -> ([Point 2 r], [Point 2 r], [Point 2 r])
separates reds blues = \case
  Nothing -> ([],[],[])
  Just l -> ( allSameSide reds l
            , allSameSide blues l
            , onDifferentSides reds blues l
            )

toSpec                    :: (Fractional r, Ord r, Show r) => TestCase r -> Spec
toSpec (TestCase reds blues sol) = describe "Red Blue Separator tests" $ do
    it "same as manual" $ do
      (flip evalRand (mkStdGen 5) $
        IsJust <$> separatingLine reds blues)
      `shouldBe`
        (IsJust $ if sol then Just undefined else Nothing)
    it "separates the points" $ do
      (flip evalRand (mkStdGen 35) $
        separates reds blues <$> separatingLine reds blues)
      `shouldBe` ([],[],[]) -- no points on the wrong sides


newtype IsJust a = IsJust (Maybe a) deriving (Show)
instance Eq a => Eq (IsJust a) where
  (IsJust a) == (IsJust b) = isJust a == isJust b


-- | Point sets per color, Crosses form the solution
readInput    :: FilePath -> IO (Either ConversionError (NonEmpty (TestCase Rational)))
readInput fp = fmap (fmap f . view pages) <$> readIpeFile fp
  where
    f      :: IpePage Rational -> TestCase Rational
    f page = TestCase blues reds sol
      where
        -- if we are not using disks than there is no separtor
        sol = fromMaybe False $ (\p -> p^.core.symbolName == "mark/disk(sx)")
                                <$> firstOf (content.traverse._IpeUse) page

        syms = page^..content.traverse._IpeUse
        -- extract red and blue points
        blues = g (named "blue") syms
        reds  = g (named "red") syms

        g c = NonEmpty.fromList
            . map (\p -> ext $ p^.core.symbolPoint)
            . filter (\p -> p^?extra._Attr SStroke == Just c)
