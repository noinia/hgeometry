{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Algorithms.Geometry.LowerEnvelope.LowerEnvSpec where

import           Data.Vinyl (RecElem, Rec)
import           Data.Vinyl.TypeLevel (RIndex)
import qualified Data.Singletons
import qualified Algorithms.Geometry.LowerEnvelope.DualCH as DualCH
import           Control.Lens
import           Data.Eq.Approximate
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Ipe
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.Proxy
import           GHC.TypeLits
import           Test.Hspec
import           Util

-- import Algorithms.Geometry.LowerEnvelope.Types

spec :: Spec
spec = testCases "test/Algorithms/Geometry/LowerEnvelope/manual.ipe"

testCases    :: FilePath -> Spec
testCases fp = (runIO $ readInput fp) >>= \case
    Left e    -> it "reading Smallest enclosing disk file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


type Approx r = AbsolutelyApproximateValue (ToleranceProxy 3) r

data TestCase r = TestCase { _lines    :: NonEmpty (Line 2 r :+ ())
                           , _color    :: Maybe (IpeColor r)
                           , _solution :: [Point 2 (Approx r)]
                           }
                  deriving (Show,Eq)


readInput    :: FilePath -> IO (Either ConversionError [TestCase Rational])
readInput fp = fmap f <$> readSinglePageFile fp
  where
    f page = [ let c = lookup' $ NonEmpty.head lSet
               in TestCase ((\l -> l^.core.to supportingLine :+ ()) <$> lSet)
                           c
                           (solutionOf c)
             | lSet <- mapMaybe NonEmpty.nonEmpty $ byStrokeColour segs
             ]
      where
        segs :: [LineSegment 2 () Rational :+ IpeAttributes Path Rational]
        segs = page^..content.traverse._withAttrs _IpePath _asLineSegment
        pts  = page^..content.traverse._IpeUse

        solutionOf c = [ AbsolutelyApproximateValue <$> p^.core.symbolPoint
                       | p <- pts, lookup' p == c
                       ]


lookup' :: RecElem Rec 'Stroke 'Stroke ats ats (RIndex 'Stroke ats)
        => (core :+ Attributes f ats) -> Maybe (Data.Singletons.Apply f 'Stroke)
lookup' (_ :+ ats) = lookupAttr (Proxy :: Proxy 'Stroke) ats


toSpec                    :: (Fractional r, Ord r, Show r) => TestCase r -> Spec
toSpec (TestCase ls c sol) = it ("testing the " <> show c <> " set") $
  (map (approx . (^.core))
   . DualCH.vertices . DualCH.lowerEnvelope $ ls) `shouldBe` sol


approx :: Point 2 value -> Point 2 (AbsolutelyApproximateValue absolute_tolerance value)
approx = fmap AbsolutelyApproximateValue

-- shouldApprox       :: forall f r. ( Functor f, Eq (f (Approx r))
--                                   , Show (f (Approx r))
--                                   )
--                    => f r -> f r -> Expectation
-- a `shouldApprox` b = a' `shouldBe` b'
--   where
--     a' :: f (Approx r)
--     a' = AbsolutelyApproximateValue <$> a
--     b' :: f (Approx r)
--     b' = AbsolutelyApproximateValue <$> b


newtype ToleranceProxy a = ToleranceProxy (Proxy a)
  deriving (Show, Eq)

instance KnownNat n => AbsoluteTolerance (ToleranceProxy n) where
  absoluteToleranceOf = toleranceFromKnownNat . getAbsoluteTolerance

toleranceFromKnownNat   :: (Fractional r, KnownNat n) => proxy n -> r
toleranceFromKnownNat p = recip . fromInteger $ (10 :: Integer) ^ (natVal p)

instance KnownNat n => RelativeTolerance (ToleranceProxy n) where
  relativeToleranceOf = toleranceFromKnownNat . getRelativeTolerance

instance KnownNat n => ZeroTolerance (ToleranceProxy n) where
  zeroToleranceOf = toleranceFromKnownNat . getZeroTolerance

--     where
--       f   :: Fractional r => AbsolutelyApproximateValue (Proxy n) r -> r
--       f _ = recip $ (fromInteger 10) ^^ (natVal (Proxy :: Proxy n))
