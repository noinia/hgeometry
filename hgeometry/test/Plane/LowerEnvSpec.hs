{-# LANGUAGE UndecidableInstances #-}
module Plane.LowerEnvSpec
  ( spec
  ) where

import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Ord (comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HyperPlane
import           HGeometry.Instances ()
import           HGeometry.Intersection
import           HGeometry.NonEmpty.Util
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope.Connected (VertexForm)
import qualified HGeometry.Plane.LowerEnvelope.Connected.BruteForce as BruteForce
import qualified HGeometry.Plane.LowerEnvelope.Connected.Randomized as Randomized
import           HGeometry.Point
import           HGeometry.Vector
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import           Debug.Trace

--------------------------------------------------------------------------------

type R = RealNumber 5


newtype NonDegenerate plane = NonDegenerate (NonEmpty plane)
  deriving newtype (Show,Eq,Foldable,Functor)

instance Arbitrary (NonDegenerate (Plane R)) where
  arbitrary = do h0 <- arbitrary
                 h1 <- arbitrary `suchThat` nonParallelWith h0
                 h2 <- arbitrary `suchThat` \h' -> nonParallelWith h0 h' && nonParallelWith h1 h'
                 rest <- arbitrary
                 pure . NonDegenerate $ h0 :| (h1 : h2 : rest)

-- instance Arbitrary (NonDegenerate (Plane R)) where
--   arbitrary = fmap f <$> arbitrary
--     where
--       f :: Plane Int -> Plane R
--       f = fmap fromIntegral


nonParallelWith      :: Plane R -> Plane R -> Bool
nonParallelWith h h' = case h `intersect` h' of
                         Just (Plane_x_Plane_Line _) -> True
                         _                           -> False




newtype Same = Same (VertexForm R (Plane R))
  deriving newtype (Show)

instance Eq Same where
  (Same env) == (Same env') = and
                            $ NonEmpty.zipWith (<=>) (NEMap.toAscList env) (NEMap.toAscList env')
    where
      (p,defs) <=> (q,defs') = p == q
                               &&
                               Set.fromList (F.toList defs) == Set.fromList (F.toList defs')


spec :: Spec
spec = describe "Lower Envelope tests" $ do
         it "manual" $
           let seed   = 0
               planes = NonDegenerate $ NonEmpty.fromList [ Plane 1    0 2
                                                          , Plane (-1) 0 3
                                                          , Plane 0    1 10
                                                          ]
           in Same (Randomized.computeVertexForm (mkStdGen seed) planes)
              `shouldBe`
              Same (BruteForce.computeVertexForm planes)

         prop "same as brute force" $
           \seed (planes :: NonDegenerate (Plane R)) ->
             Same (Randomized.computeVertexForm (mkStdGen seed) planes)
             ===
             Same (BruteForce.computeVertexForm planes)
