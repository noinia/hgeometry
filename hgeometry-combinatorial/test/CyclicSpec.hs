module CyclicSpec(spec) where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector.NonEmpty as NV
import           HGeometry.Cyclic
import           HGeometry.Foldable.Util
import           HGeometry.Sequence.NonEmpty
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "Cyclic tests" $ do
    describe "HasDirectedTraversals tests" $ do
      prop "traverseRight consistent between vector ans NonEmpty" $
        \i (xs :: NonEmpty Int) ->
          toListOf (traverseRightFrom i) (NV.fromNonEmpty xs)
          ===
          toListOf (traverseRightFrom i) xs
      prop "traverseRight consistent between vector ans NonEmpty" $
        \i (xs :: NonEmpty Int) ->
          toListOf (traverseLeftFrom i) (NV.fromNonEmpty xs)
          ===
          toListOf (traverseLeftFrom i) xs
      prop "traverseRight consistent between vector and NonEmpty Sequence" $
        \i (xs :: NonEmpty Int) ->
          toListOf (traverseRightFrom i) (NV.fromNonEmpty xs)
          ===
          toListOf (traverseRightFrom i) (fromNonEmpty @ViewL1 xs)
      prop "traverseRight consistent between vector and NonEmpty Sequence" $
        \i (xs :: NonEmpty Int) ->
          toListOf (traverseLeftFrom i) (NV.fromNonEmpty xs)
          ===
          toListOf (traverseLeftFrom i) (fromNonEmpty @ViewL1 xs)
