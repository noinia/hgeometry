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
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Instances ()
import           HGeometry.NonEmpty.Util
import           HGeometry.Number.Real.Rational
import qualified HGeometry.Plane.LowerEnvelope.Connected.BruteForce as BruteForce
import qualified HGeometry.Plane.LowerEnvelope.Connected.Randomized as Randomized
import           HGeometry.Point
import           HGeometry.Vector
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


--------------------------------------------------------------------------------

type R = RealNumber 5

spec :: Spec
spec = describe "Lower Envelope tests" $ do
         prop "same as brute force" $
           \seed (planes :: NonEmpty (Plane R)) ->
             Randomized.computeVertexForm (mkStdGen seed) planes
             ===
             BruteForce.computeVertexForm planes
