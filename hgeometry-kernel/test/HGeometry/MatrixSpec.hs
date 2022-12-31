{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module HGeometry.MatrixSpec
  ( spec
  ) where

import           Control.Lens hiding (elements)
import           HGeometry.Kernel.Instances ()
import           HGeometry.Matrix
import           HGeometry.Number.Real.Rational
import           HGeometry.Properties (NumType)
import           HGeometry.Vector
import qualified Linear.Matrix
import qualified Linear.V2 as V2
import qualified Linear.V3 as V3
import qualified Linear.V4 as V4
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck (Arbitrary(..), suchThat)
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------

type R = RealNumber 5

newtype NonZeroM m = NonZeroM m deriving (Show,Eq)

instance ( Arbitrary m
         , HasElements m m, Num (NumType m), Eq (NumType m)
         ) => Arbitrary (NonZeroM m) where
  arbitrary = NonZeroM <$> arbitrary `suchThat` (anyOf elements (/= 0))

spec = describe "Matrix tests" $ do
         describe "determinant the same as linear" $ do
           prop "2d" $
             \(NonZeroM (m :: Matrix 2 2 R)) -> det m == Linear.Matrix.det22 (toLinear2 m)
           prop "3d" $
             \(NonZeroM (m :: Matrix 3 3 R)) -> det m == Linear.Matrix.det33 (toLinear3 m)
           prop "4d" $
             \(NonZeroM (m :: Matrix 4 4 R)) -> det m == Linear.Matrix.det44 (toLinear4 m)
         describe "inverse the same as linear" $ do
           prop "2d" $
             \(NonZeroM (m :: Matrix 2 2 R)) ->
               toLinear2 (inverseMatrix m) == Linear.Matrix.inv22 (toLinear2 m)
           prop "3d" $
             \(NonZeroM (m :: Matrix 3 3 R)) ->
               toLinear3 (inverseMatrix m) == Linear.Matrix.inv33 (toLinear3 m)
           -- prop "4" $
           --   \(NonZeroM (m :: Matrix 4 4 R)) ->
           --     toLinear4 (inverseMatrix m) == Linear.Matrix.inv44 (toLinear4 m)



toLinear   :: (Matrix_ matrix n m r, OptVector_ m r) => matrix -> [[r]]
toLinear m = (^..components) <$> m^..rows

toLinear2 m = toV2 $ toV2 <$> toLinear m
toLinear3 m = toV3 $ toV3 <$> toLinear m
toLinear4 m = toV4 $ toV4 <$> toLinear m

toV2 [x,y] = V2.V2 x y
toV3 [x,y,z] = V3.V3 x y z
toV4 [x,y,z,w] = V4.V4 x y z w
