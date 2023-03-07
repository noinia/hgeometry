{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module HGeometry.MatrixSpec
  ( spec
  ) where

import           Control.Lens hiding (elements)
-- import           Data.Ratio
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

type R = RealNumber 5 --
-- type R = Rational

newtype NonZeroM m = NonZeroM m deriving (Show,Eq)

instance ( Arbitrary m
         , HasElements m m, Num (NumType m), Eq (NumType m)
         ) => Arbitrary (NonZeroM m) where
  arbitrary = NonZeroM <$> arbitrary `suchThat` (anyOf elements (/= 0))

-- newtype Invertible n r = Invertible (Matrix n n r) deriving (Show,Eq)

-- instance ( Arbitrary r
--          , HasDeterminant n
--          ) => Arbitrary (Invertible m r) where
--   arbitrary = Invertible <$> arbitrary `suchThat` ((/= 0) . det)



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



toLinear   :: ( Matrix_ matrix n m r
              , Has_ Vector_ m r
              , HasComponents (Vector n (Vector m r)) (Vector n (Vector m r))
              ) => matrix -> [[r]]
toLinear m = (^..components) <$> m^..rows.components

toLinear2 m = toV2 $ toV2 <$> toLinear m
toLinear3 m = toV3 $ toV3 <$> toLinear m
toLinear4 m = toV4 $ toV4 <$> toLinear m

toV2 [x,y] = V2.V2 x y
toV3 [x,y,z] = V3.V3 x y z
toV4 [x,y,z,w] = V4.V4 x y z w


-- testM :: Matrix 3 3 R
-- testM = Matrix (Vector3 (Vector3 (-0.21993) (-0.69023) 0.8687) (Vector3 0.62704 (-0.54744) (-0.71737)) (Vector3 (-0.27474) 0.49027 (-0.31481)))


-- -- testM :: Matrix 3 3 R
-- -- testM = Matrix (Vector3 (Vector3 ((-4504613468457) % 5617527700211) (253086709880 % 1794090340679) (151287166868 % 3214861165871)) (Vector3 ((-157572829658) % 1163246314995) ((-2028056022401) % 8758172408534) ((-795115313114) % 4008900273389)) (Vector3 ((-517550976002) % 3451424212337) ((-2160176384643) % 2513722922126) (1343766338877 % 3191970087605)))

-- diff = ( toLinear3 (inverseMatrix testM)
--        , Linear.Matrix.inv33 (toLinear3 testM)
--        )


-- -- V3 (V3 (-2.17829~) 1.77294~ 3.95123~)
-- --    (V3 2.13548~ 1.10512~ (-1.03036~))
-- --    (V3 0.83048~ (-1.14079~) (-1.97128~))

-- -- V3 (V3 (-2.08665~) (-0.83064~) (-3.86520~))
-- --    (V3 (-1.57078~) (-1.22602~) (-1.54072~))
-- --    (V3 (-0.62521~) (-1.18443~) (-2.20274~)))
