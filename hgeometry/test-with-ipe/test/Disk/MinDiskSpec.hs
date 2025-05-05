{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Disk.MinDiskSpec
  (spec
  ) where

import           Control.Lens
import           Control.Monad (when)
import           Control.Monad.Random.Strict (evalRand)
import           Data.Foldable
import qualified Data.Map.Monoidal as Map
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import           Golden
import           HGeometry
import           HGeometry.Ball
import           HGeometry.Disk
import           HGeometry.Disk.Smallest
import qualified HGeometry.Disk.Smallest.Naive as Naive
import           HGeometry.Ext
import           HGeometry.Instances ()
import           HGeometry.Number.Real.Rational
import           Ipe
import           System.OsPath
import           System.Random (mkStdGen)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.Util

--------------------------------------------------------------------------------

type R = RealNumber 5


spec :: Spec
spec = describe "Disk.MinDisk" $ do
         testCases [osp|test-with-ipe/Disk/minDisk.ipe|]
         prop "smallest enclosing disk same as naive" $
           \gen (pts :: NESet.NESet (Point 2 R)) ->
             NESet.size pts >= 2 ==>
               smallestEnclosingDisk (mkStdGen gen) pts `sameDisk`
               Naive.smallestEnclosingDisk pts

instance HasDefaultFromIpe (IpeSymbol r) where
  type DefaultFromIpe (IpeSymbol r) = IpeSymbol
  defaultFromIpe = _withAttrs _IpeUse id

testCases    :: OsPath -> Spec
testCases fp = describe ("tests from" <> show fp) $
                 do pts <- runIO $ readAllFrom =<< getDataFileName fp
                    let sets      = groupByColor pts
                        testCases = (\pts' -> TestCase (Set.map (view symbolPoint) pts')
                                                       (solutionOf pts')
                                    ) <$> sets
                    for_ testCases toSpec

groupByColor :: [IpeSymbol R :+ _] -> Map.MonoidalMap (Maybe (IpeColor R)) (Set.Set (IpeSymbol R))
groupByColor = foldMap (\(p :+ ats) ->
                           Map.singleton (lookupAttr SStroke ats) (Set.singleton p))



solutionOf pts = case mapMaybe f (toList pts) of
                   [p,q]   -> Just . DiametralDisk $ DiametralPoints p q
                   [p,q,r] -> DiskByPoints <$> diskFromPoints p q r
                   _       -> Nothing
  where
    f s | s^.symbolName == "mark/cross(sx)" = Just (s^.symbolPoint)
        | otherwise                         = Nothing

data TestCase = TestCase { _pointSet :: Set.Set (Point 2 R)
                         , _solution :: Maybe (DiskByPoints (Point 2 R))
                         }
              deriving (Show,Eq)


toSpec                          :: TestCase -> Spec
toSpec (TestCase pts mSolution) =
    describe ("testing point set with solution " ++ show mSolution) $ do
      pure ()

      case mSolution of
        Nothing       -> pure ()
        Just solution -> prop "naive correct" $
                           Naive.smallestEnclosingDisk pts `sameDisk` solution


      -- it "comparing with naive solution" $
        -- (flip evalRand (mkStdGen 2123) $
        --   view enclosingDisk <$> RIC.smallestEnclosingDisk pts)
        -- `shouldBe`
        -- ((Naive.smallestEnclosingDisk pts)^.enclosingDisk)
      -- when (isJust sol) $
      --   it "manal solution" $
      --     (flip evalRand (mkStdGen 5) $
      --       view enclosingDisk <$> RIC.smallestEnclosingDisk pts)
      --     `shouldBe`
      --     (diskOf $ fromJust sol)


-- diskOf               :: TwoOrThree (Point 2 R) -> Disk () r

sameDisk             :: DiskByPoints (Point 2 R) -> DiskByPoints (Point 2 R) -> Property
sameDisk diskA diskB = diskA^.center === diskB^.center .&&.
                       diskA^.squaredRadius === diskB^.squaredRadius
