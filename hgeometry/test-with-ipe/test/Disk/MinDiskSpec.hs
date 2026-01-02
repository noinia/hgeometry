{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Disk.MinDiskSpec
  (spec
  ) where

import           Control.Lens
import           Data.Foldable
import qualified Data.Map.Monoidal as Map
import           Data.Maybe
import qualified Data.Set.NonEmpty as Set
import           Golden
import           HGeometry
import           HGeometry.Disk
import           HGeometry.Disk.Smallest
import qualified HGeometry.Disk.Smallest.Naive as Naive
import           HGeometry.Ext
import           HGeometry.Instances ()
import           R
import           Ipe
import           System.OsPath
import           System.Random (mkStdGen)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Disk.MinDisk" $ do
         testCases [osp|test-with-ipe/Disk/minDisk.ipe|]
         prop "smallest enclosing disk same as naive" $
           \gen (pts :: Set.NESet (Point 2 R)) ->
             Set.size pts >= 2 ==>
               smallestEnclosingDisk (mkStdGen gen) pts `sameDisk`
               Naive.smallestEnclosingDisk pts

instance HasDefaultFromIpe (IpeSymbol r) where
  type DefaultFromIpe (IpeSymbol r) = IpeSymbol
  defaultFromIpe = _withAttrs _IpeUse id

testCases    :: OsPath -> Spec
testCases fp = describe ("tests from" <> show fp) $
                 do pts <- runIO $ readAllFrom =<< getDataFileName fp
                    let sets' = groupByColor pts
                        tests = (\pts' -> TestCase (Set.map (view symbolPoint) pts')
                                          (solutionOf pts')
                                ) <$> sets'
                    for_ tests toSpec

groupByColor :: [IpeSymbol R :+ _] -> Map.MonoidalMap (Maybe (IpeColor R))
                                                      (Set.NESet (IpeSymbol R))
groupByColor = foldMap (\(p :+ ats) ->
                           Map.singleton (lookupAttr SStroke ats) (Set.singleton p))



solutionOf pts = case mapMaybe f (toList pts) of
                   [p,q]   -> Just . DiametralDisk $ DiametralPoints p q
                   [p,q,r] -> DiskByPoints <$> diskFromPoints p q r
                   _       -> Nothing
  where
    f s | s^.symbolName == "mark/cross(sx)" = Just (s^.symbolPoint)
        | otherwise                         = Nothing

data TestCase = TestCase { _pointSet :: Set.NESet (Point 2 R)
                         , _solution :: Maybe (DiskByPoints (Point 2 R))
                         }
              deriving (Show,Eq)


toSpec                          :: TestCase -> Spec
toSpec (TestCase pts mSolution) =
    describe ("testing point set with solution " ++ show mSolution) $ do
      let gen = 20 -- pick some arbitrary seed

      case mSolution of
        Nothing       -> pure ()
        Just solution -> do prop "naive correct" $
                              Naive.smallestEnclosingDisk pts `sameDisk` solution

      prop "comparing with naive solution" $
        smallestEnclosingDisk (mkStdGen gen) pts `sameDisk` Naive.smallestEnclosingDisk pts
      -- since the random sol is the same as the brute force, it is thus also the same
      -- as the manual solution :).

-- | Make sure that the two disks are the same; (by constructing the unique disk defined
-- by its center and radius).
sameDisk             :: DiskByPoints (Point 2 R) -> DiskByPoints (Point 2 R) -> Property
sameDisk diskA diskB = diskA^.center === diskB^.center .&&.
                       diskA^.squaredRadius === diskB^.squaredRadius
