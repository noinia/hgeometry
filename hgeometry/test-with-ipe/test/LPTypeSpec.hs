{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
module LPTypeSpec
  ( spec
  , render
  , bug2
  ) where

import           Control.Lens
import           Data.Foldable (toList)
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust, fromMaybe, isJust)
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Vector as V
import qualified Data.Vector.Generic as Vector
import           Data.Word
import           Debug.Trace
import           GHC.TypeLits
import           Golden
import           HGeometry.Ball
import           HGeometry.Combinatorial.Util
import           HGeometry.Disk
import qualified HGeometry.Disk.Smallest.Naive as Naive
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane
import           HGeometry.Instances ()
import           HGeometry.Intersection
import           HGeometry.LPType
import           HGeometry.LPType.LinearProgramming
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Number.Real.Rational
import           HGeometry.Permutation.Shuffle
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Unbounded
import           HGeometry.Vector hiding (basis)
import           Ipe
import           Ipe.Color
import           Prelude hiding (filter)
import           System.OsPath
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck ( (===), (==>), property,Discard(..), counterexample
                                 , Arbitrary(..), oneof, suchThat
                                 , withDiscardRatio, withMaxSuccess
                                 , Property, (.&&.)
                                 )
import           Test.QuickCheck.Instances ()
import           VectorBuilder.Builder (foldable)
import           VectorBuilder.Vector (build)
import           Witherable
--------------------------------------------------------------------------------

type R = RealNumber 5

lpRecomputeBasis = extendBasis linearProgrammingMinY
lpInitialBasis   = initialBasis linearProgrammingMinY

-- data Basis halfSpace d where
--   Basis :: (i <= d + 1) => Vector i halfSpace

instance (Arbitrary r, Eq r, Fractional r) => Arbitrary (Basis2D r (HalfSpaceF (LineEQ r))) where
  arbitrary = oneof [ pure EmptyBasis
                    , do y <- arbitrary
                         pure $ Basis1 y (HalfSpace Positive (LineEQ 0 y))
                    , do l  <- arbitrary
                         l' <- arbitrary `suchThat` (\(LineEQ a _) ->
                                                       signum a /= signum (l^.slope))
                         case l `intersect` l' of
                           Just (Line_x_Line_Point p) ->
                             pure $ Basis2 p (HalfSpace Positive l) (HalfSpace Positive l')
                           _ -> arbitrary -- retry
-- don't generate infeasible basises for now
                    ]








-- do the skew transform


-- linearProgramming              :: Vector d r -> LPType (Top r) set halfSpace
-- linearProgramming v constraints = LPType {
--     costFunction hs        = undefined
--   , elements               = constraints
--   , combinatorialDimension = 2 -- TODO:
--   , basisFor          hs  = take d hs
--   , initialBasis      hs  = take d hs
--   }



--------------------------------------------------------------------------------



-- | Runs the subexponential time and returns both the cost and the optimal basis.
subExpWith                  :: ( Foldable set, Foldable basis
                               , Ord t, Eq a, RandomGen gen
                               )
                           => gen
                               -- ^ random generator
                          -> LPType t basis set a
                          -- ^ an LP-type problem
                          -> set a
                          -> (t, basis a)
subExpWith gen problem hs = withOpt (costFunction problem) $ subExp gen problem hs

-- | Also return the value of the optimal solution
withOpt         :: (b -> t) -> b -> (t, b)
withOpt v basis = (v basis, basis)



--------------------------------------------------------------------------------

spec :: Spec
spec = describe "LPType Spec" $ do
         describe "1D LP" $ do
           it "1D feasible example" $ do
             lp1D (NonEmpty.fromList [ HalfSpace Positive (Point1 0 :: Point 1 R)
                                     , HalfSpace Negative (Point1 10)
                                     , HalfSpace Positive (Point1 3)
                                     , HalfSpace Negative (Point1 5)
                                     , HalfSpace Positive (Point1 2)
                                     ]) `shouldBe` Feasible1 (HalfSpace Positive (Point1 3))

           it "1D infeasible example" $ do
             lp1D (NonEmpty.fromList [ HalfSpace Positive (Point1 0 :: Point 1 R)
                                     , HalfSpace Negative (Point1 10)
                                     , HalfSpace Positive (Point1 3)
                                     , HalfSpace Negative (Point1 5)
                                     , HalfSpace Negative (Point1 2)
                                     ]) `shouldBe` InFeasible1 (HalfSpace Positive (Point1 3))
                                                               (HalfSpace Negative (Point1 2))

         it "initialBasis" $ do
           case exampleLP of
             (h1:h2:_) -> lpInitialBasis exampleLP `shouldBe` (Basis2 (Point2 2.5 2.5) h1 h2)
             _         -> fail "error"

         it "lp extend basis" $ do
           case exampleLP of
             (_:_:h3:_) -> let basis' = lpInitialBasis exampleLP
                           in lpRecomputeBasis h3 basis' `shouldBe` Nothing
             _          -> fail "error"

         it "subExp" $
           fst (subExpWith (mkStdGen 42) linearProgrammingMinY exampleLP) `shouldBe` (Val 2.5)

         it "manual infeasible" $ do
           let h1 = HalfSpace Positive (LineEQ 0.29166 38.66671) :: HalfSpaceF (LineEQ R)
               h2 = HalfSpace Positive (LineEQ (-0.6) 195.2)
               h3 = HalfSpace Negative (LineEQ 0 80)
               ib = lpInitialBasis [h1,h2]
           lpRecomputeBasis h3 ib `shouldBe` Just (Infeasible (Vector3 h3 h1 h2))

         prop "feasible means feasible" $ withMaxSuccess 50 $ withDiscardRatio 1000 $
           \gen (halfPlanes :: [HalfPlane R]) ->
             case subExp (mkStdGen gen) linearProgrammingMinY halfPlanes of
               Basis2 v _ _ -> property $ all (v `intersects`) halfPlanes
               _            -> property Discard

         -- prop "feasible means interseciton point" $
         --   \gen (halfPlanes :: HalfPlane R) ->
         --     case subExp (mkStdGen gen) (linearProgrammingMinY halfPlanes) of
         --       (_,Basis2 v _ _) -> property $ isAVertex v
         --       _                -> property Discard


         modifyMaxSize (const 1000) $ prop "clarkson2 same as subExp" $
           \gen (halfPlanes :: V.Vector (HalfPlane R)) ->
             let problem        = linearProgrammingMinY
                 (resSubExp, _) = subExpWith (mkStdGen gen) problem halfPlanes
                 (res,_)        = withOpt (costFunction problem)
                                $ clarkson2 (mkStdGen gen) problem halfPlanes
             in res `shouldBe` resSubExp

         modifyMaxSize (const 1000) $ prop "clarkson2 same as subExp (positives)" $
           \gen (lines' :: [LineEQ R]) ->
             let problem        = linearProgrammingMinY
                 (resSubExp, _) = subExpWith (mkStdGen gen)    problem halfPlanes
                 (res,_)        = withOpt (costFunction problem)
                                $ clarkson2 (mkStdGen gen) problem halfPlanes
                 halfPlanes     = V.fromList [ HalfSpace Positive l | l <- lines' ]
             in res `shouldBe` resSubExp

         modifyMaxSize (const 1000) $ prop "clarkson same as clarkson2 (positives)" $
           \gen (lines' :: [LineEQ R]) ->
             let problem    = linearProgrammingMinY
                 (res2, _)  = withOpt (costFunction problem)
                            $ clarkson2 (mkStdGen gen) problem halfPlanes
                 (res,_)    = clarkson  (mkStdGen gen) problem halfPlanes
                 halfPlanes = V.fromList [ HalfSpace Positive l | l <- lines' ]
             in res `shouldBe` res2

         prop "brute force test" $
           \gen (halfPlanes :: [HalfPlane R]) ->
             let res = subExpWith (mkStdGen gen) linearProgrammingMinY halfPlanes
             in counterexample (show res) $
             case bruteForceSolutions halfPlanes of
               [] -> case res of
                       (_, EmptyBasis)   -> True -- for now
                       (_, Basis1 _ h)   -> h^.boundingHyperPlane.slope == 0
                       (_, Infeasible _) -> True
                       (_, Basis2 _ _ _) -> False
               vs -> case res of
                       (_, Basis2 v _ _) -> v `elem` vs
                       (_, EmptyBasis)   -> True -- could be true
                       (_, Basis1 _ _)   -> True -- could be true
                       _                 -> False

         testIpe [osp|LPType/lpType_linearProgramming.ipe|]
         testIpe [osp|LPType/linearProgramming1.ipe|]
         testIpe [osp|LPType/linearProgramming2.ipe|]
         testIpe [osp|LPType/linearProgramming2_simpl.ipe|]
         testIpe [osp|LPType/infeasible_lp.ipe|]

         bug
         bug2




         -- it "extract" $
         --   extract 3 (NonEmpty.fromList "foobarenzo") `shouldBe` ('b',"fooarenzo")
         -- prop "extract1" $
         --   extract1 (mkStdGen 42) (NonEmpty.fromList "foobarenzo") "foo" ===  ('n',"foobarezo")

-- sameDisk             :: DiskByPoints (Point 2 R) -> DiskByPoints (Point 2 R) -> Property
-- sameDisk diskA diskB = diskA^.center === diskB^.center .&&.
--                        diskA^.squaredRadius === diskB^.squaredRadius


instance (Arbitrary a, Ord a) => Arbitrary (NESet.NESet a) where
  arbitrary = NESet.fromList <$> arbitrary


bruteForceSolutions    :: ( Ord r, Fractional r
                          , HyperPlane_ line 2 r
                          , IsIntersectableWith line line
                          , Intersection line line ~ Maybe (LineLineIntersectionG r line)
                          , Dimension line ~ 2, NumType line ~ r
                          , Foldable set, Functor set
                          )
                       => set (HalfSpaceF line) -> [Point 2 r]
bruteForceSolutions hs = mapMaybe intersectionPoint $ uniquePairs (view boundingHyperPlane <$> hs)
  where
    intersectionPoint (Two l l') = case l `intersect` l' of
      Just (Line_x_Line_Point p) | all (p `intersects`) hs -> Just p
      _                                                    -> Nothing

bug = describe "bug" $ do
        let cs = [HalfSpace Positive (LineEQ 3 (-1.5))
                 ,HalfSpace Positive (LineEQ (-5.5) 2.6)
                 ,HalfSpace Positive (LineEQ 5 4)
                 ,HalfSpace Negative (LineEQ (-3) 2)
                 ]
            gen = 1
            res = subExpWith (mkStdGen gen) linearProgrammingMinY cs

            ib = lpInitialBasis cs

        it "initial basis" $
          ib `shouldSatisfy` (\case
             Basis2 _ a b -> a == cs !! 0 && b == cs !! 1
             _            -> False)

        -- prop "extend 2" $
        --   lpRecomputeBasis (cs !! 2) ib `shouldBe` Nothing

        -- prop "extend 23" $
        --   lpRecomputeBasis (cs !! 3) (lpRecomputeBasis (cs !! 2) ib) `shouldSatisfy` (\case
        --     (Infeasible _) -> True
        --     _              -> False)

        -- prop "extend 3" $
        --   lpRecomputeBasis (cs !! 3) ib `shouldBe` ans


        prop "thebug" $ counterexample (show res) $
          case res of
            (_, Basis2 v _ _) -> all (v `intersects`) cs
            (_, Infeasible _) -> True
            _                 -> False
        it "bruteforce" $
          bruteForceSolutions cs `shouldBe` []

        it "bruteforce 234" $
          bruteForceSolutions (drop 1 cs) `shouldBe` []

bug2 = describe "bug2" $ do
        let cs :: [HalfPlane R]
            cs = [HalfSpace Positive (LineEQ 13.33333 (-17.5))        -- :+ red
                 ,HalfSpace Positive (LineEQ (-16.57143) (-11.26316)) -- :+ purple
                 ,HalfSpace Positive (LineEQ (-3.05883) 17.5)         -- :+ blue
                 ,HalfSpace Positive (LineEQ 5 (-11.2))               -- :+ green
                 ]
            gen = 1
            res = subExpWith (mkStdGen gen) linearProgrammingMinY cs

            -- ib = lpInitialBasis cs

        -- it "initial basis" $
        --   ib `shouldSatisfy` (\case
        --      Basis2 _ a b -> a == cs !! 0 && b == cs !! 1
        --      _            -> False)

        -- it "extend 0" $
        --   let ib = Basis2 (Point2 3.5131 6.60655) (cs !! 2) (cs !! 3) in
        --   lpRecomputeBasis (cs !! 0) ib `shouldSatisfy` (\case
        --     Just (Basis2 _ a b,[_]) -> a == cs !! 0 && b == cs !! 2
        --     _                       -> False
        --                                                 )

        -- prop "recompute basis -> removed correct" $
        --   \(h :: HalfPlane R) basis ->
        --     case lpRecomputeBasis h basis of
        --       Just (newBasis,removed) -> Set.fromList (h : toList basis)
        --                                  ===
        --                                  Set.fromList (toList newBasis <> removed)
        --       Nothing -> property Discard

        prop "thebug" $ counterexample (show res) $
          case res of
            (_, Basis2 v _ _) -> all (v `intersects`) cs
            (_, Infeasible _) -> True
            _                 -> False
        it "bruteforce" $
          bruteForceSolutions cs `shouldSatisfy` ((== 2) . length)

        -- it "bruteforce 234" $
        --   bruteForceSolutions (drop 1 cs) `shouldBe` []



exampleLP :: [HalfPlane R]
exampleLP = [ HalfSpace Positive (LineEQ 1    0)
            , HalfSpace Positive (LineEQ (-1) 5)
            , HalfSpace Positive (LineEQ 2    (-3))
            ]



--------------------------------------------------------------------------------






 -- [HalfSpace Positive (Arg (89.86891~,175.55038~) (Point2 175.55038~ 89.86891~,HalfSpace Positive (LineEQ 0.29166~ 38.66671~)))

 -- ,HalfSpace Positive (Arg (22.64946~,287.58279~) (Point2 287.58279~ 22.64946~,HalfSpace Positive (LineEQ (-0.06897~) 42.48275~)))]

testIpe      :: OsPath -> Spec
testIpe inFp = describe ("linear programming on file " <> show inFp) $ do
          (halfPlanes', solution) <- runIO $ loadInputs inFp
          let halfPlanes :: V.Vector _
              halfPlanes = fromFoldable halfPlanes'

          -- let (h1:|h2:h3:_) = (view core <$> halfPlanes)
          -- runIO $ mapM_ print (view core <$> halfPlanes)

          -- it "collect" $
          --   (collect $ mapMaybe (constructHalfSpaceOn h2) [h1,h3]) `shouldBe`
          --   Right []

          -- it "extend initialBasis" $ do
          --   let basis = lpInitialBasis [h1,h3]
          --   lpRecomputeBasis h2 basis `shouldBe` (Basis2 (Point2 89 175) h2 h1)

-- HalfSpace Positive (LineEQ 0.29166~ 38.66671~)
-- HalfSpace Positive (LineEQ (-0.06897~) 42.48275~)


-- HalfSpace Positive (LineEQ (-0.6) 195.2)

          prop "subExp correct, " $
            let (sol,basis') = subExpWith (mkStdGen 42) linearProgrammingMinY
                                      (view core <$> halfPlanes)
            in counterexample (show sol) $ Set.fromList (toList basis') === solution

          prop "clarkson correct, " $
            let (sol,basis') = clarkson (mkStdGen 42)  linearProgrammingMinY
                                        (view core <$> halfPlanes)
            in counterexample (show sol) $ Set.fromList (toList basis') === solution


loadInputs      :: OsPath -> IO ( NonEmpty (HalfPlane R :+ _)
                                , Set.Set (HalfPlane R)
                                )
loadInputs inFp = do
        inFp'      <- getDataFileName ([osp|test-with-ipe/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (rays :: NonEmpty (HalfLine (Point 2 R) :+ _)) = NonEmpty.fromList $ readAll page
        -- take the left halfplane of every halfline
            halfPlanes = over core (toLineEQ . leftHalfPlane . asOrientedLine) <$> rays
            solution   = foldMap (\(h :+ ats) ->
                                    if lookupAttr SStroke ats == Just red
                                    then Set.singleton h else mempty
                                 ) halfPlanes
        pure (halfPlanes, solution)

toLineEQ                :: HalfSpaceF (LinePV 2 R) -> HalfPlane R
toLineEQ (HalfSpace _ l)
    | l^.direction.xComponent > 0 = HalfSpace Positive l' -- sign was already positive
    | otherwise                   = HalfSpace Negative l'
  where
    l' = fromJust . toLinearFunction $ l


    -- goldenWith [osp|data/test-with-ipe/golden/|]
    --                 (ipeContentGolden { name = theName  })
    --                 ( myIpeTest halfPlanes)


--------------------------------------------------------------------------------

render = writeIpeFile [osp|/tmp/out.ipe|] . addStyleSheet opacitiesStyle . singlePageFromContent $
           map (iO . ipeConstraint) cs
  where
    cs :: [HalfPlane R :+ IpeColor R]
    cs = [HalfSpace Positive (LineEQ 13.33333 (-17.5))        :+ red
         ,HalfSpace Positive (LineEQ (-16.57143) (-11.26316)) :+ purple
         ,HalfSpace Positive (LineEQ (-3.05883) 17.5)         :+ blue
         ,HalfSpace Positive (LineEQ 5 (-11.2))               :+ green
         ]

ipeConstraint (h :+ c) = ipeHalfPlane c $ h&boundingHyperPlaneLens %~ fromLineEQ
                                           &halfSpaceSign %~ \case
                                                                Positive -> Negative
                                                                Negative -> Positive
