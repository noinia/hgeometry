{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module HGeometry.HyperPlaneSpec where

import Control.Lens hiding (unsnoc)
import Data.Maybe (isJust)
import GHC.TypeNats
import HGeometry.HalfSpace
import HGeometry.HyperPlane
import HGeometry.HyperPlane.NonVertical
import HGeometry.Intersection
import HGeometry.Kernel.Instances ()
import HGeometry.Line
import HGeometry.Number.Real.Rational
import HGeometry.Point
import HGeometry.Vector
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ((===), (==>))

--------------------------------------------------------------------------------

type R = RealNumber 10

myHyp :: NonVerticalHyperPlane 2 R
myHyp = NonVerticalHyperPlane $ Vector2 1 2
-- should be the same as myLine

myHyp2 :: NonVerticalHyperPlane 2 R
myHyp2 = hyperPlaneFromEquation $ Vector3 2 1 (-1)
-- should be the same as myLine



myLine :: LineEQ R
myLine = LineEQ 1 2

myPoints :: [(Point 2 R, Bool)]
myPoints = [ (Point2 10 10, False)
           , (Point2 10 1000, True)
           , (Point2 0 20, True)
           , (Point2 0 2, True)
           ]

asHyp :: ( NonVerticalHyperPlane_ hyperPlane d r
         , MkHyperPlaneConstraints d r, Num r
         ) => hyperPlane -> HyperPlane d r
asHyp = hyperPlaneFromEquation . hyperPlaneEquation

-- | Test if the hyperplane is non-vertical
isNonVertical :: ( HyperPlane_ hyperPlane d r, Fractional r, Eq r, 1 <= d)
              => hyperPlane -> Bool
isNonVertical = isJust . asNonVerticalHyperPlane

-- | Returns GT iff the point lies above the hyperplane
onSideTestNonVertical     :: forall point nonVerticalHyperPlane d r.
                             ( NonVerticalHyperPlane_ nonVerticalHyperPlane d r
                             , Point_ point d r
                             , Ord r, Fractional r
                             , Has_ Additive_ (d-1) r
                             , d-1 <= d, 1 <= d
                             ) => point -> nonVerticalHyperPlane -> Ordering
onSideTestNonVertical p h = let (v, pd) = unsnoc (p^.vector) :: (Vector (d-1) r, r)
                            in pd `compare` evalAt (Point v) h

spec :: Spec
spec = describe "HyperPlane Tests" $ do
         it "show myHyp3" $
           show myHyp3 `shouldBe` "HyperPlane [2,1,-1]"
         it "read myHyp3" $
           read "HyperPlane [2,1,-1]" `shouldBe` myHyp3
         showReadTests
         it "same hyperplane" $ myHyp `shouldBe` myHyp2
         -- it "in halfspace" $ do
         --   mapM_ (\(q,ans) -> (q `intersects` myHalfspace) `shouldBe` ans) myPoints

         it "vertical hyperPlane through y-axis" $
           hyperPlaneThrough (Vector2 (Point2 0 (-1)) (origin :: Point 2 Int))
           `shouldBe` HyperPlane2 0 (-1) 0

         it "as nonvertical 1" $
           asNonVerticalHyperPlane (HyperPlane2 0 0 1) `shouldBe`
             Just (NonVerticalHyperPlane (Vector2 0 0))
         it "as nonvertical 2" $
           asNonVerticalHyperPlane (HyperPlane2 (-1) (-1) (-1)) `shouldBe`
             Just (NonVerticalHyperPlane (Vector2 (-1) (-1)))
         it "as nonvertical 3" $
           asNonVerticalHyperPlane (HyperPlane2 0 (-1) 0) `shouldBe`
             Nothing

--------------------------------------------------------------------------------
         prop "normal vector ok" $
           \(p :: Point 3 R) (n :: Vector 3 R) ->
             (quadrance n > 0) ==>
               normalVector (fromPointAndNormal p n :: HyperPlane 3 R) `isScalarMultipleOf` n

         prop "normal vector ok for non-vertical" $
           \(p :: Point 3 R) (n :: Vector 3 R) ->
             (quadrance n > 0 && n^.zComponent /= 0) ==>
               normalVector (fromPointAndNormal p n :: NonVerticalHyperPlane 3 R) `isScalarMultipleOf` n

-- Note that the normal vector we start with and the one we get back from 'normalVector'
--do not have to be identical: Take an arbitrary hyperplane, and consider its two normal
--vectors , and pick the negative one (i.e. pointing into the negative halfspace.; then
--build the plane using this negative normal. We then want the 'normalVector' function to
--return the positive normal vector instead.
--------------------------------------------------------------------------------


         prop "onside test non-vertical hyperplane2 (i.e. lines)" $
           \(h :: HyperPlane 2 R) (q :: Point 2 R) ->
             isNonVertical h ==>
               q `onSideTest` h === (case asNonVerticalHyperPlane h of
                                        Just h' -> onSideTestNonVertical q h'
                                        _       -> error "impossible"
                                    )
         prop "onside test vertical lines" $
           \(x :: R) (q :: Point 2 R) ->
             let h = HyperPlane2 x (-1) 0 -- vertical line through x
             in q `onSideTest` h === (q^.xCoord) `compare` x


         prop "onside test non-vertical hyperplane3 (i.e. planes)" $
           \(h :: HyperPlane 3 R) (q :: Point 3 R) ->
             isNonVertical h ==>
               q `onSideTest` h === (case asNonVerticalHyperPlane h of
                                        Just h' -> onSideTestNonVertical q h'
                                        _       -> error "impossible"
                                    )
         -- prop "onside test vertical lines" $
         --   \(x :: R) (q :: Point 3 R) ->
         --     let h = HyperPlane2 x (-1) 0 -- vertical line through x
         --     in q `onSideTest` h === (q^.xCoord) `compare` x

         prop "onside test NonVertical Hyperplanes i.e. lines) consitent " $
           \(h :: NonVerticalHyperPlane 2 R) (q :: Point 2 R) ->
             q `onSideTest` h === onSideTestNonVertical q h

         it "onside test NonVertical Hyperplanes i.e. lines) manual " $ do
           (Point2 3 10) `onSideTestNonVertical` (LineEQ 1 1) `shouldBe` GT
           (Point2 3 10) `onSideTestNonVertical` (LineEQ 2 1) `shouldBe` GT
           (Point2 3 3) `onSideTestNonVertical` (LineEQ 2 1)  `shouldBe` LT
           (Point2 3 7) `onSideTestNonVertical` (LineEQ 2 1)  `shouldBe` EQ
           (Point2 0 1) `onSideTestNonVertical` (LineEQ 2 1)  `shouldBe` EQ
           (Point2 0 0) `onSideTestNonVertical` (LineEQ 2 1)  `shouldBe` LT
           (Point2 0 4) `onSideTestNonVertical` (LineEQ 2 1)  `shouldBe` GT


         it "on side of vertical line / hyperplane" $
           (Point2 0 1 `onSideTest` HyperPlane2 3 (-1) 0)
           `shouldBe` LT
         it "on side of non-vertical line / hyperplane" $
           (Point2 0 110 `onSideTest` HyperPlane2 3 (-1) (-1))
           `shouldBe` GT
         it "on side of non-vertical line / hyperplane 2" $
           (Point2 0 (-1) `onSideTest` HyperPlane2 (-1) (-1) (-1))
           `shouldBe` EQ
         it "on side of non-vertical line / hyperplane 3" $
           (Point2 0 1 `onSideTest` HyperPlane2 (-1) (-1) (-1))
           `shouldBe` GT
         it "on side of non-vertical line / hyperplane 4" $
           (Point2 0 0 `onSideTest` HyperPlane2 (-1) (-1) (-1))
           `shouldBe` GT
         it "on side of non-vertical line / hyperplane 5" $
           (Point2 0 0 `onSideTest` HyperPlane2 (-1) (-1) (-1))
           `shouldBe` GT
         prop "pointOn produces a point on the hyperplane (2d)" $
           \(h :: HyperPlane 2 R) -> onHyperPlane (pointOn h) h
         prop "pointOn produces a point on the hyperplane (3d)" $
           \(h :: HyperPlane 3 R) -> onHyperPlane (pointOn h) h

         prop "intersects nonvertical conistent" $
           \(l :: LineEQ R) (m :: LineEQ R) ->
             (l `intersects` m) `shouldBe` (asHyp l `intersects` asHyp m)

         prop "fromPointAnNormal and sideTest consistent for HyperPlane in R^3" $
           \(p :: Point 3 R) n ->
             allOf components (>0) n ==>
             ((p .+^ n) `onSideTest` (fromPointAndNormal p n :: HyperPlane 3 R))
             `shouldBe` GT

         prop "fromPointAnNormal and intersects halfSpace consistent in R^3" $
           \(p :: Point 3 R) n ->
             allOf components (>0) n ==>
             ((p .+^ n) `intersects` (HalfSpace Positive (fromPointAndNormal p n)
                                      :: HalfSpace 3 R
                                     ))
             `shouldBe` True

         prop "fromPointAnNormal and sideTest consistent for HyperPlane " $
           \(p :: Point 2 R) n ->
             allOf components (>0) n ==>
             ((p .+^ n) `onSideTest` (fromPointAndNormal p n :: HyperPlane 2 R))
             `shouldBe` GT
         prop "fromPointAnNormal and sideTest consistent for NonVerticalHyperPlane " $
           \(p :: Point 2 R) n ->
             allOf components (>0) n ==>
             ((p .+^ n) `onSideTest` (fromPointAndNormal p n :: NonVerticalHyperPlane 2 R))
             `shouldBe` GT
         prop "fromPointAnNormal and sideTest consistent for LineEQ" $
           \(p :: Point 2 R) n ->
             allOf components (>0) n ==>
             ((p .+^ n) `onSideTest` (fromPointAndNormal p n :: LineEQ R))
             `shouldBe` GT
         prop "normalVector and fromPointAndNormal consistent (HyperPlane 2 R)" $
           \(p :: Point 2 R) n ->
             allOf components (>0) n ==>
             normalVector (fromPointAndNormal p n :: HyperPlane 2 R) `shouldBe` n

         prop "nonVertical sidetest means above (NonVHyperplane 2)" $
           \(q :: Point 2 R) (h :: NonVerticalHyperPlane 2 R) ->
             let y  = evalAt (projectPoint q) h
                 q' = q&yCoord .~ y+1
             in (q' `onSideTest` h) `shouldBe` GT
         prop "nonVertical sidetest means above (NonVHyperplane 3)" $
           \(q :: Point 3 R) (h :: NonVerticalHyperPlane 3 R) ->
             let z  = evalAt (projectPoint q) h
                 q' = q&zCoord .~ z+1
             in (q' `onSideTest` h) `shouldBe` GT
         prop "nonVertical sidetest means above LineEQ " $
           \(q :: Point 2 R) (h :: LineEQ R) ->
             let y  = evalAt (projectPoint q) h
                 q' = q&yCoord .~ y+1
             in (q' `onSideTest` h) `shouldBe` GT


         -- prop "intersect nonvertical conistent" $
         --   \(l :: LineEQ R) (m :: LineEQ R) ->
         --     (l `intersect` m) `shouldBe` (asHyp l `intersect` asHyp m)

         -- it "intersect tests" $ do
         --   let h = HalfSpace Positive $ horizontalLine (4 % 1 :: Rational)
         --       l = LinePV origin (Vector2 (1 % 1) (1 % 1 :: Rational))
         --   ((horizontalLine @Rational $ 5 % 1) `intersects` h) `shouldBe` True
         --   (l `intersects` h) `shouldBe` True


showReadTests :: Spec
showReadTests = describe "show/read tests for" $ do
  describe "HyperPlane1" $ do
    prop "Double"       $ \(v :: HyperPlane 1 Double)         -> (read . show) v == v
    prop "Int"          $ \(v :: HyperPlane 1 Int)            -> (read . show) v == v
    prop "Rational"     $ \(v :: HyperPlane 1 Rational)       -> (read . show) v == v

  describe "HyperPlane2" $ do
    prop "Double"       $ \(v :: HyperPlane 2 Double)         -> (read . show) v == v
    prop "Int"          $ \(v :: HyperPlane 2 Int)            -> (read . show) v == v
    prop "Rational"     $ \(v :: HyperPlane 2 Rational)       -> (read . show) v == v

  describe "HyperPlane3" $ do
    prop "Double"       $ \(v :: HyperPlane 3 Double)         -> (read . show) v == v
    prop "Int"          $ \(v :: HyperPlane 3 Int)            -> (read . show) v == v
    prop "Rational"     $ \(v :: HyperPlane 3 Rational)       -> (read . show) v == v

  -- describe "HyperPlane4" $ do
  --   prop "Double"       $ \(v :: HyperPlane 4 Double)         -> (read . show) v == v
  --   prop "Int"          $ \(v :: HyperPlane 4 Int)            -> (read . show) v == v
  --   prop "Rational"     $ \(v :: HyperPlane 4 Rational)       -> (read . show) v == v




lineA, lineB :: LineEQ R
lineA = LineEQ 1.1352896048 (-1.0479630631)
lineB = LineEQ 0.8777185381 1.5392597445

hypA = asHyp lineA
hypB = asHyp lineB

test :: Bool
test = hypA `intersects` hypB

myHyp3 :: HyperPlane 2 R
myHyp3 = hyperPlaneFromEquation $ Vector3 2 1 (-1)

testz = Point2 0 2 `onHyperPlane` myHyp3
