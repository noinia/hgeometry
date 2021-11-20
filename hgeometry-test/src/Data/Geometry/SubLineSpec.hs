{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.SubLineSpec (spec) where

import Control.Lens
import Data.Ext
import Data.Geometry
import Data.Geometry.SubLine
import Data.Ratio
import Data.UnBounded
import Test.Hspec
import Test.QuickCheck
import Data.RealNumber.Rational

type R = RealNumber 5

spec :: Spec
spec = do
  describe "subLineTests" $
    it "subline specialization in R^2" $
      property $ \(alpha :: R) l@(Line p v) (i :: Interval () R)  ->
        let q  = p .+^ alpha *^ v
            sl = SubLine l i
        in onSubLineOrig q sl `shouldBe` onSubLine2 q sl

  it "manual test " $
      ((Point2 (-1) (-1 :: R)) `onSubLine2`
       (seg^._SubLine))
    `shouldBe` False
  it "open intersection " $ do
      ((Point2 1 1) `onSubLine2` (seg2^._SubLine))
        `shouldBe` False
      ((Point2 5 5) `onSubLine2` (seg2^._SubLine))
        `shouldBe` False
      ((Point2 2 2) `onSubLine2` (seg2^._SubLine))
        `shouldBe` True

  it "Intersection test" $ do
    let mySeg :: LineSegment 2 Char R
        mySeg    = ClosedLineSegment (origin :+ 'a') (Point2 14 0 :+ 'b')

        myLine :: SubLine 2 () (UnBounded R) R
        myLine   = fromLine $ lineThrough (Point2 0 0) (Point2 10 0)

        myAnswer :: Interval (Either () Char) (UnBounded R)
        myAnswer = ClosedInterval (Val 0 :+ Right 'a') (Val (7 / 5) :+ Right 'b')
    (myLine `intersect` (mkSL mySeg)) `shouldBe` coRec (myLine&subRange .~ myAnswer)

    let sc, sm :: LineSegment 2 () R
        sc = ClosedLineSegment (ext $ Point2 1 1) (ext $ Point2 1 2)
        sm = ClosedLineSegment (ext $ Point2 1 2) (ext $ Point2 1 1)
    (sc `intersects` sm) `shouldBe` True




mkSL  :: (Num r, Arity d) => LineSegment d a r -> SubLine d a (UnBounded r) r
mkSL s = s^._SubLine.re _unBounded


seg :: LineSegment 2 () R
seg = ClosedLineSegment (ext (Point2 1 1)) (ext (Point2 5 5))

seg2 :: LineSegment 2 () R
seg2 = OpenLineSegment (ext (Point2 1 1)) (ext (Point2 5 5))


-- | Original def of onSubline
onSubLineOrig                 :: (Ord r, Fractional r, Arity d)
                          => Point d r -> SubLine d p r r -> Bool
onSubLineOrig p (SubLine l r) = toOffset' p l `inInterval` r
