{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.SubLineSpec where

import Control.Lens
import Data.Ext
import Data.Geometry
import Data.Geometry.Line
import Data.Geometry.LineSegment
import Data.Geometry.SubLine
import Data.Ratio
import Data.UnBounded
import Data.Vinyl.CoRec
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = do
  describe "subLineTests" $
    it "subline specialization in R^2" $
      property $ \(alpha :: Rational) l@(Line p v) (i :: Interval () Rational)  ->
        let q  = p .+^ alpha *^ v
            sl = SubLine l i
        in onSubLineOrig q sl `shouldBe` onSubLine2 q sl

  it "manual test " $
      ((Point2 (-1) (-1 :: Rational)) `onSubLine2`
       (seg^._SubLine))
    `shouldBe` False

  it "Intersection test" $ do
    let mySeg :: LineSegment 2 () Rational
        mySeg    = ClosedLineSegment (ext origin) (ext $ Point2 14 0)
        myLine :: SubLine 2 () (UnBounded Rational) Rational
        myLine   = fromLine $ lineThrough (Point2 0 0) (Point2 10 0)
        myAnswer :: Interval () (UnBounded Rational)
        myAnswer = ClosedInterval (ext $ Val 0) (ext . Val $ 7 % 5)
    (myLine `intersect` (mkSL mySeg)) `shouldBe` coRec (myLine&subRange .~ myAnswer)

    let sc, sm :: LineSegment 2 () Rational
        sc = ClosedLineSegment (ext $ Point2 1 1) (ext $ Point2 1 2)
        sm = ClosedLineSegment (ext $ Point2 1 2) (ext $ Point2 1 1)
    (sc `intersects` sm) `shouldBe` True




mkSL  :: (Num r, Arity d) => LineSegment d () r -> SubLine d () (UnBounded r) r
mkSL s = s^._SubLine.re _unBounded


seg :: LineSegment 2 () Rational
seg = ClosedLineSegment (ext (Point2 1 1)) (ext (Point2 5 5))



-- | Original def of onSubline
onSubLineOrig                 :: (Ord r, Fractional r, Arity d)
                          => Point d r -> SubLine d p r r -> Bool
onSubLineOrig p (SubLine l r) = toOffset' p l `inInterval` r
