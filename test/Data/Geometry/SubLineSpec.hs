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
import QuickCheck.Instances ()
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

  it "Intersection test" $
    let mySeg  = Val <$> ClosedLineSegment (ext origin) (ext $ Point2 (14 :: Rational) 0)
        mySeg' = mySeg^._SubLine
        myLine = fromLine $ lineThrough (Point2 0 0) (Point2 10 (0 :: Rational))
    in (myLine `intersect` mySeg')
       `shouldBe`
       coRec (myLine&subRange .~ ClosedInterval (ext $ Val 0) (ext . Val $ 7 % 5))



seg :: LineSegment 2 () Rational
seg = ClosedLineSegment (ext (Point2 1 1)) (ext (Point2 5 5))



-- | Original def of onSubline
onSubLineOrig                 :: (Ord r, Fractional r, Arity d)
                          => Point d r -> SubLine d p r -> Bool
onSubLineOrig p (SubLine l r) = toOffset p l `inInterval` r
