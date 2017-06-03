{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.SubLineSpec where

import Data.Ext
import Control.Lens
import Data.Geometry.LineSegment
import Data.Geometry.Interval
import Data.Geometry.Line
import Data.Geometry.Point
import Data.Geometry.SubLine
import Data.Geometry.Vector
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


seg :: LineSegment 2 () Rational
seg = ClosedLineSegment (ext (Point2 1 1)) (ext (Point2 5 5))



-- | Original def of onSubline
onSubLineOrig                 :: (Ord r, Fractional r, Arity d)
                          => Point d r -> SubLine d p r -> Bool
onSubLineOrig p (SubLine l r) = toOffset p l `inInterval` r
