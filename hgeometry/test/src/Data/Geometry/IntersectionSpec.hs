{-# LANGUAGE UnicodeSyntax #-}
module Data.Geometry.IntersectionSpec where

import Data.Ext
import Data.Geometry.Line
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.Polygon
import Data.Intersection
import Data.Proxy
import Data.RealNumber.Rational
import Data.Vinyl
import Paths_hgeometry
import Test.Hspec
import Test.QuickCheck

--------------------------------------------------------------------------------

type R = RealNumber 5


-- |
spec :: Spec
spec = do
        it ("intersects agrees with intersect: Data.Geometry.Line ") $
           property (propIntersectionsAgree @(Line 2 R) @(Line 2 R))
        it ("intersects agrees with intersect: Data.Geometry.LineSegment ") $
           property (propIntersectionsAgree @(LineSegment 2 () R) @(LineSegment 2 () R))
        it ("intersects agrees with intersect: Data.Geometry.LineSegment x Line ") $
           property (propIntersectionsAgree @(LineSegment 2 () R) @(Line 2 R))

propIntersectionsAgree     :: forall a b. ( IsIntersectableWith a b
                              , NoIntersection ∈ IntersectionOf a b
                              , RecApplicative (IntersectionOf a b)
                              )
                           => a -> b -> Expectation
propIntersectionsAgree a b = (a `intersects` b)
                             `shouldBe`
                             (defaultNonEmptyIntersection (Proxy @a) (Proxy @b) $ a `intersect` b)
