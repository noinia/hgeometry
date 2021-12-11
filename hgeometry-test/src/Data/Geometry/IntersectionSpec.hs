{-# LANGUAGE UnicodeSyntax #-}
module Data.Geometry.IntersectionSpec where

import Data.Ext
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.Polygon
import Data.Intersection
import Data.Proxy
import Data.RealNumber.Rational
import Ipe
import Paths_hgeometry_test
import Test.Hspec
import Test.QuickCheck



-- |
spec = prop ("intersects agrees with intersect: Data.Geometry.Line ") propIntersectionsAgree



propIntersectionsAgree     :: ( IsIntersectableWith a b
                              , NoIntersection ∈ IntersectionOf a b
                              , RecApplicative (IntersectionOf g h)
                              )
                           => a -> b -> Expectation
propIntersectionsAgree a b = (a `intersects` b)
                             `shouldBe`
                             (defaultNonEmptyIntersection a b)
