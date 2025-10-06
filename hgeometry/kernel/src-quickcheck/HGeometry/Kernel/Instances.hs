{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Kernel.Instances
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Arbitrary instances for the types in hgeometry-kernel
--
--------------------------------------------------------------------------------
module HGeometry.Kernel.Instances where

import Control.Lens hiding (cons)
import Data.Maybe (catMaybes)
import Data.Semialign
import GHC.TypeLits
import HGeometry.Ball
import HGeometry.Disk
import HGeometry.Box
import HGeometry.Combinatorial.Instances ()
import HGeometry.HalfSpace
import HGeometry.HyperPlane (HyperPlane(..))
import HGeometry.HyperPlane.NonVertical (NonVerticalHyperPlane(..))
import HGeometry.Interval
import HGeometry.Interval.EndPoint ()
import HGeometry.Line.General
import HGeometry.HalfLine
import HGeometry.Line.LineEQ
import HGeometry.Line.PointAndVector
import HGeometry.LineSegment
import HGeometry.Matrix
import HGeometry.Point
import HGeometry.Point.Instances ()
import HGeometry.Properties
import HGeometry.Triangle
import HGeometry.Vector
import HGeometry.Vector.Instances ()
import Test.QuickCheck

--------------------------------------------------------------------------------

-- instance Arbitrary v => Arbitrary (PointF v) where
--   arbitrary = Point <$> arbitrary

instance Arbitrary r => Arbitrary (EndPoint ep r) where
  arbitrary = EndPoint <$> arbitrary
  shrink (EndPoint p) = EndPoint <$> shrink p

instance Arbitrary EndPointType where
  arbitrary = (\b -> if b then Open else Closed) <$> arbitrary
  shrink = \case
    Open   -> [Closed]
    Closed -> []

instance Arbitrary r => Arbitrary (AnEndPoint r) where
  arbitrary = AnEndPoint <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance ( Arbitrary (endPoint r)
         , Eq (endPoint r), Ord r, IxValue (endPoint r) ~ r, EndPoint_ (endPoint r)
         ) => Arbitrary (Interval endPoint r) where
  arbitrary = do p <- arbitrary
                 q <- arbitrary `suchThat` (isValidInterval p)
                 pure $ buildInterval p q
  shrink i = [ buildInterval p q
             | p <- shrink $ i^.startPoint
             , q <- shrink $ i^.endPoint
             , isValidInterval p q
             ]

isValidInterval     :: (Eq (endPoint r), Ord r, IxValue (endPoint r) ~ r, EndPoint_ (endPoint r))
                    => endPoint r -> endPoint r -> Bool
isValidInterval p q = p /= q && ((p^._endPoint == q^._endPoint) `implies` bothClosed p q)

bothClosed     :: EndPoint_ (endPoint r) => endPoint r -> endPoint r -> Bool
bothClosed p q = endPointType p == Closed && endPointType q == Closed

implies :: Bool -> Bool -> Bool
implies p q = not p || q


instance ( Arbitrary point, Arbitrary (Vector (Dimension point) (NumType point))
         , Num (NumType point), Eq (Vector (Dimension point) (NumType point))
         , Has_ Additive_ (Dimension point) (NumType point)
         ) => Arbitrary (HalfLine point) where
  arbitrary = HalfLine <$> arbitrary <*> (arbitrary `suchThat` (/= zero))

instance ( Arbitrary (endPoint point)
         , IsEndPoint (endPoint point) (endPoint point)
         , IxValue (endPoint point) ~ point
         , Eq point
         ) => Arbitrary (LineSegment endPoint point) where
  arbitrary = do p <- arbitrary
                 q <- arbitrary `suchThat` (\q' -> q'^._endPoint /= p^._endPoint)
                 pure $ LineSegment p q
  shrink s = [ LineSegment p q
             | p <- shrink $ s^.startPoint
             , q <- shrink $ s^.endPoint
             , q^._endPoint /= p^._endPoint
             ]

instance ( Arbitrary point
         , Arbitrary (NumType point)
         , Ord (NumType point)
         , Num (NumType point)
         ) => Arbitrary (Ball point) where
  arbitrary = Ball <$> arbitrary
                   <*> (arbitrary `suchThat` (> 0))
  shrink (Ball c r) = [ Ball c' r'
                      | c' <- shrink c
                      , r' <- 1 : shrink r
                      , r' > 0
                      ]

instance (Arbitrary point, Eq point, ConstructablePoint_ point d r, Num r
         ) => Arbitrary (DiametralBall point) where
  arbitrary = do p <- arbitrary
                 q <- arbitrary `suchThat` (/= p)
                 pure $ DiametralPoints p q
  shrink (DiametralPoints p q) = [ DiametralPoints p' q'
                                 | p' <- origin : shrink p
                                 , q' <- shrink q
                                 , p' /= q'
                                 ]

instance (Arbitrary point, Point_ point 2 r, Ord r, Num r
         ) => Arbitrary (BallByPoints' 3 point) where
  arbitrary = do mDisk <- diskFromPoints <$> arbitrary <*> arbitrary <*> arbitrary
                 case mDisk of
                   Just disk -> pure disk
                   Nothing   -> arbitrary

  shrink (BoundaryPoints (Vector3 a b c)) =
    catMaybes [ diskFromPoints a' b' c'
              | a' <- shrink a, b' <- shrink b, c' <- shrink c
              ]

instance (Arbitrary point, ConstructablePoint_ point 2 r, Ord r, Num r, Eq point
         ) => Arbitrary (DiskByPoints point) where
  arbitrary = oneof [DiametralDisk <$> arbitrary, DiskByPoints <$> arbitrary]

instance ( Arbitrary point
         , Point_ point 2 r, Num r, Ord r
         , Eq point
         ) => Arbitrary (Triangle point) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary `suchThat` (/= a)
                 c <- arbitrary `suchThat` (\c' -> c' /= a && c' /= b && ccw a b c' /= CoLinear)
                 pure $ Triangle a b c
  shrink (Triangle a b c) = [ Triangle a' b' c'
                            | a' <- shrink a
                            , b' <- shrink b
                            , c' <- shrink c
                            , b' /= a', c' /= a', c' /= b', ccw a' b' c' /= CoLinear
                            ]

instance Arbitrary r => Arbitrary (LineEQ r) where
  arbitrary = LineEQ <$> arbitrary <*> arbitrary

instance Arbitrary r => Arbitrary (VerticalOrLineEQ r) where
  arbitrary = frequency [ (5,  VerticalLineThrough <$> arbitrary)
                        , (95, NonVertical <$> arbitrary)
                        ]


instance ( Arbitrary r
         , Has_ Additive_ d r
         , Eq (Vector d r), Num r
         , KnownNat d
         ) => Arbitrary (LinePV d r) where
  arbitrary = LinePV <$> arbitrary
                     <*> (arbitrary `suchThat` (/= zero))


instance ( Arbitrary point
         , Arbitrary r
         , Point_ point d r
         , Num r
         , Ord r
         , Zip (Vector d)
         ) => Arbitrary (Box point) where
  arbitrary = (\p v -> Box p (p .+^ v)) <$> arbitrary
                                        <*> arbitrary `suchThat` (allOf components (> 0))
  shrink b = [ Box p (p .+^ v)
             | p <- shrink $ b^.minPoint
             , v <- shrink $ size b
             , allOf components (> 0) v
             ]

instance ( Has_ Additive_ m r
         , Has_ Vector_ n (Vector m r)
         , Ixed (Vector n (Vector m r))
         , Ixed (Vector m r)
         , Arbitrary r
         ) =>
  Arbitrary (Matrix n m r) where
  arbitrary = (matrixFromRows :: Vector n (Vector m r) -> Matrix n m r)
           <$> arbitrary

instance ( Arbitrary r, Has_ Vector_ (d+1) r, Has_ Additive_ d r
         , Num r, Eq (Vector d r)) => Arbitrary (HyperPlane d r) where
  arbitrary = do a0                <- arbitrary
                 (a :: Vector d r) <- arbitrary `suchThat` (/= zero)
                 pure $ HyperPlane $ cons a0 a
  shrink (HyperPlane u) = [ HyperPlane u
                          | v <- shrink u, prefix @d @(d+1) @_ @(Vector d r) v /= zero
                          ]


instance (Arbitrary r, Has_ Additive_ d r
         , Num r, Eq (Vector d r)) => Arbitrary (NonVerticalHyperPlane d r) where
  arbitrary = NonVerticalHyperPlane <$> arbitrary `suchThat` (/= zero)
  shrink (NonVerticalHyperPlane u) = [ NonVerticalHyperPlane u
                                     | v <- shrink u, v /= zero
                                     ]

instance Arbitrary boundingHyperPlane => Arbitrary (HalfSpaceF boundingHyperPlane) where
  arbitrary = HalfSpace <$> arbitrary <*> arbitrary
