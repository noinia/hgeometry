module Geometry.Point.WithExtra
  ( WithExtra(WithExtra)
  , _WithExtra


  , ccw'
  , sortAround'
  , ccwCmpAroundWith'
  , cwCmpAroundWith'
  , cmpByDistanceTo'
  , cwCmpAround'
  ) where

import Control.Lens
import Data.Coerce
import Data.Ext
import Geometry.Point.Boxed
import Geometry.Point.EuclideanDistance(cmpByDistanceTo)
import Geometry.Point.Class
import Geometry.Point.Orientation.Degenerate (sortAround, ccwCmpAroundWith,cwCmpAroundWith, cwCmpAround, ccw, CCW(..))
import Geometry.Properties
import Geometry.Vector


newtype WithExtra point extra d r = WithExtra (point d r :+ extra)
type instance Dimension (WithExtra point extra d r) = d
type instance NumType (WithExtra point extra d r) = r

_WithExtra :: Iso (WithExtra point extra d r) (WithExtra point extra d s) (point d r :+ extra) (point d s :+ extra)
_WithExtra = iso coerce coerce

instance AsExt (WithExtra point extra d r) where
  type CoreOf (WithExtra point extra d r)  = point d r
  type ExtraOf (WithExtra point extra d r) = extra
  _Ext = _WithExtra

-- TODO: HACK
instance AsExt (Point 2 r) where
  type CoreOf (Point 2 r)  = Point 2 r
  type ExtraOf (Point 2 r) = ()
  _Ext = iso ext (view core)

instance Affine (point d)  => Affine (WithExtra point extra d) where
  type Diff (WithExtra point extra d) = Diff (point d)

  (WithExtra (p :+ _)) .-. (WithExtra (q :+ _)) = p .-. q
  p .+^ v = p&_WithExtra.core %~ (.+^ v)

instance (Point_ point d r) => Point_ (WithExtra point extra) d r where
  fromVector v = WithExtra $ fromVector v :+ undefined --- FIXME: hack
  asVector = _WithExtra . core . asVector



sortAround'          :: forall p q r. (Ord r, Num r)
                     => Point 2 r :+ q -> [Point 2 r :+ p] -> [Point 2 r :+ p]
sortAround' (c :+ _) = coerce
                     . sortAround (fromGenericPoint c)
                     . coerce @[Point 2 r :+ p] @[WithExtra Point p 2 r]



ccwCmpAroundWith'                               :: forall a b c r. (Ord r, Num r)
                                              => Vector 2 r
                                              -> Point 2 r :+ a
                                              -> Point 2 r :+ b
                                              -> Point 2 r :+ c
                                              -> Ordering
ccwCmpAroundWith' v a b c = ccwCmpAroundWith v (a^.core) (b^.core) (c^.core)



cwCmpAroundWith'                               :: forall a b c r. (Ord r, Num r)
                                              => Vector 2 r
                                              -> Point 2 r :+ a
                                              -> Point 2 r :+ b
                                              -> Point 2 r :+ c
                                              -> Ordering
cwCmpAroundWith' v a b c = cwCmpAroundWith v (a^.core) (b^.core) (c^.core)


cwCmpAround'                               :: forall a b c r. (Ord r, Num r)
                                              => Point 2 r :+ a
                                              -> Point 2 r :+ b
                                              -> Point 2 r :+ c
                                              -> Ordering
cwCmpAround' a b c = cwCmpAround (a^.core) (b^.core) (c^.core)


ccw'       :: (Ord r, Num r) => Point 2 r :+ a -> Point 2 r :+ b -> Point 2 r :+ c -> CCW
ccw' a b c = ccw (a^.core) (b^.core) (c^.core)


cmpByDistanceTo'       :: (Ord r, Num r)
                       => Point 2 r :+ a -> Point 2 r :+ b -> Point 2 r :+ c -> Ordering
cmpByDistanceTo' c p q = cmpByDistanceTo (c^.core) (p^.core) (q^.core)
