module Geometry.Point.WithExtra
  ( WithExtra(WithExtra)
  , _WithExtra


  , ccw'
  , sortAround'
  , ccwCmpAroundWith'
  , cwCmpAroundWith'
  ) where

import Control.Lens
import Data.Coerce
import Data.Ext
import Geometry.Point.Boxed
import Geometry.Point.Class
import Geometry.Point.Orientation.Degenerate (sortAround, ccwCmpAroundWith,cwCmpAroundWith, ccw, CCW(..))
import Geometry.Properties
import Geometry.Vector


newtype WithExtra point extra d r = WithExtra (point d r :+ extra)
type instance Dimension (WithExtra point extra d r) = d
type instance NumType (WithExtra point extra d r) = r

_WithExtra :: Iso (WithExtra point extra d r) (WithExtra point extra d s) (point d r :+ extra) (point d s :+ extra)
_WithExtra = iso coerce coerce

instance Affine (point d)  => Affine (WithExtra point extra d) where
  type Diff (WithExtra point extra d) = Diff (point d)

  (WithExtra (p :+ _)) .-. (WithExtra (q :+ _)) = p .-. q
  p .+^ v = p&_WithExtra.core %~ (.+^ v)

instance (Point_ point d r) => Point_ (WithExtra point extra) d r where
  fromVector v = WithExtra $ fromVector v :+ undefined --- FIXME: hack
  asVector = _WithExtra . core . asVector



sortAround'   :: forall p r. (Ord r, Num r)
              => Point 2 r :+ p -> [Point 2 r :+ p] -> [Point 2 r :+ p]
sortAround' c = coerce . sortAround (coerce @(Point 2 r :+ p) @(WithExtra Point p 2 r) c) . coerce


ccwCmpAroundWith'                               :: forall p r. (Ord r, Num r)
                                              => Vector 2 r
                                              -> Point 2 r :+ p
                                              -> Point 2 r :+ p
                                              -> Point 2 r :+ p
                                              -> Ordering
ccwCmpAroundWith' v a b c = ccwCmpAroundWith v (coerce' a) (coerce' b) (coerce' c)
  where
    coerce' :: Point 2 r :+ p -> WithExtra Point p 2 r
    coerce' = coerce

cwCmpAroundWith'                               :: forall p r. (Ord r, Num r)
                                              => Vector 2 r
                                              -> Point 2 r :+ p
                                              -> Point 2 r :+ p
                                              -> Point 2 r :+ p
                                              -> Ordering
cwCmpAroundWith' v a b c = cwCmpAroundWith v (coerce' a) (coerce' b) (coerce' c)
  where
    coerce' :: Point 2 r :+ p -> WithExtra Point p 2 r
    coerce' = coerce

ccw'       :: (Ord r, Num r) => Point 2 r :+ a -> Point 2 r :+ b -> Point 2 r :+ c -> CCW
ccw' a b c = ccw (a^.core) (b^.core) (c^.core)
