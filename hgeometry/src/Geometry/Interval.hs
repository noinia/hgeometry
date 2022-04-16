--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Interval
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.Geometry.Interval(
                               -- * 1 dimensional Intervals
                               Interval (Interval, OpenInterval,ClosedInterval)
                             , fromRange, toRange
                             , _Range

                               -- * querying the start and end of intervals
                             , HasStart(..), HasEnd(..)
                             -- * Working with intervals
                             , intersectsInterval, inInterval
                             , shiftLeft'

                             , asProperInterval, flipInterval

                             , module Data.Range
                             ) where

import           Control.DeepSeq
import           Control.Lens (Iso', Lens', iso, (%~), (&), (^.))
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Boundary
import           Data.Geometry.Properties
import           Data.Range
import           Data.Semigroup (Arg (..))
import qualified Data.Traversable as T
import           Data.Vinyl
import           Data.Vinyl.CoRec
import           GHC.Generics (Generic)
import           Test.QuickCheck

--------------------------------------------------------------------------------

-- | An Interval is essentially a 'Data.Range' but with possible payload
--
-- We can think of an interval being defined as:
--
-- >>> data Interval a r = Interval (EndPoint (r :+ a)) (EndPoint (r :+ a))
newtype Interval a r = GInterval (Range (r :+ a))
                     deriving (Eq,Generic,Arbitrary)

-- | Cast an interval to a range.
toRange :: Interval a r -> Range (r :+ a)
toRange (GInterval r) = r

-- | Intervals and ranges are isomorphic.
_Range :: Iso' (Interval a r) (Range (r :+ a))
_Range = iso toRange fromRange
{-# INLINE _Range #-}

-- | Constrct an interval from a Range
fromRange :: Range (r :+ a) -> Interval a r
fromRange = GInterval

deriving instance (NFData a, NFData r) => NFData (Interval a r)

instance (Show a, Show r) => Show (Interval a r) where
  show ~(Interval l u) = concat [ "Interval (", show l, ") (", show u,")"]

instance Functor (Interval a) where
  fmap f (GInterval r) = GInterval $ fmap (first f) r

instance F.Foldable (Interval a) where
  foldMap f (GInterval r) = foldMap (f . (^.core)) r

instance T.Traversable (Interval a) where
  traverse f (GInterval r) = GInterval <$> T.traverse f' r
    where
      f' = bitraverse f pure

instance Bifunctor Interval where
  bimap f g (GInterval r) = GInterval $ fmap (bimap g f) r


-- type instance IntersectionOf r (Interval b r) = [NoIntersection, r]
-- -- somehow: GHC does not understand the r here cannot be 'Interval a r' itself :(

-- instance Ord r => r `HasIntersectionWith` Interval b r where
--   x `intersects` r = x `inRange` fmap (^.core) (r^._Range )


-- instance Ord r => r `IsIntersectableWith` Interval b r where
--   x `intersect` r | x `intersects` r = coRec x
--                   | otherwise        = coRec NoIntersection

-- | Test if a value lies in an interval. Note that the difference between
--  inInterval and inRange is that the extra value is *not* used in the
--  comparison with inInterval, whereas it is in inRange.
intersectsInterval       :: Ord r => r -> Interval a r -> Bool
x `intersectsInterval` r = x `inRange` fmap (^.core) (r^._Range )


-- | Compute where the given query value is with respect to the interval.
--
-- Note that even if the boundary of the interval is open we may
-- return "OnBoundary".
inInterval :: Ord r => r -> Interval a r -> PointLocationResult
x `inInterval` (Interval l r) =
  case x `compare` (l^.unEndPoint.core) of
    LT -> Outside
    EQ -> OnBoundary
    GT -> case x `compare` (r^.unEndPoint.core) of
            LT -> Inside
            EQ -> OnBoundary
            GT -> Outside


pattern OpenInterval       :: (r :+ a) -> (r :+ a) -> Interval a r
pattern OpenInterval   l u = GInterval (OpenRange   l u)

pattern ClosedInterval     :: (r :+ a) -> (r :+ a) -> Interval a r
pattern ClosedInterval l u = GInterval (ClosedRange l u)


pattern Interval     :: EndPoint (r :+ a) -> EndPoint (r :+ a) -> Interval a r
pattern Interval l u = GInterval (Range l u)
{-# COMPLETE Interval #-}

--------------------------------------------------------------------------------

class HasStart t where
  type StartCore t
  type StartExtra t
  start :: Lens' t (StartCore t :+ StartExtra t)

instance HasStart (Interval a r) where
  type StartCore (Interval a r) = r
  type StartExtra (Interval a r) = a
  start = _Range.lower.unEndPoint
  {-# INLINE start #-}

class HasEnd t where
  type EndCore t
  type EndExtra t
  end :: Lens' t (EndCore t :+ EndExtra t)

instance HasEnd (Interval a r) where
  type EndCore (Interval a r) = r
  type EndExtra (Interval a r) = a
  end = _Range.upper.unEndPoint
  {-# INLINE end #-}

type instance Dimension (Interval a r) = 1
type instance NumType   (Interval a r) = r


type instance IntersectionOf (Interval a r) (Interval b r)
  = [NoIntersection, Interval (Either a b) r]

instance Ord r => Interval a r `HasIntersectionWith` Interval b r
instance Ord r => Interval a r `IsIntersectableWith` Interval b r where

  nonEmptyIntersection = defaultNonEmptyIntersection

  (GInterval r) `intersect` (GInterval s) = match (r' `intersect` s') $
         H (\NoIntersection -> coRec NoIntersection)
      :& H (\(Range l u)    -> coRec . GInterval $ Range (l&unEndPoint %~ g)
                                                         (u&unEndPoint %~ g) )
      :& RNil
    where
      r' :: Range (Arg r (r :+ Either a b))
      r' = fmap (\(x :+ a) -> Arg x (x :+ Left a))  r
      s' :: Range (Arg r (r :+ Either a b))
      s' = fmap (\(x :+ b) -> Arg x (x :+ Right b)) s

      g (Arg _ x) = x

-- | Shifts the interval to the left by delta
shiftLeft'       :: Num r => r -> Interval a r -> Interval a r
shiftLeft' delta = fmap (subtract delta)


-- | Makes sure the start and endpoint are oriented such that the
-- starting value is smaller than the ending value.
asProperInterval                                     :: Ord r => Interval p r -> Interval p r
asProperInterval i | (i^.start.core) > (i^.end.core) = flipInterval i
                   | otherwise                       = i

-- | Flips the start and endpoint of the interval.
flipInterval :: Interval a r -> Interval a r
flipInterval = _Range %~ \(Range s t) -> Range t s
