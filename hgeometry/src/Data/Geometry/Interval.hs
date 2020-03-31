{-# LANGUAGE TemplateHaskell  #-}
module Data.Geometry.Interval(
                             -- * 1 dimensional Intervals
                               Interval
                             , fromRange, toRange
                             , _Range

                             , pattern OpenInterval
                             , pattern ClosedInterval
                             , pattern Interval

                             -- * querying the start and end of intervals
                             , HasStart(..), HasEnd(..)
                             -- * Working with intervals
                             , inInterval
                             , shiftLeft'

                             , asProperInterval, flipInterval

                             , module Data.Range
                             ) where

import           Control.DeepSeq
import           Control.Lens (lens, (^.),(%~),(&), Lens')
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Properties
import           Data.Range
import           Data.Semigroup(Arg(..))
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
newtype Interval a r = GInterval { toRange :: Range (r :+ a) }
                     deriving (Eq,Generic,Arbitrary)

_Range :: Lens' (Interval a r) (Range (r :+ a))
_Range = lens toRange (const GInterval)

-- | Constrct an interval from a Range
fromRange :: Range (r :+ a) -> Interval a r
fromRange = GInterval

deriving instance (NFData a, NFData r) => NFData (Interval a r)

instance (Show a, Show r) => Show (Interval a r) where
  show ~(Interval l u) = concat [ "Interval (", show l, ") (", show u,")"]

instance Functor (Interval a) where
  fmap = T.fmapDefault

instance F.Foldable (Interval a) where
  foldMap = T.foldMapDefault

instance T.Traversable (Interval a) where
  traverse f (GInterval r) = GInterval <$> T.traverse f' r
    where
      f' = bitraverse f pure

instance Bifunctor Interval where
  bimap f g (GInterval r) = GInterval $ fmap (bimap g f) r



-- | Test if a value lies in an interval. Note that the difference between
--  inInterval and inRange is that the extra value is *not* used in the
--  comparison with inInterval, whereas it is in inRange.
inInterval       :: Ord r => r -> Interval a r -> Bool
x `inInterval` r = x `inRange` (fmap (^.core) $ r^._Range )


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

class HasEnd t where
  type EndCore t
  type EndExtra t
  end :: Lens' t (EndCore t :+ EndExtra t)

instance HasEnd (Interval a r) where
  type EndCore (Interval a r) = r
  type EndExtra (Interval a r) = a
  end = _Range.upper.unEndPoint

type instance Dimension (Interval a r) = 1
type instance NumType   (Interval a r) = r


type instance IntersectionOf (Interval a r) (Interval a r) = [NoIntersection, Interval a r]

instance Ord r => (Interval a r) `IsIntersectableWith` (Interval a r) where

  nonEmptyIntersection = defaultNonEmptyIntersection

  (GInterval r) `intersect` (GInterval s) = match (r' `intersect` s') $
         (H $ \NoIntersection -> coRec NoIntersection)
      :& (H $ \(Range l u)    -> coRec . GInterval $ Range (l&unEndPoint %~ g)
                                                           (u&unEndPoint %~ g) )
      :& RNil
    where
      f x = Arg (x^.core) x
      r' = fmap f r
      s' = fmap f s

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
