{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}
module Data.Geometry.Interval-- (
                             -- -- * 1 dimensional Intervals
                             --   Interval(..)
                             -- , Intersection(..)

                             -- -- * querying the start and end of intervals
                             -- , HasStart(..), HasEnd(..)
                             -- -- * Working with intervals
                             -- , width
                             -- , inInterval
                             -- )
       where

import           Control.Applicative
import           Control.Lens(makeLenses, (^.),(%~),(.~),(&), Lens')
import           Data.Bitraversable
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Properties
import           Data.Range
import           Data.Semigroup
import qualified Data.Traversable as T
import           Data.Vinyl
import           Frames.CoRec

--------------------------------------------------------------------------------

-- | An Interval is essentially a 'Data.Range' but with possible payload
newtype Interval a r = GInterval { _unInterval :: Range (r :+ a) }
                     deriving (Eq)
makeLenses ''Interval

instance (Show a, Show r) => Show (Interval a r) where
  show (Interval l u) = concat [ "Interval (", show l, ") (", show u,")"]

instance Functor (Interval a) where
  fmap = T.fmapDefault

instance F.Foldable (Interval a) where
  foldMap = T.foldMapDefault

instance T.Traversable (Interval a) where
  traverse f (GInterval r) = GInterval <$> T.traverse f' r
    where
      f' = bitraverse f pure

-- | Test if a value lies in an interval. Note that the difference between
--  inInterval and inRange is that the extra value is *not* used in the
--  comparison with inInterval, whereas it is in inRange.
inInterval       :: Ord r => r -> Interval a r -> Bool
x `inInterval` r = x `inRange` (fmap (^.core) $ r^.unInterval )


-- :: (r :+ a) -> (r :+ a) -> OpenInterval a r
pattern OpenInterval   l u = GInterval (OpenRange   l u)
pattern ClosedInterval l u = GInterval (ClosedRange l u)

-- | :: EndPoint (r :+ a) -> EndPoint (r :+ a) -> Interval a r
pattern Interval l u = GInterval (Range l u)


--------------------------------------------------------------------------------

class HasStart t where
  type StartCore t
  type StartExtra t
  start :: Lens' t (StartCore t :+ StartExtra t)

instance HasStart (Interval a r) where
  type StartCore (Interval a r) = r
  type StartExtra (Interval a r) = a
  start = unInterval.lower.unEndPoint

class HasEnd t where
  type EndCore t
  type EndExtra t
  end :: Lens' t (EndCore t :+ EndExtra t)

instance HasEnd (Interval a r) where
  type EndCore (Interval a r) = r
  type EndExtra (Interval a r) = a
  end = unInterval.upper.unEndPoint

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
      s' = fmap f r

      g (Arg _ x) = x


shiftLeft'   :: Num r => r -> Interval a r -> Interval a r
shiftLeft' x = fmap (subtract x)
