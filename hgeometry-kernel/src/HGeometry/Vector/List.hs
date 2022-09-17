module HGeometry.Vector.List
  ( ListVector(..)
  ) where

import Control.Applicative (liftA2)
import Control.Lens
import HGeometry.Properties
import GHC.Generics
import HGeometry.Vector.Class

--------------------------------------------------------------------------------

-- | Implementation of a vector by a List.
newtype ListVector d r = ListVector [r]
                       deriving ( Show,Eq,Ord,Functor,Foldable,Traversable
                                , FunctorWithIndex Int
                                , FoldableWithIndex Int
                                , Applicative
                                , Generic
                                )

instance Wrapped (ListVector d r)
instance Rewrapped (ListVector d r) (ListVector d s)

instance Ixed (ListVector d r)  where
  ix i = _Wrapped.ix i



instance TraversableWithIndex Int (ListVector d) where
  itraverse f (ListVector l) = ListVector <$> itraverse f l

type instance Dimension (ListVector d r) = d
type instance NumType   (ListVector d r) = r

type instance Index (ListVector d r) = Int
type instance IxValue (ListVector d r) = r

instance HasComponents (ListVector d r) (ListVector d s) where
  components = itraversed

instance Additive_ (ListVector d r) where
  zero = pure 0
  liftU2 = liftA2
  liftI2 = liftA2

instance Metric_ (ListVector d r)

instance Vector_ (ListVector d r) d r where
  vectorFromList = Just . ListVector
  -- FIXME: implement this properlty; i.e. check that we actually have d elements
