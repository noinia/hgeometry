module Data.Util where

import Control.Lens
import Data.Semigroup

-- |  strict triple
data STR a b c = STR { fst' :: !a, snd' :: !b , trd' :: !c}
               deriving (Show,Eq,Ord,Functor)


instance Field1 (STR a b c) (STR d b c) a d where
  _1 = lens fst' (\(STR _ b c) d -> STR d b c)

instance Field2 (STR a b c) (STR a d c) b d where
  _2 = lens snd' (\(STR a _ c) d -> STR a d c)

instance Field3 (STR a b c) (STR a b d) c d where
  _3 = lens trd' (\(STR a b _) d -> STR a b d)





-- | Strict pair
data SP a b = SP !a !b deriving (Show,Eq,Ord,Functor)

instance (Semigroup a, Semigroup b) => Semigroup (SP a b) where
  (SP a b) <> (SP c d) = SP (a <> c) (b <> d)

instance Field1 (SP a b) (SP c b) a c where
  _1 = lens (\(SP a _) -> a) (\(SP _ b) c -> SP c b)

instance Field2 (SP a b) (SP a c) b c where
  _2 = lens (\(SP _ b) -> b) (\(SP a _) c -> SP a c)

instance Bifunctor SP where
  bimap f g (SP a b) = SP (f a) (g b)

-- | Strict pair with both items the same
type Two a = SP a a
