module Algorithms.Geometry.SoS.RWithIdx where

import           Data.Ord (Down(..))

--------------------------------------------------------------------------------

-- data R r = Constant !r | Indirect !r {-# UNPACK #-} !Int {-# UNPACK #-} !Int

-- instance Show r => Show (R r) where
--   show (Constant r)     = show r
--   show (Indirect r _ _) = show r

-- instance Eq r => Eq (R r) where

data RWithIdx r = RWithIdx !r
                           {-# UNPACK #-} !Int -- ^ index of the point in [0,n]
                           {-# UNPACK #-} !Int -- ^ index of the coordinate in [0,d-1]
                deriving (Eq)

instance Show r => Show (RWithIdx r) where
  showsPrec i (RWithIdx r _ _) = showsPrec i r

instance Ord r => Ord (RWithIdx r) where
  (RWithIdx x i j) `compare` (RWithIdx y k l) = x `compare` y
                                             <> (Down i) `compare` (Down k)
                                             <> j `compare` l
  -- see the paper, function Smaller, for the slightly weird implementation I guess.




-- | Data type that allows us to combine indexed R values and "normal"
-- constant R values.  Two indexed r values are equal if and only if
-- their indices match exactly. That means that values comming form
-- different indices can always be ordered, i.e. we get rid of degeneracies.
--
-- When comparing an indexed value against a constant the
-- equality/ordering is determined only w.r.t. the "real" r value.
--
data R r = Constant !r
         | IndexedR !r {-# UNPACK #-} !Int -- ^ index of the point in [0,n]
                       {-# UNPACK #-} !Int -- ^ index of the coordinate in [0,d-1]

toR :: R r -> r
toR = \case
  Constant r      -> r
  IndexedR r _ _  -> r

fromR :: r -> R r
fromR = Constant

fromIndexed                  :: RWithIdx r -> R r
fromIndexed (RWithIdx x i j) = IndexedR x i j

instance Show r => Show (R r) where
  showsPrec i = showsPrec i . toR

instance Eq r => Eq (R r) where
  (IndexedR x i j) == (IndexedR y k l) = x == y && i == k && j == l
  a                == b                = toR a == toR b

instance Ord r => Ord (R r) where
  (IndexedR x i j) `compare` (IndexedR y k l) = RWithIdx x i j `compare` RWithIdx y k l
  a                `compare` b                = toR a          `compare` toR b


-- instance (Num r, Ord r) => Num (R r) where
--   -- +, *, -, abs, signum,fromInteger, negate
--   -- (Constant )
--   signum
--   fromInteger = Constant

--   -- negate is prob. troublesome
