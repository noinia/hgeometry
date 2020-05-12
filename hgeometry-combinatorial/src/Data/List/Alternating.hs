module Data.List.Alternating(
    Alternating(..)
  , withNeighbours
  , mergeAlternating
  , insertBreakPoints
  , reverse
  ) where

import Prelude hiding (reverse)
import Control.Lens
import Data.Bifoldable
import Data.Bitraversable
import Data.Ext
import qualified Data.List as List

--------------------------------------------------------------------------------

-- | A (non-empty) alternating list of a's and b's
data Alternating a b = Alternating a [b :+ a] deriving (Show,Eq,Ord)

instance Bifunctor Alternating where
  bimap = bimapDefault
instance Bifoldable Alternating where
  bifoldMap = bifoldMapDefault
instance Bitraversable Alternating where
  bitraverse f g (Alternating a xs) = Alternating <$> f a <*> traverse (bitraverse g f) xs


-- | Computes a b with all its neighbours
--
-- >>> withNeighbours (Alternating 0 ['a' :+ 1, 'b' :+ 2, 'c' :+ 3])
-- [(0,'a' :+ 1),(1,'b' :+ 2),(2,'c' :+ 3)]
withNeighbours                     :: Alternating a b -> [(a,b :+ a)]
withNeighbours (Alternating a0 xs) = let as = a0 : map (^.extra) xs
                                     in zipWith (\a ba -> (a,ba)) as xs



-- | Generic merging scheme that merges two Alternatings and applies
-- the function 'f', with the current/new value at every event. So
-- note that if the alternating consists of 'Alternating a0 [t1 :+
-- a1]' then the function is applied to a1, not to a0 (i.e. every
-- value ai is considered alive on the interval [ti,t(i+1))
--
-- >>> let odds  = Alternating "a" [3 :+ "c", 5 :+ "e", 7 :+ "g"]
-- >>> let evens = Alternating "b" [4 :+ "d", 6 :+ "f", 8 :+ "h"]
-- >>> mergeAlternating (\_ a b -> a <> b) odds evens
-- [3 :+ "cb",4 :+ "cd",5 :+ "ed",6 :+ "ef",7 :+ "gf",8 :+ "gh"]
-- >>> mergeAlternating (\t a b -> if t `mod` 2 == 0 then a else b) odds evens
-- [3 :+ "b",4 :+ "c",5 :+ "d",6 :+ "e",7 :+ "f",8 :+ "g"]
-- >>> mergeAlternating (\_ a b -> a <> b) odds (Alternating "b" [0 :+ "d", 5 :+ "e", 8 :+ "h"])
-- [0 :+ "ad",3 :+ "cd",5 :+ "ee",7 :+ "ge",8 :+ "gh"]
mergeAlternating                         :: Ord t
                                         => (t -> a -> b -> c)
                                         -> Alternating a t -> Alternating b t -> [t :+ c]
mergeAlternating f (Alternating a00 as0)
                   (Alternating b00 bs0) = go a00 b00 as0 bs0
  where
    go a  _  []                bs                 = map (\(t :+ b) -> t :+ f t a b) bs
    go _  b  as                []                 = map (\(t :+ a) -> t :+ f t a b) as
    go a0 b0 as@((t :+ a):as') bs@((t' :+ b):bs') = case t `compare` t' of
                                                      LT -> (t  :+ f t  a  b0) : go a  b0 as' bs
                                                      EQ -> (t  :+ f t  a  b)  : go a  b  as' bs'
                                                      GT -> (t' :+ f t' a0 b)  : go a0 b  as  bs'


-- | Adds additional t-values in the alternating, (in sorted order). I.e. if we insert a
-- "breakpoint" at time t the current 'a' value is used at that time.
--
-- >>> insertBreakPoints [0,2,4,6,8,10] $ Alternating "a" [3 :+ "c", 5 :+ "e", 7 :+ "g"]
-- Alternating "a" [0 :+ "a",2 :+ "a",3 :+ "c",4 :+ "c",5 :+ "e",6 :+ "e",7 :+ "g",8 :+ "g",10 :+ "g"]
insertBreakPoints                         :: Ord t => [t] -> Alternating a t -> Alternating a t
insertBreakPoints ts a@(Alternating a0 _) =
  Alternating a0 $ mergeAlternating (\_ _ a' -> a') (Alternating undefined (ext <$> ts)) a


-- | Reverses an alternating list.
--
-- >>> reverse $ Alternating "a" [3 :+ "c", 5 :+ "e", 7 :+ "g"]
-- Alternating "g" [7 :+ "e",5 :+ "c",3 :+ "a"]
reverse                      :: Alternating a b -> Alternating a b
reverse p@(Alternating s xs) = case xs of
    []             -> p
    ((e1 :+ _):tl) -> let ys = (e1 :+ s) : List.zipWith (\(_ :+ v) (e :+ _) -> e :+ v) xs tl
                          t  = (last xs)^.extra
                      in Alternating t (List.reverse ys)
