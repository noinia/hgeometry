module Util where

import Data.Vinyl
import Data.Geometry.Ipe
import Data.Proxy
import Data.Ext
import Data.Function(on)
import qualified Data.List as L
import Data.Singletons(Apply)

byStrokeColour :: (Stroke ∈ ats, Ord (Apply f Stroke))
               => [a :+ Attributes f ats] -> [[a :+ Attributes f ats]]
byStrokeColour = map (map fst) . L.groupBy ((==) `on` snd) . L.sortOn snd
               . map (\x -> (x,lookup' x))
  where
    lookup' (_ :+ ats) = lookupAttr (Proxy :: Proxy Stroke) ats

-- | Computes all elements on which the two lists differ
difference :: Eq a => [a] -> [a] -> [a]
difference xs ys = (xs L.\\ ys) ++ (ys L.\\ xs)

-- differenceBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]


diffBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
diffBy p xs ys = foldr (L.deleteBy p) ys xs

-- | \(O(n^2)\) set that ignores duplicates and order
newtype NaiveSet a = NaiveSet [a] deriving (Show)

instance Eq a => Eq (NaiveSet a) where
  (NaiveSet xs) == (NaiveSet ys) = L.null $ difference xs ys
