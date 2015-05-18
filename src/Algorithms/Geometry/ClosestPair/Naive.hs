module Algorithms.Geometry.ClosestPair.Naive where

import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Point
import           Data.Geometry.Vector(Arity)
import           Data.Geometry(qdA)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import           Data.Semigroup
import qualified Data.Vector.Fixed as FV


-- | Two a's
data Two a = Two !a !a deriving (Show,Read,Eq,Ord)

-- | A pair of points
type PP d p r = ArgMin r (Two (Point d r :+ p))

-- | Create a pair of points
mkPair                         :: (Arity d, Num r)
                               => Point d r :+ p -> Point d r :+ p -> PP d p r
mkPair pp@(p :+ _) qq@(q :+ _) = let dst = qdA p q
                                 in Min (Arg dst (Two pp qq))


-- | Naive algorithm to compute the closest pair in d dimensions. Runs in
-- O(n^2) time (for any constant d). Note that we need at least two elements
-- for there to be a closest pair.
closestPair :: ( FV.Dim v ~ FV.S (FV.S n)
               , FV.Vector v (Point d r :+ p)
               , Ord r, Arity d, Num r
               ) => v (Point d r :+ p) -> Two (Point d r :+ p)
closestPair = getVal . getMin . sconcat . fmap (uncurry' mkPair) . pairs
  where
    uncurry' f (Two a b) = f a b
    getVal (Arg _ x) = x

-- | Produce all lists from a vec of elements. Since the Vec contains at least two
-- elements, the resulting list is non-empty
pairs :: (FV.Dim v ~ FV.S (FV.S n), FV.Vector v a) => v a -> NE.NonEmpty (Two a)
pairs = NE.fromList . concatMap f . L.inits . FV.toList
  where
    f []     = []
    f (x:xs) = [Two x y | y <- xs]


getVal (Arg _ x) = x
