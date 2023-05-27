module HGeometry.LowerEnvelope.AtMostThree
  ( AtMostThree(..)
  , commonElems
  ) where

import qualified Data.Set as Set
import           HGeometry.Vector
import           Witherable
import qualified Data.Foldable as F

--------------------------------------------------------------------------------

-- | Lists of length at most three
data AtMostThree a = None
                   | One !a
                   | Two !a !a
                   | Three !a !a !a
                   deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

instance Filterable AtMostThree where
  mapMaybe f = \case
      None        -> None
      One x       -> case f x of
                       Nothing -> None
                       Just x' -> One x'
      Two x y     -> case f x of
                       Nothing -> None
                       Just x' -> case f y of
                                    Nothing -> One x'
                                    Just y' -> Two x' y'
      Three x y z -> case f x of
                       Nothing -> None
                       Just x' -> case f y of
                                    Nothing -> One x'
                                    Just y' -> case f z of
                                                 Nothing -> Two x' y'
                                                 Just z' -> Three x' y' z'

instance Witherable AtMostThree

-- | Compute the common elements of two vectors of length 3
commonElems     :: Ord a
                => Vector 3 a
                -> Vector 3 a
                -> AtMostThree a
commonElems u v = case Set.toAscList $ Set.intersection (Set.fromList $ F.toList u)
                                                        (Set.fromList $ F.toList v) of
                    []      -> None
                    [x]     -> One x
                    [x,y]   -> Two x y
                    [x,y,z] -> Three x y z
                    _       -> error "commonElems: absurd"
