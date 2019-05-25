{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.PlanarGraph.Dart
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing Darts (edges) in a planar graph.
--------------------------------------------------------------------------------
module Data.PlanarGraph.Dart where

import Control.DeepSeq
import Control.Lens hiding ((.=))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary(..),suchThat)

-- $setup
-- >>> :{
-- let dart i s = Dart (Arc i) (read s)
-- :}

--------------------------------------------------------------------------------

-- | An Arc is a directed edge in a planar graph. The type s is used to tie
-- this arc to a particular graph.
newtype Arc s = Arc { _unArc :: Int } deriving (Eq,Ord,Enum,Bounded,Generic,NFData)

instance Show (Arc s) where
  show (Arc i) = "Arc " ++ show i

instance Arbitrary (Arc s) where
  arbitrary = Arc <$> (arbitrary `suchThat` (>= 0))


-- | Darts have a direction which is either Positive or Negative (shown as +1
-- or -1, respectively).
data Direction = Negative | Positive deriving (Eq,Ord,Bounded,Enum,Generic)

instance NFData Direction

instance Show Direction where
  show Positive = "+1"
  show Negative = "-1"

instance Read Direction where
  readsPrec _ "-1" = [(Negative,"")]
  readsPrec _ "+1" = [(Positive,"")]
  readsPrec _ _    = []

instance Arbitrary Direction where
  arbitrary = (\b -> if b then Positive else Negative) <$> arbitrary

-- | Reverse the direcion
rev          :: Direction -> Direction
rev Negative = Positive
rev Positive = Negative

-- | A dart represents a bi-directed edge. I.e. a dart has a direction, however
-- the dart of the oposite direction is always present in the planar graph as
-- well.
data Dart s = Dart { _arc       :: !(Arc s)
                   , _direction :: !Direction
                   } deriving (Eq,Ord,Generic)
makeLenses ''Dart

instance NFData (Dart s)

instance Show (Dart s) where
  show (Dart a d) = "Dart (" ++ show a ++ ") " ++ show d

instance Arbitrary (Dart s) where
  arbitrary = Dart <$> arbitrary <*> arbitrary

-- | Get the twin of this dart (edge)
--
-- >>> twin (dart 0 "+1")
-- Dart (Arc 0) -1
-- >>> twin (dart 0 "-1")
-- Dart (Arc 0) +1
twin            :: Dart s -> Dart s
twin (Dart a d) = Dart a (rev d)

-- | test if a dart is Positive
isPositive   :: Dart s -> Bool
isPositive d = d^.direction == Positive


instance Enum (Dart s) where
  toEnum x
    | even x    = Dart (Arc $ x `div` 2) Positive
    | otherwise = Dart (Arc $ x `div` 2) Negative
  -- get the back edge by adding one

  fromEnum (Dart (Arc i) d) = case d of
                                Positive -> 2*i
                                Negative -> 2*i + 1


-- | Enumerates all darts such that
-- allDarts !! i = d   <=> i == fromEnum d
allDarts :: [Dart s]
allDarts = concatMap (\a -> [Dart a Positive, Dart a Negative]) [Arc 0..]
