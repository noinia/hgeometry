--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Measured.Report
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
--
--------------------------------------------------------------------------------
module HGeometry.Measured.Report
  ( Report(Report, MkReport)
  , Report1(Report1)
  , ReportList(ReportList)
  ) where

import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Functor.Const
import qualified Data.List as List
import           HGeometry.Measured.Class
import           HGeometry.Tree.Binary.Static

--------------------------------------------------------------------------------

-- | Report is a type to be used for reporting/collecting all results of a query.
--
-- The result is internally represented as a binary tree, so that we can guarantee O(1)
-- time and space (<>).
newtype Report a = MkReport (Maybe (Report1 a))
  deriving (Show,Eq,Ord,Functor,Foldable,Traversable)

-- | Get the results of a Report as a list.
pattern Report    :: [a] -> Report a
pattern Report xs <- (F.toList -> xs)
{-# COMPLETE Report #-}

instance Semigroup (Report a) where
  (MkReport m) <> (MkReport m') = MkReport $ case (m,m') of
    (Nothing, Nothing) -> Nothing
    (Nothing, Just _)  -> m'
    (Just _, Nothing)  -> m
    (Just l, Just r)   -> Just $ l <> r

instance Monoid (Report a) where
  mempty = MkReport Nothing

instance Measured Report a where
  measure = MkReport . Just . measure

instance CanInsert Report a where
  insertMeasure x (MkReport m) = case m of
                                   Nothing -> measure x
                                   Just xs -> MkReport . Just $ insertMeasure x xs

--------------------------------------------------------------------------------

-- | Type to represent reporting elements. The result is represented as a binary tree so
-- that we can support O(1) time and space (<>)
newtype Report1 a = Report1 (BinLeafTree (Const () a) a)
  deriving (Show,Eq,Ord)

instance Semigroup (Report1 a) where
  (Report1 l) <> (Report1 r) = Report1 $ Node l mempty r

instance Functor Report1 where
  fmap f (Report1 t) = Report1 . coerce $ fmap f t

instance Foldable Report1 where
  foldMap f (Report1 t) = foldMap f t

instance Traversable Report1 where
  traverse f (Report1 t) = Report1 . coerce <$> traverse f t

instance Measured Report1 a where
  measure = Report1 . Leaf

instance CanInsert Report1 a where
  insertMeasure x (Report1 xs) = Report1 $ Node (Leaf x) mempty xs


--------------------------------------------------------------------------------

-- | Report is a type to be used for reporting/collecting all results of a query. The
-- result is directly represented as a list.
newtype ReportList a = ReportList [a]
  deriving (Show,Eq,Ord,Semigroup,Monoid,Functor,Foldable,Traversable)

instance Measured ReportList a where
  measure = ReportList . (:[])

instance CanInsert ReportList a where
  insertMeasure x (ReportList xs) = ReportList (x:xs)

instance Eq a => CanDelete ReportList a where
  -- | Removes the first occurance of the element
  deleteMeasure x (ReportList xs) = Just . ReportList $ List.delete x xs
