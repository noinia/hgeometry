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
  ( Report(Report)
  ) where

import qualified Data.List as List
import           HGeometry.Measured.Class

--------------------------------------------------------------------------------


-- | Type to represent reporting elements
newtype Report a = Report [a]
  deriving (Show,Eq,Ord,Semigroup,Monoid)

instance Measured Report a where
  measure = Report . (:[])

instance CanInsert Report a where
  insertMeasure x (Report xs) = Report (x:xs)

instance Eq a => CanDelete Report a where
  -- | Removes the first occurance of the element
  deleteMeasure x (Report xs) = Just . Report $ List.delete x xs
