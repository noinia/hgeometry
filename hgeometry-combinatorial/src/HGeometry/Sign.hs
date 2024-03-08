--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Sign
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Signs of expressions
--------------------------------------------------------------------------------
module HGeometry.Sign where

import Data.Foldable1
import Data.Monoid

--------------------------------------------------------------------------------

-- | The sign of an expression
data Sign = Negative | Positive deriving (Show,Eq,Ord,Enum,Bounded)

-- | Flip Positive <=> Negative.
flipSign :: Sign -> Sign
flipSign = \case
  Negative -> Positive
  Positive -> Negative

--------------------------------------------------------------------------------

-- | Given the terms, in decreasing order of significance, computes the sign
--
-- i.e. expects a list of terms, we base the sign on the sign of the first non-zero term.
signFromTerms :: (Num r, Eq r, Foldable1 f) => f r -> Maybe Sign
signFromTerms = getFirst . foldMap (First . signum')
  where
    signum' x = case signum x of
                  -1 -> Just Negative
                  0  -> Nothing
                  1  -> Just Positive
                  _  -> error "signum': absurd"
