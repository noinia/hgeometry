module Algorithms.Geometry.SoS.Sign where

import qualified Data.List as List
import           Data.Maybe

--------------------------------------------------------------------------------

-- | The sign of an expression
data Sign = Negative | Positive deriving (Show,Eq,Ord,Enum,Bounded)

flipSign :: Sign -> Sign
flipSign = \case
  Negative -> Positive
  Positive -> Negative

--------------------------------------------------------------------------------

-- | Given the terms, in decreasing order of significance, computes the sign
--
-- i.e. expects a list of terms, we base the sign on the sign of the first non-zero term.
--
-- pre: the list contains at least one such a term.
signFromTerms :: (Num r, Eq r) => [r] -> Sign
signFromTerms = List.head . mapMaybe signum'
  where
    signum' x = case signum x of
                  -1    -> Just Negative
                  0     -> Nothing
                  1     -> Just Positive
                  _     -> error "signum': absurd"
