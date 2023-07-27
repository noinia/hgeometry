--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.NonEmpty.Util
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.NonEmpty.Util
  ( extractMinimaBy
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Ext

--------------------------------------------------------------------------------

-- | Extracts all minima from the list. The result consists of the
-- list of minima, and all remaining points. Both lists are returned
-- in the order in which they occur in the input.
--
-- >>> extractMinimaBy compare $ NonEmpty.fromList [1,2,3,0,1,2,3,0,1,2,0,2]
-- 0 :| [0,0] :+ [2,3,1,2,3,1,2,1,2]
extractMinimaBy             :: (a -> a -> Ordering) -> NonEmpty a -> NonEmpty a :+ [a]
extractMinimaBy cmp (x:|xs) = foldr (\y (mins@(m:|_) :+ rest) -> case m `cmp` y of
                                        LT -> mins :+ y:rest
                                        EQ -> (y NonEmpty.<| mins) :+ rest
                                        GT -> (y:|[]) :+ NonEmpty.toList mins <> rest
                                    ) ((x:|[]) :+ []) xs
  -- TODO: This is actually a good scenario for testing how much slower :+ is compared
  -- to doing nothing. i..e compare minimaBy and extractMinimaBy
  -- note that I'm using foldr here, and foldl' before
