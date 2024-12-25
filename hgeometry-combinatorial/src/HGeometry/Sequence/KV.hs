--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Sequence.KV
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Sequences of key value pairs.
--
--------------------------------------------------------------------------------
module HGeometry.Sequence.KV
  ( KV(..)

  ) where

import Control.Lens

-----------------------------------------------------------------------------------------


-- | An 'f' of key value pairs
newtype KV f k v = KV (f (k,v))
