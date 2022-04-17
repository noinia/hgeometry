--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.QuadTree.Quadrants
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Geometry.QuadTree.Quadrants( pattern Quadrants
                                       , Quadrants
                                       , module Geometry.Box.Corners
                                       ) where

import           Geometry.Box.Corners

--------------------------------------------------------------------------------

type Quadrants = Corners

pattern Quadrants         :: a -> a -> a -> a -> Corners a
pattern Quadrants a b c d = Corners a b c d
{-# COMPLETE Quadrants #-}

--------------------------------------------------------------------------------
