module Data.Geometry.QuadTree.Quadrants( pattern Quadrants
                                       , Quadrants
                                       , module Data.Geometry.Box.Corners
                                       ) where

import           Data.Geometry.Box.Corners

--------------------------------------------------------------------------------

type Quadrants = Corners

pattern Quadrants         :: a -> a -> a -> a -> Corners a
pattern Quadrants a b c d = Corners a b c d
{-# COMPLETE Quadrants #-}

--------------------------------------------------------------------------------
