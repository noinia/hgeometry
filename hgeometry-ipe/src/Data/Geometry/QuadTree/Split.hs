{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.QuadTree.Split where

import Control.Lens (makePrisms,(^.))
import Data.Geometry.QuadTree.Cell
import Data.Geometry.QuadTree.Quadrants

--------------------------------------------------------------------------------

-- | Data Type to Decide if we should continue splitting the current cell
data Split i v p = No !p | Yes !v (Quadrants i) deriving (Show,Eq,Ord)
makePrisms ''Split


-- | Split only when the Cell-width is at least wMin
limitWidthTo        :: WidthIndex -- ^ smallest allowed width of a cell (i.e. width of a leaf)
                    -> (Cell r -> pts -> Split pts v p)
                    -> (Cell r -> pts -> Split pts v (Either pts p))
limitWidthTo wMin f = \c pts -> case f c pts of
                                  No p                                -> No (Right p)
                                  Yes v qs | wMin < c^.cellWidthIndex -> Yes v qs
                                           | otherwise                -> No (Left pts)
  -- note that it is important that we still evaluate the function so
  -- that we can distinguish at the last level i.e. between a regular
  -- " we are done splitting (No (Right p))" and a "we are no longer
  -- allowed to split further (No (Left p))"
