{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.QuadTree.Split where

import Control.Lens (makePrisms,(^.))
import Data.Geometry.QuadTree.Cell
import Data.Geometry.QuadTree.Quadrants

--------------------------------------------------------------------------------

-- | Data Type to Decide if we should continue splitting the current cell
data Split i v p = No !p | Yes !v (Quadrants i) deriving (Show,Eq,Ord)
makePrisms ''Split

-- | A splitter is a function that determines weather or not we should the given cell
-- corresponding to the given input (i).
type Splitter r i v p = Cell r -> i -> Split i v p

-- | Transformer that limits the depth of a splitter
type Limiter r i v p = Splitter r i v p
                    -> Splitter r i v (Either i p)

-- | Split only when the Cell-width is at least wMin
limitWidthTo        :: WidthIndex -- ^ smallest allowed width of a cell (i.e. width of a leaf)
                    -> Limiter r i v p
limitWidthTo wMin f = \c pts -> case f c pts of
                                  No p                                -> No (Right p)
                                  Yes v qs | wMin < c^.cellWidthIndex -> Yes v qs
                                           | otherwise                -> No (Left pts)
  -- note that it is important that we still evaluate the function so
  -- that we can distinguish at the last level i.e. between a regular
  -- " we are done splitting (No (Right p))" and a "we are no longer
  -- allowed to split further (No (Left p))"
