{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LinearProgramming.Types
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- 2D Linear programming in expected linear time.
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.LinearProgramming.Types where

import           Control.Lens
import           Data.Geometry.HalfSpace
import           Data.Geometry.HalfLine
import           Data.Geometry.Point
import           Data.Geometry.Vector

--------------------------------------------------------------------------------

-- | Data type representing the solution to a linear program
data LPSolution d r = NoSolution
                    | Single !(Point d r)
                    | UnBounded (HalfLine d r)
makePrisms ''LPSolution

deriving instance (Arity d, Show r)             => Show    (LPSolution d r)
deriving instance (Arity d, Eq r, Fractional r) => Eq      (LPSolution d r)

data LinearProgram d r = LinearProgram { _objective   :: !(Vector d r)
                                       , _constraints :: [HalfSpace d r]
                                       }
makeLenses ''LinearProgram

deriving instance Arity d                       => Functor (LinearProgram d)
deriving instance (Arity d, Show r)             => Show    (LinearProgram d r)
deriving instance (Arity d, Fractional r, Eq r) => Eq      (LinearProgram d r)
