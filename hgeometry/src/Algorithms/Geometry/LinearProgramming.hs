--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.LinearProgramming
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Algorithms for Linear programming
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.LinearProgramming
  ( LinearProgram(LinearProgram)
  , objective, constraints

  , solveBoundedLinearProgram
  , solveBoundedLinearProgram'

  , maximumOn
  , oneDLinearProgramming
  , commonIntersection
  , cmpHalfPlane

  ) where

import Algorithms.Geometry.LinearProgramming.LP2DRIC
import Algorithms.Geometry.LinearProgramming.Types
