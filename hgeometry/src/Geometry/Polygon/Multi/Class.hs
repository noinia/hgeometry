{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Polygon.Multi.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Multi polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module Geometry.Polygon.Multi.Class
  (

  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           Geometry.Point.Class
import           Geometry.Polygon.Class

--------------------------------------------------------------------------------

class ( Polygon_ multiPolygon point r
      ) => MultiPolygon_ multiPolygon point r where
