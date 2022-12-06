--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Vector.Optimal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- An implementation of d-dimensional vectors that tries to pick an
-- optimzied representation.
--
--------------------------------------------------------------------------------
module Optimal
  ( module Optimal.Internal
  -- * Helper with which we can easily implement Optimal vectors for newtypes
  , WrapVector(..)
  ) where

import qualified Boxed
import           Data.RealNumber.Rational
import qualified Double.Unpacked
import qualified Float.Unpacked
import qualified Int.Unpacked
import           Optimal.Internal
import           Optimal.Wrap

--------------------------------------------------------------------------------

-- type instance VectorFamily 2 Int = V2.V2 Int

type instance VectorFamily 2 Int    = Int.Unpacked.Vec2
type instance VectorFamily 3 Int    = Int.Unpacked.Vec3
type instance VectorFamily 4 Int    = Int.Unpacked.Vec4

type instance VectorFamily 2 Double = Double.Unpacked.Vec2
type instance VectorFamily 3 Double = Double.Unpacked.Vec3
type instance VectorFamily 4 Double = Double.Unpacked.Vec4

type instance VectorFamily 2 Float  = Float.Unpacked.Vec2
type instance VectorFamily 3 Float  = Float.Unpacked.Vec3
type instance VectorFamily 4 Float  = Float.Unpacked.Vec4

type instance VectorFamily d Integer        = Boxed.Vector d Integer
type instance VectorFamily d Rational       = Boxed.Vector d Rational
type instance VectorFamily d (RealNumber n) = Boxed.Vector d (RealNumber n)
type instance VectorFamily d (Vector d' r)  = Boxed.Vector d (Vector d' r)

-- type instance VectorFamily 2 (RealNumber n) = Boxed.Vector 2 (RealNumber n)
-- type instance VectorFamily 3 (RealNumber n) = Boxed.Vector 3 (RealNumber n)
-- type instance VectorFamily 4 (RealNumber n) = Boxed.Vector 4 (RealNumber n)
-- type instance VectorFamily 5 (RealNumber n) = Boxed.Vector 5 (RealNumber n)
-- type instance VectorFamily 6 (RealNumber n) = Boxed.Vector 6 (RealNumber n)
-- type instance VectorFamily 7 (RealNumber n) = Boxed.Vector 7 (RealNumber n)
-- type instance VectorFamily 8 (RealNumber n) = Boxed.Vector 8 (RealNumber n)
-- type instance VectorFamily 9 (RealNumber n) = Boxed.Vector 9 (RealNumber n)


-- type instance VectorFamily 2 (Vector d' r) = Boxed.Vector 2 (Vector d' r)
-- type instance VectorFamily 3 (Vector d' r) = Boxed.Vector 3 (Vector d' r)
-- type instance VectorFamily 4 (Vector d' r) = Boxed.Vector 4 (Vector d' r)
-- type instance VectorFamily 5 (Vector d' r) = Boxed.Vector 5 (Vector d' r)
-- type instance VectorFamily 6 (Vector d' r) = Boxed.Vector 6 (Vector d' r)
-- type instance VectorFamily 7 (Vector d' r) = Boxed.Vector 7 (Vector d' r)
