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

type instance VectorFamily 2 Int    = Int.Unpacked.Vec2
type instance VectorFamily 3 Int    = Int.Unpacked.Vec3
type instance VectorFamily 4 Int    = Int.Unpacked.Vec4
-- this is unfortunately a bit silly :( )
type instance VectorFamily  5 Int = Int.Unpacked.VecLarge 5
type instance VectorFamily  6 Int = Int.Unpacked.VecLarge 6
type instance VectorFamily  7 Int = Int.Unpacked.VecLarge 7
type instance VectorFamily  8 Int = Int.Unpacked.VecLarge 8
type instance VectorFamily  9 Int = Int.Unpacked.VecLarge 9
type instance VectorFamily 10 Int = Int.Unpacked.VecLarge 10
type instance VectorFamily 11 Int = Int.Unpacked.VecLarge 11
type instance VectorFamily 12 Int = Int.Unpacked.VecLarge 12
type instance VectorFamily 13 Int = Int.Unpacked.VecLarge 13
type instance VectorFamily 14 Int = Int.Unpacked.VecLarge 14
type instance VectorFamily 15 Int = Int.Unpacked.VecLarge 15

type instance VectorFamily 2  Double = Double.Unpacked.Vec2
type instance VectorFamily 3  Double = Double.Unpacked.Vec3
type instance VectorFamily 4  Double = Double.Unpacked.Vec4
type instance VectorFamily 5  Double = Double.Unpacked.VecLarge 5
type instance VectorFamily 6  Double = Double.Unpacked.VecLarge 6
type instance VectorFamily 7  Double = Double.Unpacked.VecLarge 7
type instance VectorFamily 8  Double = Double.Unpacked.VecLarge 8
type instance VectorFamily 9  Double = Double.Unpacked.VecLarge 9
type instance VectorFamily 10 Double = Double.Unpacked.VecLarge 10
type instance VectorFamily 11 Double = Double.Unpacked.VecLarge 11
type instance VectorFamily 12 Double = Double.Unpacked.VecLarge 12
type instance VectorFamily 13 Double = Double.Unpacked.VecLarge 13
type instance VectorFamily 14 Double = Double.Unpacked.VecLarge 14
type instance VectorFamily 15 Double = Double.Unpacked.VecLarge 15

type instance VectorFamily 2  Float = Float.Unpacked.Vec2
type instance VectorFamily 3  Float = Float.Unpacked.Vec3
type instance VectorFamily 4  Float = Float.Unpacked.Vec4
type instance VectorFamily 5  Float = Float.Unpacked.VecLarge 5
type instance VectorFamily 6  Float = Float.Unpacked.VecLarge 6
type instance VectorFamily 7  Float = Float.Unpacked.VecLarge 7
type instance VectorFamily 8  Float = Float.Unpacked.VecLarge 8
type instance VectorFamily 9  Float = Float.Unpacked.VecLarge 9
type instance VectorFamily 10 Float = Float.Unpacked.VecLarge 10
type instance VectorFamily 11 Float = Float.Unpacked.VecLarge 11
type instance VectorFamily 12 Float = Float.Unpacked.VecLarge 12
type instance VectorFamily 13 Float = Float.Unpacked.VecLarge 13
type instance VectorFamily 14 Float = Float.Unpacked.VecLarge 14
type instance VectorFamily 15 Float = Float.Unpacked.VecLarge 15

type instance VectorFamily d ()             = Boxed.Vector d () -- not sure about this one...
type instance VectorFamily d Integer        = Boxed.Vector d Integer
type instance VectorFamily d Rational       = Boxed.Vector d Rational

type instance VectorFamily d [r]            = Boxed.Vector d [r]
type instance VectorFamily d (Maybe r)      = Boxed.Vector d (Maybe r)
type instance VectorFamily d (RealNumber n) = Boxed.Vector d (RealNumber n)
type instance VectorFamily d (Vector d' r)  = Boxed.Vector d (Vector d' r)

type instance VectorFamily d (a,b)          = Boxed.Vector d (a,b)
type instance VectorFamily d (a,b,c)        = Boxed.Vector d (a,b,c)
type instance VectorFamily d (a,b,c,e)      = Boxed.Vector d (a,b,c,e)
type instance VectorFamily d (a,b,c,e,f)    = Boxed.Vector d (a,b,c,e,f)
type instance VectorFamily d (a,b,c,e,f,g)  = Boxed.Vector d (a,b,c,e,f,g)
