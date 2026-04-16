{-# LANGUAGE TemplateHaskell  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Slab
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A data type to represent slabs; i.e. regions bounded by two parallel lines.
--
--------------------------------------------------------------------------------
module HGeometry.Slab
  ( Slab(Slab), definingLine, squaredWidth, leftData, rightData
  , fromParalelHalfplanes
  , leftBoundary, rightBoundary
  ) where

import HGeometry.Properties (NumType,Dimension)
import HGeometry.Line.PointAndVector
import Control.Lens
import HGeometry.Point
import HGeometry.Vector
import HGeometry.HalfSpace.Class
import HGeometry.Intersection
import HGeometry.Ext
import HGeometry.Number.Radical
import Prelude hiding (sqrt)

--------------------------------------------------------------------------------

-- | A data type representing a slab.
data Slab r side = Slab { _definingLine        :: !(LinePV 2 r)
                        -- ^ The oriented line that defines the slab. This
                        -- is the left bounding line of the slab; i.e.
                        -- the interior of the slab is to its right
                        , _squaredWidth  :: !r
                        -- ^ the squared width of the slab
                        , _leftData  :: side
                        -- ^ data associated with the left bounding line
                        , _rightData :: side
                        -- ^ data associated with the right bounding line
                        }
                 deriving stock (Show,Eq,Functor,Foldable)

makeLenses ''Slab

type instance NumType   (Slab r side) = r
type instance Dimension (Slab r side) = 2


--------------------------------------------------------------------------------

-- | Create a slab out of two halfplanes whose bounding lines are parallel.
--
fromParalelHalfplanes       :: ( HalfPlane_ halfPlane r, Num r, Fractional r
                               , HasIntersectionWith (Point 2 r) halfPlane
                               , HasSupportingLine (BoundingHyperPlane halfPlane 2 r)
                               )
                            => halfPlane -> halfPlane -> Slab r halfPlane
fromParalelHalfplanes h1 h2
    | h1IsLeftHalfPlane     = Slab l1 dist h1 h2
    | otherwise             = Slab l2 dist h2 h1
  where
    l1@(LinePV a (Vector2 x y)) = h1^.boundingHyperPlane.to supportingLine
    l2                          = h2^.boundingHyperPlane.to supportingLine

    h1IsLeftHalfPlane = let  w = Vector2 (-y) x in (a .+^ w) `intersects` h1
    dist = squaredEuclideanDistTo a l2


-- | Get the left boundary of the slab
leftBoundary   :: Slab r side -> LinePV 2 r :+ side
leftBoundary s = let l = s^.definingLine in l :+ s^.leftData

-- | Get the right boundary of the slab
rightBoundary   :: (Fractional r, Radical r) => Slab r side -> LinePV 2 r :+ side
rightBoundary s = let (LinePV p v@(Vector2 x y)) = s^.definingLine
                      n                          = signorm $ Vector2 y (-x)
                      dist                       = sqrt $ s^.squaredWidth
                      p'                         = p .+^ (dist *^ n)
                        --
                  in (LinePV p' v) :+ s^.rightData
