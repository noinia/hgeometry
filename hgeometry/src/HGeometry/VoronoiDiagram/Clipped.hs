{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.VoronoiDiagram.Clipped
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Representing a Voronoi Diagram of a set of points in R^2 that is clipped to some
-- triangle, this means all cells are bounded convex polygons. (The points may be outside
-- the triangle).
--
--------------------------------------------------------------------------------
module HGeometry.VoronoiDiagram.Clipped
  ( ClippedVoronoiDiagram
  , _ClippedVoronoiDiagram

  , voronoiDiagramIn
  , voronoiDiagramInWith
  ) where

import Control.Lens
import Control.Subcategory.Functor
import Data.Foldable1
import Data.Map.NonEmpty (NEMap)
import HGeometry.Ext
import HGeometry.HyperPlane
import HGeometry.Plane.LowerEnvelope.Clipped
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Triangle
import HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane)
import Control.DeepSeq

--------------------------------------------------------------------------------

-- | A Voronoi diagram clipped to a triangle
newtype ClippedVoronoiDiagram point = ClippedVoronoiDiagram (ClippedMinimizationDiagram point)


deriving instance (Show r, Num r, Show point, r ~ NumType point
                  ) => Show (ClippedVoronoiDiagram point)

deriving instance (NFData point, NFData (NumType point)
                  ) => NFData (ClippedVoronoiDiagram point)


-- | Get access to the underling nonEmpty Map
_ClippedVoronoiDiagram :: (NumType point ~ r)
                       => Iso' (ClippedVoronoiDiagram point)
                               (NEMap point (ClippedMDCell r point ()))
_ClippedVoronoiDiagram = coerced . _ClippedMinimizationDiagramMap
{-# INLINE _ClippedVoronoiDiagram #-}

-- instance Constrained ClippedVoronoiDiagram where
--   type Dom ClippedVoronoiDiagram point = Ord point

-- instance CFunctor ClippedVoronoiDiagram where
--   cmap f (ClippedVoronoiDiagram m) = ClippedVoronoiDiagram $ cmap f m
-- -- not sure why I can't derive this


type instance NumType   (ClippedVoronoiDiagram point) = NumType point
type instance Dimension (ClippedVoronoiDiagram point) = 2



-- | Computes the Voronoi inside the given triangle.
--
-- O(n^4) (for now).
voronoiDiagramIn :: ( Point_ point 2 r, Point_ corner 2 r, Functor nonEmpty, Ord point
                    , Ord r, Fractional r, Foldable1 nonEmpty

                    , Show r, Show corner, Show point-- TODO: remove these
                    )
                 => Triangle corner -> nonEmpty point -> ClippedVoronoiDiagram point
voronoiDiagramIn = voronoiDiagramInWith lowerEnvelopeIn

-- | Given a function to compute a lower envelope; construct use it to construct the
-- Voronoi diagram inside the given triangle.
voronoiDiagramInWith :: ( Point_ point 2 r, Functor nonEmpty, Ord point
                        , Ord r, Fractional r, Foldable1 nonEmpty
                        )
                     => (Triangle corner -> nonEmpty (Plane r :+ point) -> ClippedMinimizationDiagram (Plane r :+ point))
                     -> Triangle corner
                     -> nonEmpty point
                     -> ClippedVoronoiDiagram point
voronoiDiagramInWith lowerEnv tri = ClippedVoronoiDiagram
                                  . cmap (^.extra) . lowerEnv tri . fmap (\p -> pointToPlane p :+ p)
