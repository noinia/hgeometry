{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.VertexForm
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Representation of the Lower envelope of planes in vertex form;
-- i.e. storing only the vertices.
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.VertexForm
  ( VertexForm(VertexForm)
  , hasVertices, vertices'
  , singleton
  , LEVertex, pattern LEVertex, Definers
  , BoundedVertexF(Vertex)
  , location, definers, location2

  , intersectionLine
  , intersectionPoint
  ) where

import           Control.Lens
import qualified Data.Map as Map
import qualified Data.Set as Set
import           HGeometry.Combinatorial.Util
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Plane.LowerEnvelope.Type
import           HGeometry.Point
import           HGeometry.Properties
import           Hiraffe.Graph

--------------------------------------------------------------------------------

-- | Vertices of a lower envelope in vertex form.
type LEVertex = BoundedVertexF (Const ())

-- | Convenience Constructor for computing vertices of the lower
-- envelope that are in Vertex form.
pattern LEVertex        :: Point 3 (NumType plane) -> Definers plane -> LEVertex plane
pattern LEVertex v defs = Vertex v defs (Const ())
{-# COMPLETE LEVertex #-}

-- | The definers of a vertex.
type Definers plane = Set.Set plane

-- | The lower envelope in vertex form
newtype VertexForm plane =
  VertexForm (Map.Map (Point 3 (NumType plane))
                      (Definers plane)
             )

-- | Iso to access the underlying Map.
_VertexFormMap :: Iso (VertexForm plane) (VertexForm plane')
                      (Map.Map (Point 3 (NumType plane))  (Definers plane))
                      (Map.Map (Point 3 (NumType plane')) (Definers plane'))
_VertexFormMap = coerced

-- | Test if there are vertices
hasVertices :: VertexForm plane -> Bool
hasVertices = not . Map.null . view _VertexFormMap

deriving instance ( Show plane, Show (NumType plane)
                  ) => Show (VertexForm plane)
deriving instance ( Eq plane
                  , Eq (NumType plane)
                  ) => Eq   (VertexForm plane)

-- | Computes a lower envelope consisting of a single vertex.
singleton   :: LEVertex plane -> VertexForm plane
singleton v = VertexForm $ Map.singleton (v^.location) (v^.definers)

instance (Ord (NumType plane), Ord plane) => Semigroup (VertexForm plane) where
  (VertexForm m) <> (VertexForm m') = VertexForm $ Map.unionWith (<>) m m'
instance (Ord (NumType plane), Ord plane) => Monoid (VertexForm plane) where
  mempty = VertexForm mempty

instance Ord (NumType plane) => HasVertices' (VertexForm plane) where
  type Vertex   (VertexForm plane) = Definers plane
  type VertexIx (VertexForm plane) = Point 3 (NumType plane)
  vertexAt i = _VertexFormMap . iix i
  numVertices (VertexForm m) = Map.size m

-- instance Ord (NumType plane) => HasVertices (VertexForm plane) (VertexForm plane) where
--   vertices = _VertexFormMap . itraversed

-- | Traversal (rather than a traversal1) of the vertices.
vertices' :: IndexedTraversal' (VertexIx (VertexForm plane))
                                      (VertexForm plane) (Vertex (VertexForm plane))
vertices' = _VertexFormMap . itraversed

--------------------------------------------------------------------------------


-- | Given two planes, computes the line in which they intersect.
intersectionLine :: (Plane_ plane r, Fractional r, Eq r)
                 => plane -> plane -> Maybe (VerticalOrLineEQ r)
intersectionLine (Plane_ a1 b1 c1) (Plane_ a2 b2 c2)
    | b1 /= b2  = Just $ NonVertical $ LineEQ ((a2 - a1) / diffB) ((c2 - c1) / diffB)
                  -- the two planes intersect in some normal line
    | a1 /= a2  = Just $ VerticalLineThrough ((c2 -c1) / (a1 - a2))
                  -- the planes intersect in a vertical line
    | otherwise = Nothing
                  -- the planes don't intersect at all
  where
    diffB = b1 - b2

-- | Computes there the three planes intersect
intersectionPoint                                    :: ( Plane_ plane r, Ord r, Fractional r)
                                                     => Three plane -> Maybe (Point 3 r)
intersectionPoint (Three h1@(Plane_ a1 b1 c1) h2 h3) =
    do l12 <- intersectionLine h1 h2
       l13 <- intersectionLine h1 h3
       case (l12,l13) of
         (VerticalLineThrough _x12, VerticalLineThrough _x13) -> Nothing
           -- if the xes are the same they would be the same plane even
         (VerticalLineThrough x, NonVertical l)               -> vertNonVertIntersect x l
         (NonVertical l, VerticalLineThrough x)               -> vertNonVertIntersect x l
         (NonVertical l, NonVertical m)                       -> l `intersect` m >>= \case
           Line_x_Line_Point (Point2 x y) -> Just $ Point3 x y (a1 * x + b1* y + c1)
           Line_x_Line_Line _             -> Nothing
   where
     vertNonVertIntersect x l = let y = evalAt' x l
                                    z = a1 * x + b1* y + c1
                                in Just $ Point3 x y z
