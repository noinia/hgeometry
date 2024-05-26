module HGeometry.LowerEnvelope.DivideAndConquer
  ( lowerEnvelope
  ) where

import           Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as Map
import           Data.Word
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.LowerEnvelope.AdjListForm
import           HGeometry.LowerEnvelope.EpsApproximation
import qualified HGeometry.LowerEnvelope.Naive as Naive
import           HGeometry.LowerEnvelope.Sample
import           HGeometry.LowerEnvelope.Type
import           HGeometry.LowerEnvelope.VertexForm
import           HGeometry.Point
import           HGeometry.Properties
import           Witherable

--------------------------------------------------------------------------------

-- | below this value we use the naive algorithm.
nZero :: Int
nZero = 10

eps :: Rational
eps = 1/8 -- TODO figure out what this had to be again.

--------------------------------------------------------------------------------

-- | divide and conquer algorithm
--
-- running time: \(O(n \log n)\)
lowerEnvelope    :: ( Plane_ plane r
                    , Ord r, Fractional r, Foldable f, Functor f, Ord plane
                    , Show plane, Show r
                    ) => f plane -> LowerEnvelope plane
lowerEnvelope = fromVertexForm . lowerEnvelopeVertexForm

-- FIXME: make sure not all planes are parallel first, otherwise the triangulatedEnvelope part is kind of weird.

-- | Compute the vertices of the lower envelope
--
--
-- running time: \(O(n \log n)\)
lowerEnvelopeVertexForm    :: forall f plane r.
                              ( Plane_ plane r
                              , Ord r, Fractional r, Foldable f, Ord plane
                              ) => f plane -> VertexForm plane
lowerEnvelopeVertexForm hs
    | n <= nZero = Naive.lowerEnvelopeVertexForm hs
    | otherwise  = undefined
  where
    r = undefined
    s = undefined

    as  = epsApproximation r hs
    env = triangulatedLowerEnvelope as


    conflictLists' = computeConflictLists env hs

    superCells = formSuperCells s env

    conflictLists = undefined --- combineConlictLists




    -- do ss            <- sample p hs
    --                 (env, prisms) <- computePrisms hs ss
    --                 subEnvs       <- mapM (over conflictList lowerEnvelope) prisms
    --                 merge env subEnvs
    n = length hs
    -- s = n ^^^ (1-eps)
    -- p = probability s n


type TriangulatedLowerEnvelope plane = LowerEnvelope' plane

triangulatedLowerEnvelope :: ( Plane_ plane r
                             , Ord r, Fractional r, Foldable f, Functor f, Ord plane
                             , Show plane, Show r
                             ) => f plane -> TriangulatedLowerEnvelope plane
triangulatedLowerEnvelope = undefined


type SuperCell plane = SuperCell' (NumType plane) plane
data SuperCell' r plane = SuperCell
                          -- { boundary :: SimplePolygon (Point 2 r)
                          -- , internalVertices :: [BoundedVertex plane]
                          -- }



formSuperCells :: Int -- ^ the number of triangles s in each super cell
               -> TriangulatedLowerEnvelope plane
               -> NonEmpty (SuperCell plane)
formSuperCells = undefined


-- newtype VerticesWithConflictLists

newtype Vertex' plane = Vertex' (Point 3 (NumType plane))


computeConflictLists :: TriangulatedLowerEnvelope plane
                     -> f plane
                     -> Map.Map (Vertex' plane) (f plane)
computeConflictLists = undefined


mergeConflictLists :: Map.Map (Vertex' plane) (f plane)
mergeConflictLists = undefined
