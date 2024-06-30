module HGeometry.LowerEnvelope.DivideAndConquer
  ( lowerEnvelope
  ) where

import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Word
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line.PointAndVector
import qualified HGeometry.Line.LowerEnvelope as LowerEnvelope
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
                    , Ord r, Fractional r, Foldable1 f, Functor f, Ord plane
                    , Show plane, Show r
                    ) => f plane -> LowerEnvelope plane
lowerEnvelope hs = case verifyNotAllColinear hs of
    JustOnePlane h  -> ParallelStrips $ Set.singleton (ParallelPlane h)
    AllColinear v   -> let lines = fmap (toLine v) hs
                           env   = LowerEnvelope.lowerEnvelope lines
                       in ParallelStrips $ fromEnv env
    NonDegenerate _ -> fromVertexForm . lowerEnvelopeVertexForm $ hs


toLine :: Vector 2 r -> plane -> General r :+ plane
toLine undefined

fromEnv :: LowerEnvelopeF g (Point 2 r) (General r :+ plane) -> Set.Set (ParallelPlane plane)
fromEnv = undefined


-- | Helper type that describes whether the planes are degenerate or not
data VerifyDegenerate plane r = JustOnePlane plane
                              | AllColinear (Vector 2 r)
                              -- ^ All edges of the lower envelope are parallel to
                              -- this direction.
                              | NonDegenerate (Vector 3 plane)
                              -- ^ three non-degenrate planes prove that we will have a vertex
                              deriving (Show,Eq)

-- | Verifies that not all planes are colinear.
verifyNotAllColinear    :: ( Plane_ plane r
                           , Ord r, Fractional r, Foldable1 f, Ord plane
                           , Show plane, Show r
                           ) => f plane -> VerifyDegenrate plane (NumType plane)
verifyNotAllColinear hs = findOnParalellPlanes (toNonEmpty hs)
  where
    -- try to find two non-parallel planes h1 h2
    findOnParalellPlanes (h1 :| rest) = case NonEmpty.nonEmpty rest of
      Nothing            -> JustOnePlane h1
      Just (h2 :| rest') -> case direction h1 h2 of
        Left h' -> findOnParalellPlanes (h' :| rest')
        Right l -> findThirdPlane h1 h2 l rest -- find a third plane
  where
    -- | Tries to compute the vector of the line in which the two planes intersect,
    -- returns this vector (if the intersection line exists), or the lowest plane if one
    -- is always below the other.
    direction h1 h2 = case intersectionLine h1 h2 of
      Nothing | evalAt' origin h1 <= evalAt' origin h2 -> Left h1
              | otherwise                              -> Left h2
      Just l                                           -> Right $ toPV (l^.direction)

    findThirdPlane h1 h2 l = go
      where
        go = \case
          []                                              -> AllColinear v
          (h3 : rest)
            | direction h1 h3 `isParallelTo2` l -> go rest
            | otherwise                                   -> NonDegnerate $ Vector3 h1 h2 h3

    toPV = \case
      VerticalLineThrough x  -> verticalLine x
      NonLinear (LineEQ a b) -> fromLinearFunction a b


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
