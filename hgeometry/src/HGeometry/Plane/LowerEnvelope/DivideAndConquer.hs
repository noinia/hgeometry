module HGeometry.Plane.LowerEnvelope.DivideAndConquer
  ( lowerEnvelope
  ) where

import           Control.Lens
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Word
import           HGeometry.Ext
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Line.LineEQ
import qualified HGeometry.Line.LowerEnvelope as LowerEnvelope
import           HGeometry.Line.PointAndVector
import           HGeometry.Plane.LowerEnvelope.AdjListForm
import           HGeometry.Plane.LowerEnvelope.EpsApproximation
import qualified HGeometry.Plane.LowerEnvelope.Naive as Naive
import           HGeometry.Plane.LowerEnvelope.Type
import           HGeometry.Plane.LowerEnvelope.VertexForm
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector
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


                    , Intersection (VerticalOrLineEQ r :+ plane) (VerticalOrLineEQ r :+ plane)
                      ~ Maybe (LineLineIntersection (VerticalOrLineEQ r :+ plane))
                    ) => f plane -> LowerEnvelope plane
lowerEnvelope hs = case verifyNotAllColinear hs of
    JustOnePlane h  -> ParallelStrips $ Set.singleton (h^.re _Wrapped')
    AllColinear v   -> let lines = fmap (toLine v) hs
                           env   = LowerEnvelope.lowerEnvelope lines
                       in ParallelStrips $ fromEnv env
    NonDegenerate _ -> fromVertexForm hs . lowerEnvelopeVertexForm $ hs

toLine :: Vector 2 r -> plane -> LineEQ r :+ plane
toLine = undefined

fromEnv :: ( g ~ [])
        => LowerEnvelope.LowerEnvelopeF g (Point 2 r) (LineEQ r :+ plane) -> Set.Set (ParallelPlane plane)
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
verifyNotAllColinear    :: forall f plane r.
                           ( Plane_ plane r
                           , Ord r, Fractional r, Foldable1 f, Ord plane
                           , Show plane, Show r
                           ) => f plane -> VerifyDegenerate plane r
verifyNotAllColinear hs = findOnParalellPlanes (toNonEmpty hs)
  where
    -- try to find two non-parallel planes h1 h2
    findOnParalellPlanes              :: NonEmpty plane -> VerifyDegenerate plane r
    findOnParalellPlanes (h1 :| rest) = case NonEmpty.nonEmpty rest of
      Nothing            -> JustOnePlane h1
      Just (h2 :| rest') -> case direction h1 h2 of
        Left h' -> findOnParalellPlanes (h' :| rest')
        Right l -> findThirdPlane h1 h2 l rest -- find a third plane

    -- Tries to compute the vector of the line in which the two planes intersect,
    -- returns this vector (if the intersection line exists), or the lowest plane if one
    -- is always below the other.
    direction       :: plane -> plane -> Either plane (Vector 2 r)
    direction h1 h2 = case intersectionLine h1 h2 of
      Nothing | heightOf h1 <= heightOf h2 -> Left h1
              | otherwise                  -> Left h2
      Just l                               -> Right $ case l of
        VerticalLineThrough _    -> Vector2 0 1
        NonVertical (LineEQ a _) -> Vector2 1 a

    heightOf = evalAt (origin :: Point 2 r)


    findThirdPlane h1 h2 v = go
      where
        go = \case
          []                                    -> AllColinear v
          (h3 : rest) -> case direction h1 h3 of
            Left _  -> go rest -- h3 is parallel to h1, so simply continue the search
                               -- note that we don't really care if h1 or h3 is the lowest
            Right u | u  `isParallelTo'` v      -> go rest
                    | otherwise                 -> NonDegenerate $ Vector3 h1 h2 h3

    isParallelTo' (Vector2 ux uy) (Vector2 vx vy) = denom == 0
      where
        denom = vy * ux - vx * uy

-- | Compute the vertices of the lower envelope
--
--
-- running time: \(O(n \log n)\)
lowerEnvelopeVertexForm    :: forall f plane r.
                              ( Plane_ plane r
                              , Ord r, Fractional r, Foldable1 f, Functor f, Ord plane
                              , Show plane, Show r
                              ) => f plane -> VertexForm plane
lowerEnvelopeVertexForm hs
    | n <= nZero = Naive.lowerEnvelopeVertexForm hs
    | otherwise  = foldMap lowerEnvelopeVertexForm superCells'
                   -- we recursively compute the lower envelope (in vertex form)
                   -- for each supercell, and combine (= concatenate) their results.
  where
    -- | Computes an 1/r-net of the planes in hs
    net  = epsilonNet r hs
    -- Compute the triangulated lower envelope of the net
    env = triangulatedLowerEnvelope net
    -- We form supercells out of the triangles of the lower envelop
    superCells  = formSuperCells s env
    -- and for each supercell, compute its conflict list.
    superCells' :: NonEmpty (NonEmpty plane)
    superCells' = undefined

    r = undefined -- parameter for the 1/r net
    s = undefined -- parameter for the number of cells per super cell

    conflictLists' = computeConflictLists env hs


    -- conflictLists = undefined --- combineConlictLists




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
