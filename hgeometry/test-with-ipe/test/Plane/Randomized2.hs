module Plane.Randomized2
  (

  ) where


import Data.Foldable
import Witherable
import HGeometry.Plane
import HGeometry.Ext
import HGeometry.Kernel
import HGeometry.HalfLine
import HGeometry.Map.NonEmpty.Monoidal
import System.Random
import Control.Lens hiding (Prism)
import Data.Foldable1
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import HGeometry.Plane.LowerEnvelope.Connected.BruteForce qualified as BruteForce
import HGeometry.Plane.LowerEnvelope.Connected( mapVertices
                                              , MinimizationDiagram
                                              , MDVertex
                                              , VertexForm
                                              , fromVertexForm
                                              )
import Data.Map.NonEmpty qualified as NEMap
import Data.Map (Map)
import Data.Map qualified as Map
import Prelude hiding (filter)
--------------------------------------------------------------------------------



type TriangulatedEnvelope r plane = MonoidalNEMap plane (NonEmpty (Prism r plane :+ [plane]))



-- root i = iterate (i `div`) sqrt . fromIntegral
--   where

type Probability = Double

-- | Given a parameter r, the main idea is to take a (r/n)-sample of
-- the given set; we return both the sampled set, as well as the
-- remainder (non-sampled elements).
--
-- pre: r >= 3
--
-- more specifically, we return the first three elements, so that the
-- ouput set has size at least 3, and a p-sample of the rest.
sample          :: (RandomGen gen, Foldable1 set)
                => gen -> Int -> set a -> (NonEmpty a, [a], gen)
sample gen r xs = let (sample',rest) = splitAt r (toList xs)
                  in (NonEmpty.fromList sample',rest,gen)
  -- FIXME: do the actual sampling rather than just returning the first r elents

-- type TriangulatedEnvelope r plane = MonoidalNEMap plane (NonEmpty (Prism r plane :+ [plane]))


-- | Given a set H, a subset S (which is a (s/n)-sample) compute the
-- lower envelope of the planes in S, and their conflict lists w.r.t
-- the planes in H.
lowerEnvelope' :: forall plane r set subset gen. ( Plane_ plane r
                        , Ord r, Fractional r
                        , Foldable1 set, Foldable1 subset
                        , RandomGen gen
                        , Witherable subset

                        , Show r, Show plane
                        , Ord plane
                        ) => gen -> set plane -> subset plane ->
                           ( TriangulatedEnvelope r plane
                           , Box (Point 2 r)
                           , gen
                           )
lowerEnvelope' gen0 allPlanes ss = if (s <= nDelta) then undefined else (env, bbox, gen')
  where
    n  = length allPlanes
    nDelta = ceiling . sqrt . sqrt $ fromIntegral n
    s  = length ss
    r  = min s nDelta

    menv'           :: Maybe (TriangulatedEnvelope r plane)
    triangulatedEnv = triangulate <$> menv


    menv :: Maybe (MinimizationDiagram r (MDVertex r plane [plane]) plane)
    menv = bruteForceEnvelope allPlanes rs
    (rs,rest,gen1) = sample gen0 r ss

    -- lowerEnv =


    env = undefined
    bbox = undefined
    gen' = undefined


-- | Given two sets H and R of planes; compute the lower envelope of R using a brute force
-- method, and compute the conflict lists of every vertex w.r.t the first set H.
--
-- O(r^4 + rn)
bruteForceEnvelope           :: forall plane r set set'.
                                ( Plane_ plane r, Ord r, Fractional r
                                , Foldable1 set, Foldable1 set'
                                , Ord plane
                                , Show plane, Show r
                                )
                             => set plane
                             -> set' plane
                             -> Maybe (MinimizationDiagram r (MDVertex r plane [plane]) plane)
bruteForceEnvelope allPlanes = NEMap.withNonEmpty Nothing (Just . toDiagram)
                             . BruteForce.computeVertexForm
  where
    toDiagram :: VertexForm NEMap.NEMap r plane
              -> MinimizationDiagram r (MDVertex r plane [plane]) plane
    toDiagram = mapVertices (fmap (^.core))
              . fromVertexForm
              . NEMap.mapWithKey (\v defs -> vertexConflictList allPlanes v :+ defs)

-- data Prism r plane = Prism (Triangle (MDVertex r plane [plane]))
  -- prism may be unbounded as well






-- | Compute the conflict list of a vertex.
vertexConflictList          :: ( Point_ vertex 3 r, Ord r, Num r
                               , Plane_ plane r, Foldable set
                               ) => set plane -> vertex -> [plane]
vertexConflictList planes v = filter (v `liesAbove`) (toList planes)
  where
    v `liesAbove` h = verticalSideTest v h /= GT


-- | The unbounded edges of a prism
unboundedEdges :: Prism r plane -> Maybe (Vector 2 (HalfLine (Point 3 r)))
unboundedEdges = \case
    BoundedRegion _    -> Nothing
    UnboundedRegion () -> Just $ Vector2 (HalfLine _ _) (HalfLine _ _)
  where

-- | Planes that conflict with an edge of the prism.
edgeConflictLists              :: ( Plane_ plane r, Ord r, Num r, Foldable set
                                  ) => set plane -> HalfLine (Point 3 r) -> [plane]
edgeConflictLists planes prism = let Vector2 hl hl' = unboundedEdges prism
                                 in filter (\h -> h `intersects` hl || h `intersects hl'`)
                                           (toList planes)

-- | Triangulate the regions
triangulate :: forall plane r. MinimizationDiagram r (MDVertex r plane [plane]) plane
            -> MonoidalNEMap plane (NonEmpty (Prism r plane))
triangulate = MonoidalNEMap . fmap triangulate' . asMap
  where
    triangulate'        :: Region r (MDVertex r plane [plane]) -> NonEmpty (Prism r plane)
    triangulate' region = undefined




-- bruteForceEnvelope :: set plane ->
