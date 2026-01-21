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
import Data.List qualified as List
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
import Plane.Sample

--------------------------------------------------------------------------------



-- | For every plane, the list of prisms, each together with their conflict list
type TriangulatedEnvelope r plane = MonoidalNEMap plane (NonEmpty (Prism r plane :+ [plane]))



data Prism r plane


type Probability = Double




-- | minimum value below which we just use the bruteForce method anyway
n0 :: Int
n0 = 5





-- | Given a set H, a subset S (which is a (s/n)-sample) compute the
-- lower envelope of the planes in S, and their conflict lists w.r.t
-- the planes in H.
lowerEnvelope' :: forall plane r subset gen. ( Plane_ plane r
                        , Ord r, Fractional r
                        , Foldable1 subset
                        , RandomGen gen
                        , Show r, Show plane
                        , Ord plane
                        ) => gen -> Sample subset plane ->
                           ( TriangulatedEnvelope r plane
                           , Box (Point 2 r)
                           , gen
                           )
lowerEnvelope' gen0 input = lowerEnv gen0 input
  where
    nDelta = ceiling . sqrt . sqrt $ fromIntegral (totalSize input)
    r      = min (sampleSize input) nDelta

    lowerEnv                           :: gen
                                       -> Sample subset plane
                                       -> ( TriangulatedEnvelope r plane
                                          , Box (Point 2 r)
                                          , gen
                                          )
    lowerEnv gen sample@(Sample ss s rest n)
      | n <= n0 || s <= nDelta = let (env, bBox) = bruteForceEnvelope sample
                                 in (env, bBox, gen)
      | otherwise              = let (sample', gen') = sampleSubset gen r s ss
                                 in lowerEnv1 gen' sample' rest n

    lowerEnv1 :: gen -> Sample NonEmpty plane -> [plane] -> Int ->
                 ( TriangulatedEnvelope r plane
                 , Box (Point 2 r)
                 , gen
                 )
    lowerEnv1 gen (Sample rs r restSs s) restHs n = undefined
      where
        (env, bBox) = bruteForceEnvelope (Sample rs r (restSs <> restHs) n)
        env'        = ifoldMap (\h -> foldMap (go h)) env
          -- this should either become a traverse; or we should split gen' all the time

        go h nabla@(prism :+ conflictList) = case NonEmpty.nonEmpty conflictList of
          Nothing            -> singleton h (NonEmpty.singleton nabla)
            -- we found an actual prism in the final solution
          Just conflictList' -> _
            where
              --
              extended    = definingPlanes h prism <> conflictList'
              (rs',rest') = undefined
              m           = length extended

              env         = lowerEnv _ (Sample rs' r' rest' m)


definingPlanes h prism = h :| undefined
-- this may now introduce some duplicatesI guess




type Definers plane = [plane]



    -- lowerEnv0 :: gen -> Sample subset plane -> ( TriangulatedEnvelope r plane
    --                                            , Box (Point 2 r)
    --                                            , gen
    --                                            )
    -- lowerEnv0 gen
    --   where
    --     verticesEnv     = bruteForceLowerEnvelope rs rest
    --     bBox            = boundingBox . fmap projectPoint $ verticesEnv



    -- menv'           :: Maybe (TriangulatedEnvelope r plane)
    -- triangulatedEnv = triangulate <$> menv


    -- menv :: Maybe (MinimizationDiagram r (MDVertex r plane [plane]) plane)
    -- menv = bruteForceEnvelope allPlanes rs
    -- (rs,rest,gen1) = sample gen0 r ss

    -- lowerEnv =


    -- env = undefined
    -- bbox = undefined
    -- gen' = undefined

{-

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



data Prism r plane = Prism (Triangle (MDVertex r plane [plane]))
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
                                 in filter (\h -> h `intersects` hl || h `intersects` hl')
                                           (toList planes)

-- | Triangulate the regions
triangulate :: forall plane r. MinimizationDiagram r (MDVertex r plane [plane]) plane
            -> MonoidalNEMap plane (NonEmpty (Prism r plane))
triangulate = MonoidalNEMap . fmap triangulate' . asMap
  where
    triangulate'        :: Region r (MDVertex r plane [plane]) -> NonEmpty (Prism r plane)
    triangulate' region = undefined

-}
