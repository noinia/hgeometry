module Plane.Randomized2
  ( verticesIn
  , lowerEnvelopeOn'
  ) where

import           Data.Bifoldable
import           Data.Foldable
import           Witherable
import           HGeometry.Plane
import           HGeometry.Ext
import           HGeometry.Point.Either
import           HGeometry.Kernel
import           HGeometry.Polygon
import           HGeometry.HalfLine
import           HGeometry.Map.NonEmpty.Monoidal
import           Data.Set (Set)
import qualified Data.Set as Set
import           System.Random
import           Control.Lens hiding (Prism, Prism')
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import           Plane.BruteForce
import           Plane.Sample
import qualified HGeometry.Plane.LowerEnvelope.Connected.BruteForce as BruteForce
import HGeometry.Plane.LowerEnvelope.Connected( mapVertices
                                              , MinimizationDiagram
                                              , MDVertex
                                              , VertexForm
                                              , fromVertexForm
                                              )
import qualified Data.Map.NonEmpty as NEMap
import           Data.Map (Map)
import qualified Data.Map as Map
import           Prelude hiding (filter)
import           Plane.Sample
import           Data.Map.Monoidal (MonoidalMap)
import qualified Data.Map.Monoidal as MonoidalMap
import           HGeometry.Triangle

--------------------------------------------------------------------------------

-- | Given a domain and a set of planes; computes for each plane the region at which it is
-- lowest.
--
-- The union of these regions covers the domain.
--
lowerEnvelopeOn'            :: ( Plane_ plane r, Ord plane, Ord r, Fractional r
                              , Foldable1 set, RandomGen gen
                              , Show plane, Show r
                              )
                           => gen -> Triangle (Point 2 r) -> set plane
                           -> BoundedLowerEnvelope r plane
lowerEnvelopeOn' gen domain = fromVertices domain . verticesIn gen domain


-- | Randomized algorithm to compute the vertices of the lower envelope
--
-- we return only the vertices strictly inside the domain
verticesIn               :: forall gen set plane corner r.
                            ( Plane_ plane r, Ord r, Fractional r
                            , RandomGen gen
                            , Foldable1 set, Ord plane
                            , Point_ corner 2 r

                            , Show r, Show plane, Show corner
                            )
                         => gen
                         -> Triangle corner
                         -> set plane
                         -> Set (EnvVertex r plane)
verticesIn gen0 domain hs
  | n <= n0   = bruteForceVerticesIn domain hs
  | otherwise = lowerEnv $ sampleSubset gen0 r n hs
 where
   n = length hs
   -- r = n^{1/4}
   r = ceiling . sqrt . sqrt $ fromIntegral n

   lowerEnv                           :: (Sample NonEmpty plane, gen) -> Set (EnvVertex r plane)
   lowerEnv (Sample rs _ rest _, gen) = foldMap report vs
                                     <> ifoldMap (foldMap . recurse) env
     where
       -- | The envelope; in which each vertex is tagged with whether it lies in the domain
       -- and its conflict list
       env :: TriangulatedLowerEnvelope'' r plane
       env = triangulate . withExtraConflictLists rest <$> fromVertices domain vs

       -- | Compute the vertices of the sample using a brute force manner
       vs = Set.mapMonotonic mkVertex $ bruteForceVertices rs
         -- note: we will just tag the vertex with additional info; so mkVertex is monotonic

       -- | Test if we should report some vertex  from the sample
       report (v :+ (inside', cl)) = if inside' && null cl then Set.singleton v else mempty

       -- | We recurse on every prism
       recurse        :: plane
                      -> Prism'' r plane
                      -> Set (EnvVertex r plane)
       recurse h cell = case NonEmpty.nonEmpty $ conflictListOf cell of
           Nothing -> mempty
           Just cl -> verticesIn gen cell (h NonEmpty.<| cl)


       -- | Tag the vertex with whether it lies inside the domain, and its conflict list
       mkVertex   :: EnvVertex r plane -> EnvVertex r plane :+ (Bool, [plane])
       mkVertex v = v :+ ( v `intersects` domain
                         , computeConflictList v
                         )


       -- intersectsDomain :: NonEmpty (Prism'' r plane) -> Maybe (NonEmpty (Prism'' r plane))
       -- intersectsDomain = NonEmpty.nonEmpty
       --                  . NonEmpty.filter (`intersects` domain)
       --   -- TODO we may be able to use the bools about vertex locations already to speed this up


       computeConflictList v = let v' = location v
                               in filter (\h -> verticalSideTest v' h == LT) (toList rest)
       -- TODO: this should use the batched point loc, but whatever.


-- Every vertex is tagged with whether it lies in the domain, and its conflict list.
type V r plane = OriginalOrExtra (EnvVertex r plane :+ (Bool, [plane]))
                                 ((Point 2 r :+ r)  :+ (Bool, [plane]))

type Prism'' r plane = Triangle (V r plane)

type BoundedLowerEnvelope'' r plane =
  MonoidalMap plane (ConvexPolygon (V r plane))

-- | Triangulated lower envelope; with conflict lists
type TriangulatedLowerEnvelope'' r plane =
  MonoidalMap plane (NonEmpty (Prism'' r plane))

withExtraConflictLists    :: (Plane_ plane r, Ord r, Num r, Foldable set
                             )
                          => set plane
                          -> ConvexPolygon (OriginalOrExtra
                                               (EnvVertex r plane :+ (Bool, [plane]))
                                               (Point 2 r :+ r)
                                           )
                          -> ConvexPolygon (V r plane)
withExtraConflictLists hs = over (vertices._Extra) $ \p@(Point2 x y :+ z) ->
                              p :+ (False, computeConflictList (Point3 x y z))
  where
    computeConflictList q = filter (\h -> verticalSideTest q h == LT) (toList hs)
                            -- TODO: this should use the batched point loc, but whatever.


-- instance (Point_ corner 2 r, Ord r, Num r
--          ) => HasIntersectionWith (EnvVertex r plane) (Triangle corner) where
--   v `intersects` tri = (v^.asPoint) `intersects` tri




-- | minimum value below which we just use the bruteForce method anyway
n0 :: Int
n0 = 5

-- | computes the conflict list of a prism
conflictListOf :: Triangle (OriginalOrExtra (orig :+ (bool,[plane]))
                                            (extra :+ (bool,[plane]))
                           ) -> [plane]
conflictListOf = foldMap (bifoldMap (^.extra._2) (^.extra._2))

  -- toListOf (folded.both.extra._2.folded)


  -- foldMap (

  -- both (^.extra._2))


-- bruteForceEnvelope =


{-


-- | For every plane, the list of prisms, each together with their conflict list
type TriangulatedEnvelope r plane = MonoidalNEMap plane (NonEmpty (Prism r plane :+ [plane]))



data Prism r plane


type Probability = Double









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

-}
