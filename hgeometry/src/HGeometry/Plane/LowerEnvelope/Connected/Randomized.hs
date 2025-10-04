--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Connected.Randomized
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- expected O(n polylog n) divide and conquer implementation
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Randomized
  ( computeVertexForm

  , withConflictLists
  , takeSample

  , withExtraConflictLists
  ) where

import           Control.Lens hiding (below)
import           Data.Foldable
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Map.NonEmpty (NEMap, pattern IsEmpty, pattern IsNonEmpty)
import qualified Data.Map.NonEmpty as NEMap
import           Data.Maybe (mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Vector.NonEmpty as V
import           HGeometry.Ext
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Permutation.Shuffle
import           HGeometry.Plane.LowerEnvelope.Clipped.Type
import qualified HGeometry.Plane.LowerEnvelope.Connected.BruteForce as BruteForce
import           HGeometry.Plane.LowerEnvelope.Connected.Region
import           HGeometry.Plane.LowerEnvelope.Connected.Regions
import           HGeometry.Plane.LowerEnvelope.Connected.VertexForm (definersOf)
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Triangle
import           Prelude hiding (filter, head, last)
import           System.Random

--------------------------------------------------------------------------------

-- n_0 :: Int
-- n_0 = 2

--------------------------------------------------------------------------------

computeVertexForm        :: ( Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable1 set
                            , RandomGen gen
                            , Show plane, Show r
                            )
                         => gen -> set plane -> VertexForm Map r plane
computeVertexForm gen = computeVertexFormIn tri . V.unsafeFromVector . shuffle gen
    -- since the set is non-empty, the V.unsafeFromVector is actually also safe
  where
    tri = Triangle (Point2 (-100_000) (-100_000))
                   (Point2 (-100_000) (200_000))
                   (Point2 (200_000)  (-100_000))
    -- TODO: compute abounding box/triangle instead

toSet :: (Foldable1 f, Ord a) => f a -> NESet a
toSet = NESet.fromList . toNonEmpty

-- | pre:
--
-- - input is already a random permutation
computeVertexFormIn         :: forall plane point r.
                              ( Plane_ plane r, Ord plane, Ord r, Fractional r
                              , Show plane, Show r
                              , Point_ point 2 r
                              )
                            => Triangle point
                            -> V.NonEmptyVector plane
                            -> VertexForm Map r plane
computeVertexFormIn tri0 hs = lowerEnvelopeIn (view asPoint <$> tri0) (toSet hs)
  where
    n  = length hs
    r' = max 5 $ sqrt . sqrt @Double . fromIntegral $ n -- r = n^{1/4}
    r  = round $ r' * logBase 2 r'
      -- take a sample of size r*log r
      --
      -- the max 5 is due to the following: consider some triangle defined by the plane
      -- h_0, and let h_1, h_2, h_3, be the planes that intersect h_0 on the edges of the
      -- triangle. If this triangle has a conflict list, i.e. there is a plane h that
      -- intersects below a vertex, then we need to include at least h_0,..,h_3,h. Hence,
      -- we need at least 5 planes.

      -- TODO: make sure that if the lower envelope has a vertex, we select planes
      -- that generate a plane.

    lowerEnvelopeIn :: Triangle (Point 2 r) -> NESet plane -> Map (Point 3 r) (Definers plane)
    -- lowerEnvelopeIn tri planes | traceShow ("LE",tri,length planes) False = undefined
    lowerEnvelopeIn tri planes = let (rNet,remaining) = takeSample r planes in
      case withConflictLists remaining (BruteForce.computeVertexForm rNet) of
        IsEmpty                                                                    -> mempty
        IsNonEmpty (verticesRNet :: NEMap (Point 3 r) (Definers plane, Set plane)) ->
          NEMap.mapMaybeWithKey (asVertexIn tri) verticesRNet
            `merge` foldMap lowerEnvelopeIn' triangulatedEnv
          where
            -- Construct the lower envelope inside the triangle.
            -- also compute the conflicts of the corners
            env :: NEMap plane (ClippedMDCell'' r (MDVertex r plane (_, Set plane))
                                                  (Point 2 r :+ (ExtraDefiners plane, Set plane))
                               )
            env = withExtraConflictLists (NESet.toSet planes)
                . fromVertexFormIn tri rNet $ verticesRNet

            -- | Triangulate the lower envelope, and collect the conflict list of each
            -- prism. We will already throw away any prisms with an
            triangulatedEnv :: [Triangle (Point 2 r) :+ NESet plane]
            triangulatedEnv = NEMap.foldMapWithKey triangulate env
              -- traceShow ("n'",length planes
              --           ,"total size: ", sum (map (\(_ :+ cl) -> length cl) res)
              --           ," sizes: ",map (\(_ :+ cl) -> length cl) res
              --           ) res
              -- where
              --   res =

            -- merge the resulting Maps; making sure to actually combine the definers
            merge = Map.unionWithKey mergeDefiners


    lowerEnvelopeIn'     :: Triangle (Point 2 r) :+ NESet plane
                         -> Map (Point 3 r) (Definers plane)
    lowerEnvelopeIn' (tri :+ conflictList) = lowerEnvelopeIn tri conflictList


-- verify tri0 planes tr@(tri :+ cl)
--   | length cl < length planes = tr
--   | otherwise                 = error $ show (tri0,planes,tr)

-- | Given a size r > 0; take a sample of the planes from the given size (essentially by just
-- taking the first r planes.)
--
-- pre: r > 0
takeSample   :: (Foldable1 set, Ord plane) => Int -> set plane -> (NonEmpty plane,Set plane)
takeSample r = bimap NonEmpty.fromList Set.fromList . splitAt r . toList


-- | Report whether this is really a vertex of the global lower envelope in the region.
asVertexIn :: (Ord r, Num r, Point_ vertex 2 r, Foldable set
              , definers ~ Definers plane
              )
           => Triangle vertex -> Point 3 r -> (definers, set plane) -> Maybe definers
asVertexIn tri (Point3 x y _) (defs,conflictList)
  | null conflictList && Point2 x y `intersects'` tri = Just defs
  | otherwise                                         = Nothing
  where
    p `intersects'` tri' = p `intersects` tri' && notElemOf (vertices.asPoint) p tri'

    -- toSet :: (Foldable f, Ord a) => f a -> Set a
    -- toSet = Set.fromList . toList
{-
hasNoConflict                     :: Foldable set => (definers, set plane) -> Maybe definers
hasNoConflict (defs,conflictList)
  | null conflictList              = Just defs
  | otherwise                      = Nothing

-- | Get the conflictList of a triangle
conflictListOf :: Monoid conflictList => Triangle (vertex :+ conflictList) -> conflictList
conflictListOf = foldMap (^.extra)

-}

-- | Computes conflict list
withConflictLists        :: ( Plane_ plane r, Ord r, Num r, Ord plane
                            , FunctorWithIndex (Point 3 r) (map (Point 3 r))
                            , Show r, Show plane
                            )
                         => Set plane
                         -> VertexForm map r plane
                         -> map (Point 3 r) (Definers plane, Set plane)
withConflictLists planes = imap (\v defs -> (defs, Set.filter (below v) planes))
  where
    below v h = verticalSideTest v h == GT
    -- a plane conflicts with a vertex v if it passes strictly below the point
  -- TODO: we should replace this with something more efficient.


-- | Compute the conflict lists for the extra vertices we added. In addition,
-- compute the definers for the given position.
withExtraConflictLists        :: (Plane_ plane r, Ord r, Num r--, Point_ corner 2 r
                                 , Show plane, Show r
                                 )
                              => Set plane
                              -> NEMap plane (ClippedMDCell r plane (a, Set plane))
                              -> NEMap plane (ClippedMDCell'' r (MDVertex r plane (a, Set plane))
                                                                (Point 2 r :+ (ExtraDefiners plane, Set plane)))
withExtraConflictLists planes = NEMap.mapWithKey $ \h -> fmap (withPolygonVertex h)
  where
    withPolygonVertex h v@(Point2_ x y) = v :+ (defs,belows)
      where
        z             = evalAt v h
        (belows,rest) = Set.partition (sideTest GT) planes
        (defs, _)     = Set.partition (sideTest EQ) rest
        sideTest res h' = verticalSideTest (Point3 x y z) h' == res

triangulate                        :: (Num r, Ord plane
                                      , Show plane, Show a, Show r
                                      )
                                   => plane
                                   -> ClippedMDCell'' r (MDVertex r plane (a, Set plane))
                                                        (Point 2 r :+ (ExtraDefiners plane, Set  plane))
                                   -> [Triangle (Point 2 r) :+ NESet plane]
triangulate h (ClippedMDCell cell) = case cell of
    ActualPolygon poly -> triangulate' h poly
    DegenerateVertex _ -> []
    DegenerateEdge   _ -> []
  -- TODO: technically, there could still be vertices on the degenerate edge.
  -- make sure we hanlde those somewhere else


-- | The definers of extra vertices. These extra vertices typically lie on edges
-- of the lower envelope; these extra definers are the planes that define such an edge.
type ExtraDefiners plane = Set plane

triangulate'        :: (Num r, Ord plane
                                      , Show plane, Show a, Show r
                       )
                    => plane
                    -> ClippedBoundedRegion r (MDVertex r plane (a, Set plane))
                                              (Point 2 r :+ (ExtraDefiners plane, Set plane))
                    -> [Triangle (Point 2 r) :+ NESet plane]
triangulate' _ poly = mapMaybe' withConflictList $ case toNonEmptyOf vertices poly of
    u :| (v : vs) -> NonEmpty.zipWith (Triangle u) (v :| vs) (NonEmpty.fromList vs)
    _             -> error "absurd. trianglulate; impossible"
  where
    withConflictList tri = case foldMap conflictListOf tri of
      NESet.IsEmpty                 -> Nothing
      NESet.IsNonEmpty conflictList -> Just $
        let conflictList' = foldr NESet.insert conflictList (foldMap edgeDefiners $ edgesOf tri)
        in ((^.asPoint) <$> tri) :+ conflictList'

    edgesOf (Triangle a b c) = [(a,b),(b,c),(c,a)]

    -- computes the edge definers of the edge u,v. Note that this will typically consist
    -- of two planes; h, and the other plane h_(u,v) on the other side of the edge.
    edgeDefiners (u,v) = definersOf' u `Set.intersection` definersOf' v
    definersOf' = \case
      Original v            -> foldMap Set.singleton $ definersOf v
      Extra (_ :+ (defs,_)) -> defs

    -- get the conflictList as a vertex
    conflictListOf = \case
      Original v -> v^.vertexData._2
      Extra    p -> p^.extra._2

    mapMaybe' f = mapMaybe f . toList



----------------------------------------

-- debugRender fp x = unsafePerformIO $ debugRender' fp x
-- debugRender' fp x = writeIpeFile fp $ singlePageFromContent
--     [ x
--     ]


-- buggy = fromList

--   [NonVerticalHyperPlane [-24,18.23529,12], NonVerticalHyperPlane [-23.91667,19.33333,-11.44445],NonVerticalHyperPlane [-23.81819,6.2,10.25],NonVerticalHyperPlane [-19.04167,-24,2.36363],NonVerticalHyperPlane [-18.3,-11.5,-3.35295],NonVerticalHyperPlane [-15.36843,-3.8,2.85714],NonVerticalHyperPlane [-9.09091,8.4375,-22],NonVerticalHyperPlane [-4.66667,19,1.46153],NonVerticalHyperPlane [-3.33334,17.55555,0],NonVerticalHyperPlane [-3,-4.85715,-3.2381],NonVerticalHyperPlane [-2.13637,-15,-2.92858],NonVerticalHyperPlane [0,22,-2.33334],NonVerticalHyperPlane [1.63157,6.875,5.21739],NonVerticalHyperPlane [2.13043,1.29166,-13.125],NonVerticalHyperPlane [3.27272,-8.76924,-20.33334],NonVerticalHyperPlane [5.8,-2.66667,19.35],NonVerticalHyperPlane [6.75,-14,-23.3],NonVerticalHyperPlane [9.30769,4,-22],NonVerticalHyperPlane [9.42857,16,-6],NonVerticalHyperPlane [15.17391,16.8,2.77777],NonVerticalHyperPlane [17.1875,3.4375,16.83333],NonVerticalHyperPlane [17.8,6.77777,-5.83334],NonVerticalHyperPlane [17.82352,4.21428,-16],NonVerticalHyperPlane [24,17.64285,-13]])
