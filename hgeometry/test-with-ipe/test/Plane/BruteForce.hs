{- HLINT ignore "Use list literal pattern" -}
{-# LANGUAGE UndecidableInstances #-}
module Plane.BruteForce
  ( bruteForceVertices, bruteForceVerticesIn
  , lowerEnvelopeOn
  , triangulatedLowerEnvelopeOn
  , TriangulatedLowerEnvelope, TriangulatedLowerEnvelope'
  , BoundedLowerEnvelope, BoundedLowerEnvelope'
  , Prism, Prism'
  , Vertex'
  , EnvVertex(..), location, extraDefiners

  , fromVertices
  , triangulate


  , computeCellIn
  -- , computeDomain



  -- , Vertex(..), location, location2


  -- , bruteForceTriangulatedEnvelope
  -- , bruteForceTriangulatedEnvelopeIn
  -- , TriangulatedLowerEnvelope
  -- , Prism
  -- , Vertex'(..)

  -- , allZippers

  -- , coverCone
  -- , coverClippedCone

  -- , findMissingEdge
  -- , findRotateTo
  ) where

import           HGeometry.Intersection
import           HGeometry.Boundary
import           Data.Foldable
import           HGeometry.HalfLine
import           Control.Lens hiding (Prism, Prism')
import           Prelude hiding (filter)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Foldable1
import           HGeometry.Map.NonEmpty.Monoidal (MonoidalNEMap)
import           HGeometry.HalfPlane.CommonIntersection.Bounded
import qualified HGeometry.Map.NonEmpty.Monoidal as MonoidalNEMap
import           Data.Foldable.WithIndex
import           Data.Foldable (Foldable(..))
import           Data.Maybe (fromMaybe, maybeToList)
import           Plane.Sample
import           HGeometry.Kernel
import           HGeometry.HyperPlane.Class
import           HGeometry.Ext
import           Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.List qualified as List
import           Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MonoidalMap
import           HGeometry.Combinatorial.Util
import           Data.List (inits, tails)
import           HGeometry.List.Util
import           HGeometry.Triangle
import           Data.Map (Map)
import Data.Map qualified as Map
import           HGeometry.Plane.LowerEnvelope.Connected.Primitives
import           Control.DeepSeq
import           GHC.Generics (Generic)
import           Data.Functor.WithIndex
import           Witherable
import           HGeometry.Cyclic (Cyclic)
import           Control.Applicative


import           Plane.Debug
import           HGeometry.Polygon
import           HGeometry.Point.Either
import           Data.Bifunctor
import           HGeometry.Cone
import           Data.Ord




--------------------------------------------------------------------------------

-- | a vertex is defined by at least three planes.
--
-- The Eq and Ord instances only consider these three defining planes,
-- and assume that they are ordered in increasing order (in some
-- global order defined on the planes).
--
-- we mostlly think of the vertices as (their 2d projection). Hence, we treat a
-- vertex as 2 dimensional.
data EnvVertex r plane = EnvVertex !plane !plane !plane
                                   [plane] -- ^ remaining defining planes ; purposly lazy
                                   (Point 2 r) -- ^ its projected location
                                   r -- the z-coordinate
                       deriving (Show,Foldable,Functor,Generic)

type instance NumType   (EnvVertex r plane) = r
type instance Dimension (EnvVertex r plane) = 2


-- | Get the location of the vertex as a point in R^3.
location                                    :: EnvVertex r plane -> Point 3 r
location (EnvVertex _ _ _ _ (Point2 x y) z) = Point3 x y z


-- | Get the 2d location of the vertex.
location2 :: Lens' (EnvVertex r plane) (Point 2 r)
location2 = lens (\(EnvVertex _ _ _  _ p _) -> p)
                 (\(EnvVertex h1 h2 h3 hs _ z) p -> EnvVertex h1 h2 h3 hs p z)

-- | Access the extra definers
extraDefiners :: Lens' (EnvVertex r plane) [plane]
extraDefiners = lens (\(EnvVertex _ _ _  hs _ _) -> hs)
                 (\(EnvVertex h1 h2 h3 _ p z) hs -> EnvVertex h1 h2 h3 hs p z)


instance Eq plane => Eq (EnvVertex r plane) where
  (EnvVertex u v w _ _ _) == (EnvVertex u' v' w' _ _ _) = u == u' && v == v' && w == w'

instance Ord plane => Ord (EnvVertex r plane) where
  (EnvVertex u v w _ _ _) `compare` (EnvVertex u' v' w' _ _ _) =
    u `compare` u' <> v `compare` v' <> w `compare` w'

instance (NFData r, NFData plane) => NFData (EnvVertex r plane)

instance HasVector (EnvVertex r plane) (EnvVertex r plane) where
  vector = location2.vector
instance HasCoordinates (EnvVertex r plane) (EnvVertex r plane) where
  coordinates = location2.coordinates

instance Affine_ (EnvVertex r plane) 2 r
instance Point_ (EnvVertex r plane) 2 r

instance (Point_ corner 2 r, Num r, Ord r
         ) => HasIntersectionWith (EnvVertex r plane) (Triangle corner) where
  v `intersects` t = (v^.asPoint) `intersects` t

instance Foldable1 (EnvVertex r) where
  foldMap1 f (EnvVertex h1 h2 h3 hs _ _) = let z = f h1 <> f h2 <> f h3
                                           in maybe z (z <>) (foldMap (Just . f) hs)

--------------------------------------------------------------------------------

-- | Vertices in our bounded envelopes are either real vertices, or dummy vertices
type Vertex' v r plane = OriginalOrExtra v (Point 2 r :+ r)

-- | The lower envelope for a bounded domain is a mapping from planes
-- to the regions in which they are lowest. Since the input domain is
-- bounded; these regions are bounded convex polygons.
type BoundedLowerEnvelope r plane = BoundedLowerEnvelope' (EnvVertex r plane) r plane

-- | Implementation of bounded lower envelope that allows general types of vertices v
type BoundedLowerEnvelope' v r plane =
  MonoidalMap plane (ConvexPolygon (Vertex' v r plane))

-- | A prism is just a triangle.
type Prism r plane = Triangle (Vertex' (EnvVertex r plane) r plane)
-- | Prism
type Prism' v r plane = Triangle (Vertex' v r plane)

-- | A triangulated version of our (bounded) Lower envelope
type TriangulatedLowerEnvelope r plane = TriangulatedLowerEnvelope' (EnvVertex r plane) r plane

-- | A triangulated lower envelope
type TriangulatedLowerEnvelope' v r plane =
  MonoidalMap plane (NonEmpty (Prism' v r plane))


-- | Given a domain and a set of planes; compute the triangulated
-- lower envelope on the domain.
--
-- The union of all triangles covers the domain.
triangulatedLowerEnvelopeOn        :: forall set plane r.
                                      ( Plane_ plane r, Ord plane, Ord r, Fractional r
                                      , Foldable1 set
                                      -- , HasIntersectionWith (Point 2 r) (HalfPlane r plane)

                       , Show plane, Show r
                                      )
                                   => Triangle (Point 2 r)
                                   -> set plane
                                   -> TriangulatedLowerEnvelope r plane
triangulatedLowerEnvelopeOn domain = fmap triangulate . lowerEnvelopeOn domain

-- TODO: do we want to already here filter out the triangles that don't intersect
-- the domain?



-- | produce a set of triangles for the convex polygon
triangulate      :: ConvexPolygon vertex -> NonEmpty (Triangle vertex)
triangulate poly = case toNonEmptyOf vertices poly of
  v0 :| (v:vs) -> NonEmpty.zipWith (Triangle v0) (v :| vs) (NonEmpty.fromList vs)
  _            -> error "triangulate: absurd"


-- | Given a domain and a set of planes; computes for each plane the region at which it is
-- lowest.
--
-- The union of these regions covers the domain.
--
lowerEnvelopeOn        :: ( Plane_ plane r, Ord plane, Ord r, Fractional r
                          , Foldable1 set
                          -- , HasIntersectionWith (Point 2 r) (HalfPlane r plane)

                       , Show plane, Show r
                           )
                        => Triangle (Point 2 r) -> set plane -> BoundedLowerEnvelope r plane
lowerEnvelopeOn domain = fromVertices domain . bruteForceVertices


class (Point_ vertex 2 r) => EnvVertex_ vertex r plane | vertex -> plane, vertex -> r where
  -- | Get the three planes that define this vertex
  definingPlanes :: vertex -> Vector 3 plane
  -- | All planes passing through the given vertex.
  planesOf :: vertex -> NonEmpty plane


instance EnvVertex_ (EnvVertex r plane) r plane where
  definingPlanes (EnvVertex h1 h2 h3 _ _ _) = Vector3 h1 h2 h3
  planesOf = toNonEmpty


instance EnvVertex_ vertex r plane => EnvVertex_ (vertex :+ extra) r plane where
  definingPlanes = definingPlanes . view core
  planesOf = planesOf . view core


xs <<> ys = case NonEmpty.nonEmpty xs of
              Nothing  -> ys
              Just xs' -> xs' <> ys


--------------------------------------------------------------------------------

type HalfPlane r plane = HalfPlaneF (VerticalOrLineEQ r) :+ Maybe plane

mapMaybe' f = mapMaybe f . NonEmpty.toList




-- convertHalfPlane                                            :: HalfPlaneF (LinePV 2 r)
--                                                             -> HalfPlaneF (VerticalOrLineEQ r)
-- convertHalfPlane = undefined
-- -- convertHalfPlane (HalfSpace _ l@(LinePV p v@(Vector2 x y))) = HalfSpace _ ( l)
-- --   where
-- --     l' = asGeneralLine l
-- --     q  = p .+^ Vector2 y (negate x) -- point


-- h = h'&halfSpaceSign %~ \s -> if q `intersects` h' then Positive else Negative
--   where
--     l  = asGernalLine $ h^.boundingHyperPlane
--     q  = case l of
--            VerticalLineThrough x    -> Point2 (x-1) 0
--            NonVertical (LineEQ _ b) -> Point2 0 (b+1)
--     h' = HalfSpace Positive l





-- | Given a triangular domain D, a plane h, and the \(n_h\) vertices in which
-- h participates, let \(C\) be the region in which h defines the
-- lower envelope. We compute the region \(C\cap D\).
--
-- \(O(k \log k)\), where \(k\) is the total number of planes in the definers
-- of the vertices. (In general position this would thus be at most \(2n_h\).
computeCellIn             :: forall plane vertex corner r.
                                   ( Plane_ plane r, Ord plane, Ord r, Fractional r
                                   , EnvVertex_ vertex r plane, Point_ corner 2 r
                                   -- , HasIntersectionWith (Point 2 r) (HalfPlane r plane)

                                   , Show plane, Show r, Show vertex

                                   )
                          => Triangle corner
                          -> plane
                          -> NonEmpty vertex -> Maybe (ConvexPolygon (Vertex' vertex r plane))
computeCellIn domain h vs = fmap mkVertex <$> boundedCommonIntersection halfPlanes
  where
    -- | For each relevant other plane h', define the halfplane where h is cheaper than h'
    -- (and include the boundary of the trianlge)
    halfPlanes :: NonEmpty (HalfPlane r plane)
    halfPlanes = toNonEmpty ((:+ Nothing) <$> intersectingHalfPlanes domain)
              <> foldMap1 (NonEmpty.fromList . mapMaybe' asHalfPlane . planesOf) vs
                 -- note that each vertex has at least three defining
                 -- planes, one of which is h itself. So each plane
                 -- produces at least 2 halfplanes, hence the
                 -- fromList-- is safe.

    -- | A map that for each other plane h', tells us which vertices it is involved in.
    -- in particular, for each such a vertex, we will additionally store a map from its
    -- location to the vertex.
    verticesMap :: MonoidalMap plane (Map (Point 2 r) vertex)
    verticesMap = foldMap1 (\v -> let p = v^.asPoint
                                  in foldMap1 (\h' ->
                                                  MonoidalMap.singleton h' (Map.singleton p v)
                                              ) $ planesOf v
                           ) vs

    -- | given an other plane h', produce the halfplane where h is cheaper, and tag it with h'
    asHalfPlane    :: plane -> Maybe (HalfPlane r plane)
    asHalfPlane h' = intersectionLine h h' <&> \l ->
      let q = case l of
                VerticalLineThrough x    -> Point2 (x-1) 0
                NonVertical (LineEQ _ b) -> Point2 0 (b+1)
          s = if evalAt q h < evalAt q h' then Positive else Negative
            -- note: we picked q so that it lies on the side on which h is suppoed to be cheaper
      in HalfSpace s l :+ Just h'

    -- | look up the vertex associated with this particular location. (Or create an Extra)
    -- vertex if no such point exists.
    mkVertex            :: Point 2 r :+ HalfPlane r plane -> Vertex' vertex r plane
    mkVertex (p :+ mh') = fromMaybe (Extra $ p :+ evalAt p h) $ do
                            h' <- mh'^.extra
                            m  <- verticesMap MonoidalMap.!? h'
                            Original <$> m Map.!? p


-- | Given a bounded region D, and (a superset of) the set V of all
-- vertices of all faces of the lower envelope that intersect
-- D. Compute/construct the bounded lower envelope from
-- V on H. I.e. computes a set of plane,convex-region pairs (h,R_h) so
-- that:
--
-- - h is the lowest plane above R_h,
-- - the union of the R_h's is D,
-- - the R_h regions are pairwise disjoint.
--
-- O(N \log N). where N is the total number of planes involved in all vertices of V.
fromVertices        :: forall plane vertex corner r.
                       ( Plane_ plane r, Ord plane, Ord r, Fractional r
                       , EnvVertex_ vertex r plane, Point_ corner 2 r
                       -- , HasIntersectionWith (Point 2 r) (HalfPlane r plane)
                       , Show plane, Show r, Show vertex
                       )
                    => Triangle corner -> Set vertex
                    -> BoundedLowerEnvelope' vertex r plane
fromVertices domain = imapMaybe (computeCellIn domain) . foldMap collect
  where
    -- | For each plane h; collects the vertices that appear on the region corresponding to h
    collect   :: vertex -> MonoidalMap plane (NonEmpty vertex)
    collect v = foldMap (\h -> MonoidalMap.singleton h (NonEmpty.singleton v)) (planesOf v)
               -- note: this uses the Foldable instance on EndVertex; which
               -- essentially folds over the planes defining the vertex :)


otherPlanes h (Vector3 h1 h2 h3)
  | h == h1   = Vector2 h2 h3
  | h == h2   = Vector2 h1 h3
  | h == h3   = Vector2 h1 h2
  | otherwise = error "otherPlanes: unhandled degeneracy"


--------------------------------------------------------------------------------

-- | Computes the vertifces that lie strictly inside the given triangle
bruteForceVerticesIn        :: (  Plane_ plane r, Ord plane, Point_ corner 2 r
                               , Ord r, Fractional r, Foldable set)
                            => Triangle corner -> set plane -> Set (EnvVertex r plane)
bruteForceVerticesIn domain = Set.filter (`strictlyInside` domain) . bruteForceVertices


strictlyInside            :: ( Point_ point 2 r
                             , Triangle_ triangle corner
                             , Point_ corner 2 r, Ord r, Num r)
                          => point -> triangle -> Bool
q `strictlyInside` domain = inTriangle q domain == Inside

--   all ((q^.asPoint) `intersects`) (intersectingHalfPlanes domain)
-- -- FIXME!!!
  -- inPolygon q domain == Inside


-- inTriangle :: point -> triangle -> PointLocationResultWith (VertexIx triangle)
-- inTriangle = foldrMap1 id combine . imap inHalfSpace . intersectingHalfPlanes
--   where
--     combine x = \case
--       Inside -> x
--       Outside ->

--     Inside = Inside
--     combine Inside Outside =



-- -- | Test if a query point lies in a halfspace
-- inHalfSpace     :: (Point_ point d r, HalfSpace_ halfSpace d r, Ord r, Num r)
--                 => point -> halfSpace -> PointLocationResult
-- inHalfSpace q h = case q `onSideTest` (h^.boundingHyperPlane) of
--   LT -> _
--   EQ -> OnBoundary
--   GT -> _

-- -- inTriangle =

-- instance (Point_ corner 2 r
--          ) => HasInPolygon (Triangle corner) corner r where
--   inPolygon = inTriangle







-- | Compute the vertices of the lower envelope of the input planes.
bruteForceVertices :: ( Plane_ plane r, Ord plane, Ord r, Fractional r, Foldable set)
                   => set plane -> Set (EnvVertex r plane)
bruteForceVertices = withUniques mkVertex'
  where
    mkVertex' h1 h2 h3 hs = case vertexLocation3 h1 h2 h3 of
      Just v@(Point3 x y z) -> case List.partition (\h -> verticalSideTest v h == GT) hs of
        ([],nonGTs) -> let eqs = filter (\h -> verticalSideTest v h == EQ) nonGTs in
                         Set.singleton (EnvVertex h1 h2 h3 eqs (Point2 x y) z)
        _           -> mempty
      _      -> mempty
    -- the fact that the map is monoidal is somewhat meaningless here;
    -- as vertices are generated uniquely

    -- NOTE: It seems somewhat silly that we are evaluating
    -- verticalSideTest twice per plane here. However, testing if v
    -- lies above the plane requires much less precision, and GHC
    -- manages to optimize this.  In particular, this version is
    -- something like 15x faster than evaluating verticalSideTest v h
    -- exactly and using partition3 instead (as this forces the exact
    -- evaluation for many more) planes.

-- | Compute the point in which the three planes intersect
vertexLocation3          :: (Plane_ plane r, Ord r, Fractional r)
                         => plane -> plane -> plane -> Maybe (Point 3 r)
vertexLocation3 h1@(Plane_ a1 b1 c1) h2 h3 =
  (\(Point2 x y) -> Point3 x y (a1 * x + b1* y + c1)) <$> vertexLocation2 h1 h2 h3

-- | Compute the point below which the three planes intersect
vertexLocation2          :: (Plane_ plane r, Ord r, Fractional r)
                         => plane -> plane -> plane -> Maybe (Point 2 r)
vertexLocation2 h1 h2 h3 = do l12 <- intersectionLine h1 h2
                              l13 <- intersectionLine h1 h3
                              intersect l12 l13 >>= \case
                                Line_x_Line_Line _  -> Nothing
                                Line_x_Line_Point p -> Just p

--------------------------------------------------------------------------------

-- | Compute all unique triples a,b,c, rests
withUniques      :: (Monoid m, Foldable f) => (a -> a -> a -> [a] -> m) -> f a -> m
withUniques f xs = mconcat [ f x y z (concat [pref1, pref2, pref3, rest])
                           | Zipper pref1 x ys   <- allZippers (toList xs)
                           , Zipper pref2 y zs   <- allZippers ys
                           , Zipper pref3 z rest <- allZippers zs
                           ]

-- | Non-empty zipper type
data Zipper a = Zipper [a] a [a] deriving (Show)

-- | Produce all zippers of a given list
allZippers :: [a] -> [Zipper a]
allZippers = \case
  []     -> []
  (x:xs) -> Zipper [] x xs : [ Zipper (x:pref) y rest | Zipper pref y rest <- allZippers xs ]

--------------------------------------------------------------------------------


-- TODO: this should go into MonoidalMap I guess
instance Ord k => FilterableWithIndex k (MonoidalMap k)
instance Ord k => WitherableWithIndex k (MonoidalMap k)
