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

  -- , computeDomain



  -- , Vertex(..), location, location2


  -- , bruteForceTriangulatedEnvelope
  -- , bruteForceTriangulatedEnvelopeIn
  -- , TriangulatedLowerEnvelope
  -- , Prism
  -- , Vertex'(..)

  -- , allZippers

  , coverCone
  , coverClippedCone

  , findMissingEdge
  -- , findRotateTo
  ) where

import           Data.Foldable
import           HGeometry.HalfLine
import           Control.Lens hiding (Prism, Prism')
import           Prelude hiding (filter)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Foldable1
import           HGeometry.Map.NonEmpty.Monoidal (MonoidalNEMap)
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

-- | Given a bounded region D, and (a superset of) the set V of all
-- vertices of all faces of the lower envelope that intersect
-- D. Compute/construct the bounded lower envelope from
-- V. I.e. computes a set of plane,convex-region pairs (h,R_h) so
-- that:
--
-- - h is the lowest plane above R_h,
-- - the union of the R_h's cover D,
-- - on the domain D, the R_h regions are pairwise disjoint.
--
--
-- (In case the vertices also define faces disjoint from D; these may
-- also still be included. In particular, the current implementation
-- may include bounded faces that are disjoint from D.)
--
-- O(n\log n).
fromVertices        :: forall plane vertex corner r.
                       ( Plane_ plane r, Ord plane, Ord r, Fractional r
                       , EnvVertex_ vertex r plane, Point_ corner 2 r
                       , Show plane, Show r, Show vertex
                       )
                    => Triangle corner -> Set vertex
                    -> BoundedLowerEnvelope' vertex r plane
fromVertices domain = imapMaybe computeCell . foldMap collect
  where
    -- | For each plane h; collects the vertices that appear on the region corresponding to h
    collect   :: vertex -> MonoidalMap plane (NonEmpty vertex)
    collect v = foldMap (\h -> MonoidalMap.singleton h (NonEmpty.singleton v)) (planesOf v)
               -- note: this uses the Foldable instance on EndVertex; which
               -- essentially folds over the planes defining the vertex :)

    sortAroundBoundary (v0 :| xs) = v0 :| List.sortBy (ccwCmpAround v0) xs

    -- | For a plane; compute the vertices in CCW order around the its boundary, and
    -- extend to cover the domain
    computeCell :: plane -> NonEmpty vertex -> Maybe (ConvexPolygon (Vertex' vertex r plane))
    computeCell h (sortAroundBoundary -> vs'@(v0:|rest')) = evalAtExtras $
        case NonEmpty.nonEmpty rest' of
          Nothing   -> let f                       = evalAt (v0 .+^ w1)
                           Vector2 (h1,w1) (h2,w2) = boundingPlanes v0
                           (left,right)            = if f h < f h2
                                                     then (negated w2 :+ h2, w1 :+ h1)
                                                     else (negated w1 :+ h1, w2 :+ h2)
                             -- if v0 + w1 lies on the h-side of the intersection-line of
                             -- h and h2; then apparently w1 is the right vector, and thus
                             -- w2 is the (incoming) left boundary vector. (Which we negate
                             -- since to construct the cone we need an outgoing vector).
                             --
                             -- otherwise, w1 is the incoming left
                             -- boundary ray in reverse direction. and we negate it appropriately.
                       in coverCone domain (Cone v0 left right)
          Just rest -> case findMissingEdge isMissingEdge v0 rest of
            Nothing                     -> Just $ uncheckedFromCCWPoints (Original <$> vs')
              -- no missing edge, so we have a bounded convex polygon
            Just (al, intermediate, ar) ->
                let -- the left vector should be pointing away from al. so we compute
                    -- the vectors corresponding to the two defining halfplanes.
                    -- these vectors are so that h is cheaper to the left of the vectors.
                    -- so, we evaluate h2 at ar+w1. If at this point, h2 is cheaper than
                    -- h. this means that negated w1 is the left bounding vector of our ray.
                    -- otherwise. negated w2 is our (incoming) left bounding vector here.
                    vl = negated $ let Vector2 (_h1,w1) (h2,w2) = boundingPlanes al
                                       f = evalAt (al .+^ w1)
                                   in if f h2 < f h then w1 else w2

                    -- for the right bounding vector this similar: if at
                    -- ar+w1 h2 is cheaper than h; vector w1 must be the
                    -- incoming vector at ar (as h is cheaper on the
                    -- left of this vector), (and thus w2 must be the
                    -- outgoing vector). Otherwise, w1 is indeed the outgoing vector.
                    vr = let Vector2 (_h1,w1) (h2,w2) = boundingPlanes ar
                             f = evalAt (ar .+^ w1)
                         in if f h2 < f h then w2 else w1
                in coverUnbounded domain (HalfLine al vl) intermediate (HalfLine ar vr)

      where
        -- | for the Extra vertices, evaluate their height value
        evalAtExtras = over (_Just.vertices._Extra) (\q -> q :+ evalAt q h)

        -- | Given a vertex, compute the two planes that together with h define this vertex.
        -- (as well as for both planes) the direction vector so that h is to the left
        -- of the line with this direction vector (through v).
        boundingPlanes   :: vertex -> Vector 2 (plane, Vector 2 r)
        boundingPlanes v = fromMaybe err . (\h' -> (h',) <$> intersectionVector h h')
                        <$> otherPlanes h (definingPlanes v)
        err = error "absurd: fromVertices. planes don't intersect !?"





        -- extra' p = Extra $ p :+ evalAt p h


        -- | Given two neighbouring vertices u and v test if v is
        -- really a CCW neighbor of u; i.e. if uv is an edge of the
        -- polygon, or not. This function returns True if uv is *not* an edge.
        isMissingEdge     :: vertex -> vertex -> Bool
        isMissingEdge u v = case otherPlane u v of
          Nothing -> True
          Just h' -> let Vector2 x y = v .-. u
                         w           = Vector2 y (-x)
                         f           = evalAt (u .+^ w)
                     in f h < f h'
            -- w should be a vector pointing into the right halfplane of the edge uuv
           -- if uv is a CCW edge; then h should be cheaper just left of the
           -- edge. Conversely, it should be more expensive on the right side of the edge

        -- | Given two vertices u and v, compute the plane other than
        -- h that they have in common (if any)
        otherPlane     :: vertex -> vertex -> Maybe plane
        otherPlane u v = let f = foldMap Set.singleton . planesOf in
                         case toList $ Set.delete h (f u `Set.intersection` f v) of
                           [h'] -> Just h'
                           []   -> Nothing
                           xs    -> traceStack (show ("multiple",h,u,v,xs)) $
                             error "otherPlane: Multiple planes intersecting in a line?"


--------------------------------------------------------------------------------


-- | Given a triangle D and an convex unbounded region C; given by its
-- left bounding ray bl, its intermediate vertices (in left to right
-- order), and its right bounding ray br. Compute a convex region R
-- that contains \(C \cap D\).
--
-- The convex region R will be a convex region contain the vertices
-- and two points v_l and v_r on the bounding rays.
--
-- pre: the starting points of the two rays are disjoint
--      the intermediate vertices do not contain the starting points of the rays
--
-- note: this will return Nothing iff \(C \cap D\) is empty.
coverUnbounded :: forall vertex sequence corner r.
                    ( Point_ vertex 2 r, Point_ corner 2 r
                    , Ord r, Fractional r
                    )
                 => Triangle corner
                 -> HalfLine vertex -> [vertex] -> HalfLine vertex
                 -> Maybe (ConvexPolygon (OriginalOrExtra vertex (Point 2 r)))
coverUnbounded domain (HalfLine al vl) intermediate (HalfLine ar vr) = do
    -- let w be the direction perpendicular to the segment al,ar (and pointing into the cone)
    --
    -- our region will be a trapezoid with corners al, ar, vr, vl
    -- where the segment vl,vr is parallel to al,ar, and passes through
    -- the furthest point q in the direction w.
    let v'@(Vector2 x y) = ar .-. al
        w = Vector2 (-y) x -- the direction pointing into the cone

    q <- maximumByOf folded (cmpInDirection2 w) domain

    let m = LinePV (q^.asPoint) v' -- the line through the furthest point
        -- helper to compute the vertices on the rays
        f a v = LinePV (a^.asPoint) v `intersect` m >>= \case
          Line_x_Line_Point p -> Just (Extra p)
          _                   -> Nothing -- this should not really happen
    l <- f al vl
    r <- f ar vr

    let origs  = Original <$> al :| intermediate ++ [ar]
    pure $ uncheckedFromCCWPoints $ origs <> (r :| [l])


-- | Given a triangle D and a clipped cone C; given by it's left bounding ray bl and
-- its right boundring ray br. Compute a convex region R that contains \(C \cap D\).
--
-- The convex region R will be a quadrilateral contain the starting
-- point of the two rays and two points v_l and v_r on the rays.
--
-- pre: the starting points of the two rays are disjoint
--
-- note: this will return Nothing iff \(C \cap D\) is empty.
coverClippedCone                   :: forall apex corner r.
                                      ( Point_ apex 2 r, Point_ corner 2 r
                                      , Ord r, Fractional r
                                      )
                                   => Triangle corner
                                   -> HalfLine apex -> HalfLine apex
                                   -> Maybe (ConvexPolygon (OriginalOrExtra apex (Point 2 r)))
coverClippedCone domain left right = coverUnbounded domain left [] right


-- | Given a triangle D and a cone C; Compute a convex region R that contains \(C \cap D\).
--
-- The convex region R will be a quadrilateral contain the apex, two
-- points v_l and v_r on the rays, and the intersection of the lines perpendicular to
-- the rays.
--
-- note: this will return Nothing iff \(C \cap D\) is empty.
coverCone          :: forall apex corner r e.
                      ( Point_ apex 2 r, Point_ corner 2 r
                      , Ord r, Fractional r
                      )
                   => Triangle corner
                   -> Cone r apex e
                   -> Maybe (ConvexPolygon (OriginalOrExtra apex (Point 2 r)))
coverCone domain c = do
    -- we compute the maximum point q_l in direction of the left bounding ray l; and take
    -- the line m_l through this point perpendicular to the ray. This line intersects
    -- the ray in some defining point v_l. We do the same for the right boundary ray r.
    -- these two lines intersect in a point w. We add v_l, w, and v_r as additional vertices.

    -- all points in \(C \cap D\) will be left of m_l, and left of m_r; hence contained
    -- the output region
    let a = c^.apex
        basePt v@(Vector2 x y) = do q <- maximumByOf folded (cmpInDirection2 v) domain
                                    let m = LinePV (q^.asPoint) (Vector2 (-y) x)
                                        -- the direction of this line is perpendicular to v
                                    (,m) <$> intersectionPoint (LinePV (a^.asPoint) v) m

        -- compute the intersection point of two lines; assuming it exists
        intersectionPoint l m = l `intersect` m >>= \case
          Line_x_Line_Point p -> Just (Extra p)
          _                   -> Nothing -- this should not really happen

    (vl,ml) <- basePt (c^.leftBoundaryVector.core)
    (vr,mr) <- basePt (c^.rightBoundaryVector.core)
    w       <- intersectionPoint ml mr

    pure $ uncheckedFromCCWPoints $ Original a :| [vr, w, vl]

--------------------------------------------------------------------------------



-- | Try to find the missing edge (where the predicate somehow can
-- test whether the pair of subsequent vertices define an edge).
--
-- This function actually returns a triple (v0, [v_1,..,v_k], v_k+1)
-- of the vertices in sequence along the boundary v0,v_1,..,v_k,v_k+1,
-- so that the "unbounded edge" is the edge between v_k+1 and v_0
findMissingEdge          :: (vertex -> vertex -> Bool)
                         -> vertex -> NonEmpty vertex
                         -> Maybe (vertex, [vertex], vertex)
findMissingEdge p v0 vs' = fmap fst . find snd $
    NonEmpty.zipWith (\u (v :| intermediates) -> ((v, take k intermediates, u), p u v)) vs rests
  where
    vs    = v0 NonEmpty.<| vs'
    rests = NonEmpty.tails1 $ vs' <> vs
    k     = length vs' - 1



{-
-- | rotate so that we start with an element that satisfies the predicate.
findRotateTo   :: (a -> Bool) -> NonEmpty a -> Maybe (NonEmpty a)
findRotateTo p = fmap f . traverse NonEmpty.nonEmpty . NonEmpty.break p
  where
    f (pref, v0 :| suff) = v0 :| suff <> pref
-}





      -- undefined




      -- case NonEmpty.nonEmpty vs0 of
      -- Nothing  -> undefined -- cone
      -- Just vs1@(v1:|vs2) -> case NonEmpty.nonEmpty vs2 of
      --   Nothing           -> undefined -- clipped cone
      --   Just vs@(v3 :| _) -> undefined -- at least three


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
q `strictlyInside` domain = all ((q^.asPoint) `intersects`) (intersectingHalfPlanes domain)
-- FIXME!!!
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
