{- HLINT ignore "Use list literal pattern" -}
module Plane.BruteForce
  ( bruteForceVertices
  , lowerEnvelopeOn
  , triangulatedLowerEnvelopeOn
  , TriangulatedLowerEnvelope
  , BoundedLowerEnvelope
  , Prism
  , Vertex'
  , EnvVertex(..), location, extraDefiners
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
  , findUnbounded
  , findRotateTo
  ) where

import           Control.Lens hiding (Prism)
import           Prelude hiding (filter)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Foldable1
import           HGeometry.Map.NonEmpty.Monoidal (MonoidalNEMap)
import qualified HGeometry.Map.NonEmpty.Monoidal as MonoidalNEMap
import           Data.Foldable.WithIndex
import           Data.Foldable(Foldable(..))
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
import           Data.Map (Map)
import Data.Map qualified as Map
import           HGeometry.Plane.LowerEnvelope.Connected.Primitives
import           Control.DeepSeq
import           GHC.Generics (Generic)
import           Data.Functor.WithIndex
import           Witherable
import           HGeometry.Cyclic (Cyclic)
import Control.Applicative


import Plane.Debug
import HGeometry.Polygon
import HGeometry.Point.Either
import Data.Bifunctor
import HGeometry.Cone
import Data.Ord




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

instance (Num r, Ord r) => HasIntersectionWith (EnvVertex r plane) (Triangle (Point 2 r)) where
  v `intersects` t = (v^.asPoint) `intersects` t

instance Foldable1 (EnvVertex r) where
  foldMap1 f (EnvVertex h1 h2 h3 hs _ _) = let z = f h1 <> f h2 <> f h3
                                           in maybe z (z <>) (foldMap (Just . f) hs)


planesOf :: EnvVertex r plane -> NonEmpty plane
planesOf = toNonEmpty

--------------------------------------------------------------------------------

-- | Vertices in our bounded envelopes are either real vertices, or dummy vertices
type Vertex' r plane = OriginalOrExtra (EnvVertex r plane) (Point 2 r :+ r)

-- | The lower envelope for a bounded domain is a mapping from planes
-- to the regions in which they are lowest. Since the input domain is
-- bounded; these regions are bounded convex polygons.
type BoundedLowerEnvelope r plane =
  MonoidalMap plane (ConvexPolygon (Vertex' r plane))

-- | A prism is just a triangle.
type Prism r plane = Triangle (Vertex' r plane)

-- | A triangulated version of our (bounded) Lower envelope
type TriangulatedLowerEnvelope r plane =
  MonoidalMap plane (NonEmpty (Prism r plane))


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
triangulate      :: ConvexPolygon (Vertex' r plane) -> NonEmpty (Prism r plane)
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



xs <<> ys = case NonEmpty.nonEmpty xs of
              Nothing  -> ys
              Just xs' -> xs' <> ys

-- | Given a bounded region and a set of vertices, compute the bounded lower envelope
-- from them.
--
-- O(n\log n).
fromVertices        :: forall plane r.
                       ( Plane_ plane r, Ord plane, Ord r, Fractional r
                       , Show plane, Show r
                       )
                    => Triangle (Point 2 r) -> Set (EnvVertex r plane)
                    -> BoundedLowerEnvelope r plane
fromVertices domain = imap computeCell . foldMap collect
  where
    -- | For each plane h; collects the vertices that appear on the region corresponding to h
    collect   :: EnvVertex r plane -> MonoidalMap plane (NonEmpty (EnvVertex r plane))
    collect v = foldMap (\h -> MonoidalMap.singleton h (NonEmpty.singleton v)) v
               -- note: this uses the Foldable instance on EndVertex; which
               -- essentially folds over the planes defining the vertex :)

    sortAroundBoundary (v0 :| xs) = v0 :| List.sortBy (ccwCmpAround v0) xs

    -- | For a plane; compute the vertices in CCW order around the its boundary, and
    -- extend to cover the domain
    computeCell                                        :: plane
                                                       -> NonEmpty (EnvVertex r plane)
                                                       -> ConvexPolygon (Vertex' r plane)
    computeCell h (sortAroundBoundary -> vs'@(v0:|rest'))
      | traceShow ("computeCell",h,toList vs') False = undefined
      | otherwise
      =

        uncheckedFromCCWPoints $ (extra' <$> extras) <<> (Original <$> originals)
      where
        -- | The vertices in sorted order around some arbitrary first vertex.
        (originals, extras) = case NonEmpty.nonEmpty rest' of
          Nothing   -> let a                      = v0^.asPoint
                           f                      = evalAt (v0 .+^w2)
                           Vector2 (h1,w1) (_,w2) = boundingPlanes v0
                           (w1',w2')              = if f h < f h1 then (w1,w2) else (w2,w1)
                       in (vs', coverCone'' a w1' a w2')
          Just rest -> case traceShowWith ("UB",) $ findUnbounded isUnboundedEdge v0 rest of
            Nothing        -> (vs', [])
            Just vs@(u:|_) -> (vs, extraVertices u $ NonEmpty.last vs)

        extra' p = Extra $ p :+ evalAt p h

        -- | Computes extra vertices required to cover the (clipped) cone with apices al and ar.
        -- al is the vector pointing into al. ar is the vector pointing away from ar.
        -- both vectors have h to their left.
        coverCone''             :: Point 2 r -> Vector 2 r -> Point 2 r -> Vector 2 r
                                -> [Point 2 r]
        coverCone'' al wl ar wr = toList $ coverCone' domain al wl ar wr

        -- | Given a vertex, compute the two planes that together with h define this vertex.
        -- (as well as for both planes) the direction vector so that h is to the left
        -- of the line with this direction vector (through v).
        boundingPlanes   :: EnvVertex r plane -> Vector 2 (plane, Vector 2 r)
        boundingPlanes v = traceShowWith ("boundingPlanes",h,v,"->",) $
          fromMaybe err
                           . (\h' -> (h',) <$> intersectionVector h h')
                        <$> otherPlanes h v
        err = error "absurd: fromVertices. planes don't intersect !?"

        -- | Given two neighbouring vertices u and v test if v is
        -- really a CCW neighbor of u; i.e. if uv is an edge of the
        -- polygon, or not. This function returns True if uv is *not* an edge.
        isUnboundedEdge     :: EnvVertex r plane -> EnvVertex r plane -> Bool
        isUnboundedEdge u v
          -- | traceShow (u /= v ) False

          = traceShowWith ("isUnboundedEdge",u,v,"->",) $

          case otherPlane u v of
          Nothing -> True
          Just h' -> let Vector2 x y = v .-. u
                         w           = traceShowWith ("w",u,v,"->",) $ Vector2 y (-x)
                         f           = evalAt (traceShowWith ("loc",) $ u .+^ w)
                     in f h < f h'
            -- w should be a vector pointing into the right halfplane
            -- of the edge uuv
           -- if uv is a CCW edge; then h should be cheaper just left of the
           -- edge. Conversely, it should be more expensive on the right side of the edge

                        -- test if really lies left of the vector from v to u.

        -- | Given two vertices u and v, compute the plane other than
        -- h that they have in common (if any)
        otherPlane     :: EnvVertex r plane -> EnvVertex r plane -> Maybe plane
        otherPlane u v = let f = foldMap Set.singleton in
                         case traceShowWith ("otherPlane",h,u,v,"->",) $
                              toList $ Set.delete h (f u `Set.intersection` f v) of
                           [h'] -> Just h'
                           []   -> Nothing
                           xs    -> traceStack (show ("multiple",h,u,v,xs)) $
                             error "otherPlane: Multiple planes intersecting in a line?"

        -- | Computes the additional vertices (when we are in the unbounded case)
        --
        -- u is the first vertex of the chain and v is the last vertex of the chain.
        extraVertices     :: EnvVertex r plane -> EnvVertex r plane -> [Point 2 r]
        extraVertices u v
          | traceShow ("extraVertices",u, v) False = undefined
          | otherwise
          = coverCone'' (u^.asPoint) (dir u (>)) (v^.asPoint) (dir v (<))
          where
            dir z cmp = let Vector2 (h1,w1) (h2,w2) = boundingPlanes z
                            f = evalAt (z .+^ w1)
                        in if f h `cmp` f h2 then w1 else w2
            -- for the left vector, the vector should be incoming at u
            -- so at u + w1 h should be more expensive than h2.
            -- ( and equally expensive as h1)
            -- for the right vector, the vector should be outgoing at u
            -- so at u + w1 h should be cheaper then h2.




--------------------------------------------------------------------------------

-- | cover the clipped cone.
coverClippedCone                         :: forall apex r. ( Point_ apex 2 r, Ord r, Num r
                                                  , Show r
                                                  )
                                     => Triangle (Point 2 r)
                                     -> apex -> Vector 2 r -> apex -> Vector 2 r
                                     -> ConvexPolygon (OriginalOrExtra apex (Point 2 r))
coverClippedCone domain al leftV ar rightV =
  let al' = al^.asPoint
      ar' = ar^.asPoint
  in uncheckedFromCCWPoints $
     (Extra <$> coverCone' domain al' leftV ar' rightV) <>
     (Original <$> al :| [ar])

-- | Given the domain, and a cone; given by its apex, its left vector,
-- and its right vector (both given so that the cone is to the left of
-- the vectors).  compute a convex polygon of contant complexity that
-- covers the cone
coverCone :: forall apex r. (Point_ apex 2 r, Ord r, Num r
             , Show apex, Show r
             )
          => Triangle (Point 2 r) -> apex -> Vector 2 r -> Vector 2 r
          -> ConvexPolygon (OriginalOrExtra apex (Point 2 r))
coverCone domain a leftV rightV =
  let a' = a^.asPoint
  in uncheckedFromCCWPoints $
     (Extra <$> coverCone' domain a' leftV a' rightV) <> NonEmpty.singleton (Original a)

-- | computes the vertices of the clipped cone cover.
coverCone'                           :: forall r. ( Ord r, Num r
                                                  , Show r
                                                  )
                                     => Triangle (Point 2 r)
                                     -> Point 2 r -> Vector 2 r -> Point 2 r -> Vector 2 r
                                     -> NonEmpty (Point 2 r)
coverCone' domain al leftV ar rightV
  | traceShow ("coverCone'",al,leftV,".....",ar,rightV) False = undefined
  | otherwise
  = r :| mp <> [l]
  where
    Vector2 h1 h2  = leftHalfPlane <$> Vector2 (LinePV al leftV)
                                               (LinePV ar rightV)

    left'' = negated leftV

    l' = maximumBy (cmpInDirection2 left'') domain
    r' = maximumBy (cmpInDirection2 rightV) domain

    l = projectOnto al left'' l'
    r = projectOnto ar rightV r'

    -- the halfplane not containing v
    h = leftHalfPlane $ LinePV l (r .-. l)

    -- the corners of the domain that are in the cone, and still on the wrong side of the
    -- halfplane defined by l and r
    mp = List.sortBy (ccwCmpAroundWith rightV ar)
       $ filter (\q -> all (q `intersects`) [h1,h2,h]) (toList domain)

    -- we are overestimating the length of the vector from q to a and using that
    projectOnto          :: Point 2 r -> Vector 2 r -> Point 2 r -> Point 2 r
    projectOnto a base q = let b  = quadrance (q .-. a) *^ base
                           in a .+^ b






--------------------------------------------------------------------------------

-- | Try to find the unbounded edge (where the predicate somehow can
-- test whether the pair of subsequent vertices define an edge).
findUnbounded          :: (vertex -> vertex -> Bool)
                       -> vertex -> NonEmpty vertex
                       -> Maybe (NonEmpty vertex)
findUnbounded p v0 vs' = let vs = v0 NonEmpty.<| vs'
                         in fmap (fmap fst)
                          . findRotateTo snd
                          $ NonEmpty.zipWith (\u v -> (v, p u v)) vs (vs' <> vs)



-- | rotate so that we start with an element that satisfies the predicate.
findRotateTo   :: (a -> Bool) -> NonEmpty a -> Maybe (NonEmpty a)
findRotateTo p = fmap f . traverse NonEmpty.nonEmpty . NonEmpty.break p
  where
    f (pref, v0 :| suff) = v0 :| suff <> pref






      -- undefined




      -- case NonEmpty.nonEmpty vs0 of
      -- Nothing  -> undefined -- cone
      -- Just vs1@(v1:|vs2) -> case NonEmpty.nonEmpty vs2 of
      --   Nothing           -> undefined -- clipped cone
      --   Just vs@(v3 :| _) -> undefined -- at least three


otherPlanes h (EnvVertex h1 h2 h3 _ _ _)
  | h == h1   = Vector2 h2 h3
  | h == h2   = Vector2 h1 h3
  | h == h3   = Vector2 h1 h2
  | otherwise = error "otherPlanes: unhandled degeneracy"













--------------------------------------------------------------------------------

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
