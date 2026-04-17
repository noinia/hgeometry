{- HLINT ignore "Use list literal pattern" -}
module Plane.BruteForce
  ( bruteForceVertices
  , computeDomain

  , Vertex(..), location, location2


  , bruteForceTriangulatedEnvelope
  , bruteForceTriangulatedEnvelopeIn
  , TriangulatedLowerEnvelope
  , Prism
  , Vertex'(..)

  -- , allZippers
  ) where


import           Control.Lens hiding (Prism)
import           Prelude hiding (filter)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Foldable1
import           HGeometry.Map.NonEmpty.Monoidal (MonoidalNEMap)
import qualified HGeometry.Map.NonEmpty.Monoidal as MonoidalNEMap
import           Data.Foldable.WithIndex
import           Data.Foldable
import           Data.Maybe (fromMaybe)
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

import Debug.Trace
--------------------------------------------------------------------------------


instance Ord k => FilterableWithIndex k (MonoidalMap k) where

computeDomain   :: (Foldable set, Plane_ plane r, Ord r, Fractional r
                   ) => set plane -> Triangle (Point 2 r)
computeDomain _ = Triangle (Point2 x          x)
                           (Point2 x          (negate $ 2*x))
                           (Point2 (negate x) x)
  where
    x = 10000
  -- TODO: this should do something more sensible


-- | Given a triangular domain \(\Delta\), and two sets H and R of
-- planes; compute the lower envelope of R above \(\Delta\) using a
-- brute force method, and compute the conflict lists of every prism
-- w.r.t the first set H.
--
-- In particular, for each plane h we return a bunch of prisms that
-- cover the region (of \(\Delta\)) in which h is the lowest
-- plane. Note that the prisms may also cover things outside of
-- \(\Delta\).
--
-- In addition, we return a bounding box of the vertices of the lower envelope (if there are)
-- any vertices.
--
-- O(r^4 + rn)
bruteForceTriangulatedEnvelope        :: ( Plane_ plane r
                                         , Ord r, Fractional r, Ord plane
                                         , Foldable subset
                                         , Show plane, Show r -- TODO: remove
                                         )
                                      => Sample subset plane
                                      -> TriangulatedLowerEnvelope r plane
bruteForceTriangulatedEnvelope sample =
  bruteForceTriangulatedEnvelopeIn (computeDomain $ toList sample) sample



bruteForceTriangulatedEnvelopeIn                :: ( Plane_ plane r
                                                 , Ord r, Fractional r, Ord plane
                                                 , Foldable subset
                                                 , Show plane, Show r -- TODO: remove
                                                 )
                                              => Triangle (Point 2 r)
                                              -> Sample subset plane
                                              -> TriangulatedLowerEnvelope r plane
bruteForceTriangulatedEnvelopeIn domain sample =
    fmap (fmap (computeDummyConflicts $ remaining sample))
  . imapMaybe (triangulate domain) . fromVertices . bruteForceVertices $ sample


computeDummyConflicts                :: ( Plane_ plane r, Ord r, Fractional r
                                        , Filterable set, Monoid (set plane)
                                        )
                                     => set plane
                                     -> Prism r plane :+ set plane
                                     -> Prism r plane :+ set plane
computeDummyConflicts hs (tri :+ cl) = tri :+ cl <> foldMap clOf tri
  where
    clOf = \case
      Real _  -> mempty
      Dummy p -> filter (\h -> verticalSideTest p h == LT) hs


-- | A triangulated lower envelope; with conflict lists.
type TriangulatedLowerEnvelope r plane =
  MonoidalMap plane (NonEmpty (Prism r plane :+ [plane]))
-- make this a monoidalNEMap?

type Prism r plane = Triangle (Vertex' r plane)

-- | A Vertex wich may just be a dummy point
data Vertex' r plane = Dummy (Point 3 r)
                     | Real (Vertex r plane)
                     deriving (Show,Eq,Ord)

--------------------------------------------------------------------------------

-- | Given a plane h, and the k vertices bounding the region at which h appears on the
-- lower envelope; produce a list of prisms at which the vertex appears.
--
-- for each prism, we additionally compute partial conflict lists; i.e. the conflict lists
-- from the real vertices.
--
-- this may prdouce an empty list in case the plane was redundant; i.e. it only appeared
-- on the lower envelope in a point or line segment.
--
-- O(k\log k)
triangulate              :: forall plane r conflictList.
                            ( Plane_ plane r, Ord plane, Ord r, Fractional r
                            , Semigroup conflictList

                            , Show plane, Show r, Show conflictList
                            )
                         => Triangle (Point 2 r)
                         -> plane
                         -> NonEmpty (Vertex r plane :+ conflictList)
                         -> Maybe (NonEmpty (Prism r plane :+ conflictList))
triangulate domain h vs' = traceShowWith ("prisms for", h, ) $ case vs' of
    v1 :| []          -> traceShowWith ("cone",h,"->",) $ coverCone v1
    v1 :| v2 : []     -> traceShowWith ("clippedCone",h,"->",) $ coverClippedCone v1 v2
    v1 :| v2 : v3 : _ -> let p  = pointInteriorTo (Triangle (location2 $ v1^.core)
                                                            (location2 $ v2^.core)
                                                            (location2 $ v3^.core)
                                                  )
                             vs'' = NonEmpty.sortBy (cmpAround p) vs'
                         in Just $ case boundedOrUnBounded h p vs'' of
        Bounded vs       -> triangulate' (real <$> vs)
        Unbounded u v vs -> traceShowWith ("unbounded region found",h,u,v,vs,"->",) $
          coverClippedCone u v `mcons` triangulate' (real <$> vs)

  where
    mcons m xs = maybe xs (<> xs) m

    dummy q@(Point2 x y) = let z = evalAt q h in Dummy $ Point3 x y z

    -- | Compute candidate dummy corners for the prism based on the edges incident to v
    -- we also check against the extra planes to see whether we should be a vtx or not
    candidates               :: [plane] -> Vertex r plane -> [Point 2 r]
    candidates extraPlanes v = concatMap ( (filter hIsLowestAt
                                . traceShowWith ("before filtering",h,v,)
                                . (\w -> [p .+^ w, p .-^ w ]))
                               . (lambda *^)
                             )
                             . mapMaybe (intersectionVector h) $ otherPlanes
      where
        p = location2 v
        otherPlanes = List.delete h (planesOf v)
        hIsLowestAt q = let z = evalAt q h
                        in all (\h' -> traceShowWith ("eval",h,q,"---",h',z,evalAt q h',
                                                      z `compare` evalAt q h',
                                                      "->",) $
                                       z <= evalAt q h') (otherPlanes <> extraPlanes)


    lambda = 1000 -- TODO; this should somehow use the domain

    -- | Computes a prism to cover the cone defined by v.
    coverCone           :: Vertex r plane :+ conflictList
                        -> Maybe (NonEmpty (Prism r plane :+ conflictList))
    coverCone (v :+ cl) = case traceShowWith ("candidates for", h,v, " ",) $ candidates [] v of
      [a,b] -> NonEmpty.nonEmpty [Triangle (Real v) (dummy a) (dummy b) :+ cl]
      _     -> Nothing -- h only appears at the vertex

    coverClippedCone (v1 :+ cl1) (v2 :+ cl2) = case traceShowWith ("candidates v1",h,v1,) $ candidates (extras v2) v1 of
      [a] -> case traceShowWith ("candidates v2",h,v2,) $ candidates (extras v1) v2 of
               [b] -> NonEmpty.nonEmpty
                      [ Triangle (Real v1) (Real v2) (dummy a) :+ cl1 <> cl2
                      , Triangle (Real v2) (dummy a) (dummy b) :+ cl2
                      ]
               _   -> Nothing
      _   -> Nothing
      -- There should be at least one candidate dummy point for each unbounded vertex.
      -- moreover, I think there should also be only at most one.

    extras :: Vertex r plane -> [plane]
    extras = List.delete h . planesOf

    cmpAround p (u :+ _) (v :+ _) = ccwCmpAround p (location2 u) (location2 v)
    triangulate' = \case
      (v0 :+ cl0) :| (v:vs) -> NonEmpty.zipWith mkPrism (v :| vs) (NonEmpty.fromList vs)
        where
          mkPrism (v1 :+ cl1) (v2 :+ cl2) = Triangle v0 v1 v2 :+ (cl0 <> cl1 <> cl2)
      _                      -> error "triangulate': absurd"

    real = over core Real


-- | Given a plane h, a point p at which h defines the lower envelope;
-- and the vertices of the lower envelope region of h. Compute whether
-- the region is bounded or unbounded. if it is bounded; make sure
-- that the vertices the unbounded direction is between the last and
-- first vertex.
--
-- pre: there are at least 3 vertices:
boundedOrUnBounded                :: forall plane r a.
                                     (Plane_ plane r, Ord plane, Ord r, Fractional r)
                                  => plane
                                  -> Point 2 r
                                  -> NonEmpty (Vertex r plane :+ a)
                                  -> Boundary (Vertex r plane :+ a)
boundedOrUnBounded h p vs@(v1:|_) = combine $ foldr findSplit (Left (v1, [])) vs
  where
    combine = \case
      Left (_, _)            -> Bounded vs
      Right (w, suff,start') -> let vs'@(v :| _) = start' <>> suff
                                in Unbounded v w vs'

    -- we assume we are unbounded; and the goal is to find that place
    findSplit   :: Vertex r plane :+ a -> State plane r a -> State plane r a
    findSplit v = \case
      Right (w, suff, start')            -> Right (w, v NonEmpty.<| suff, start')
        -- we already found the split
      Left (u, start')
          | all hBelowAllAtQ otherPlanes -> Right (v, NonEmpty.singleton v,start')
                                            -- we found the unbounded direction
          | otherwise                    -> Left (v, v:start')
        where
          u' = location2 (u^.core)
          v' = location2 (v^.core)
          m  = u' .+^ ((1/2) *^ (v' .-. u'))
          -- midpoint on the "edge" uv
          q = m .+^ (m .-. p)
          -- q is a point just "beyond"  the edge uv as seen from p; i.e.
          -- q lies outside the cell corresponding to h.
          otherPlanes = Set.delete h $ planesOf' (u^.core) <> planesOf' (v^.core)
          -- collect whatever planes u and v have in common (other than h).
          hBelowAllAtQ h' = z <= evalAt q h'
          z = evalAt q h
  -- the main idea is as follows: for every consecutive pair of
  -- vertices v_i v_i+1 in the cyclic order compute the midpoint m;
  -- then consider the vector w from p to m, and let q = m + w then if
  -- h is still cheaper than the other planes then we are unbounded in
  -- between v_i and v_i+1

    xs <>> ys = case NonEmpty.nonEmpty xs of
                  Nothing  -> ys
                  Just xs' -> xs' <> ys

-- | The state while trying to find whether we are bounded or unbounded
type State plane r a =
  Either ( Vertex r plane :+ a -- the right neighbour of v_i
         , [Vertex r plane :+ a] -- all vertices [v_{i+1},..v_n]
         )
         ( Vertex r plane :+ a -- the last vertex before the gap
         , NonEmpty (Vertex r plane :+ a) -- the suffix; i.e. the last
           -- vertices before the gap

         , [Vertex r plane :+ a] -- the start vertices that should still
           -- be added to the list.
         )

data Boundary vertex = Unbounded vertex vertex (NonEmpty vertex)
                     -- ^ the two vertices incident to the unbounded edges
                     -- and the list of all vertices (including those unbounded ones)
                     | Bounded (NonEmpty vertex)

--------------------------------------------------------------------------------

-- | A (not so great) representation of the Lower envelope;
--
-- the vertices bounding a plane are given in arbitrary order.
--
-- (this assumes there is at least one vertex) in the lower envelope.
type LowerEnvelope r plane = MonoidalMap plane (NonEmpty (Vertex r plane :+ [plane]))

-- |
-- Given the vertices of the lower envelope; compute the envelope itself; i.e. for every
-- plane, compute the vertices at which it appears on the lower envelope.
--
--
-- O(h\log h), where \(h\) is the complexity of the envelope.
fromVertices :: Ord plane => MonoidalMap (Vertex r plane) [plane] -> LowerEnvelope r plane
fromVertices = ifoldMap $ \v cl -> let v' = NonEmpty.singleton $ v :+ cl
                                   in MonoidalMap.fromList [ (h,v') | h <- planesOf v ]
  -- TODO: what do we do with empty maps
  -- TODO: verify that the monoidal combinations are not too expensive.
  --
  -- we may want to use some DList like thing rather than NonEmpty? to guarantee O(1)
  -- time (<>)

-- | Comutes the lower envelope of the sampled planes as well as the conflict lists
-- of every vertex.
--
-- O(r^4 + rn)
bruteForceEnvelope :: (Plane_ plane r, Ord r, Fractional r, Ord plane, Foldable subset)
                   => Sample subset plane -> LowerEnvelope r plane
bruteForceEnvelope = fromVertices . bruteForceVertices

{-
-- | Comutes the lower envelope of the sampled planes as well as the conflict lists
-- of every vertex.
--
-- O(r^4 + rn)
bruteForceTriangulatedLowerEnvelope :: Sample subset plane -> TriangulatedLowerEnvelope plane
bruteForceTriangulatedLowerEnvelope = imap triangulate . bruteForceEnvelope

-}



-- --------------------------------------------------------------------------------

-- -- | Given a list hs of halfplanes that define the boundary of their
-- -- (non-empty) common intersection (in CCW orientation) (and a point
-- -- inside this common intersection). Compute whether the comon
-- -- intersection is bounded or unbounded. In ase it is unbounded, return the planes
-- -- in CCW order (i.e. so that the first and last halfplane bound the unbounded region)
-- boundedOrUnbounded :: ( HalfPlane_ halfPlane 2 r, Ord r, Num r
--                       , Foldable1 nonEmpty
--                       )
--                    => nonEmpty halfPlane
--                    -> Point 2 r
--                    -> Either (NonEmpty halfPlane)
--                              (Cyclic nonEmpty halfPlane)
-- boundedOrUnbounded hs _ = undefined

-- asCone   :: Vertex r plane :+ [plane] -> Maybe (Cone r (Vertex r plane :+ [plane]) plane)
-- asCone v = undefined




--------------------------------------------------------------------------------

-- | a vertex is defined by at least three planes.
--
-- The Eq and Ord instances only consider these three defining planes,
-- and assume that they are ordered in increasing order (in some
-- global order defined on the planes).
data Vertex r plane = Vertex !plane !plane !plane
                             [plane] -- ^ remaining defining planes ; purposly lazy
                             (Point 3 r) -- ^ this field is purposly lazy
                    deriving (Show,Foldable,Functor,Generic)

-- | Smart constructor for constructing a Vertex.
--
-- pre: the three defining planes are given in increasing order
mkVertex             :: (Plane_ plane r, Ord r, Fractional r)
                     => plane -> plane -> plane -> [plane] -> Maybe (Vertex r plane)
mkVertex h1 h2 h3 hs = Vertex h1 h2 h3 hs <$> vertexLocation3 h1 h2 h3

instance Eq plane => Eq (Vertex r plane) where
  (Vertex u v w _ _) == (Vertex u' v' w' _ _) = u == u' && v == v' && w == w'

instance Ord plane => Ord (Vertex r plane) where
  (Vertex u v w _ _) `compare` (Vertex u' v' w' _ _) =
    u `compare` u' <> v `compare` v' <> w `compare` w'

instance (NFData r, NFData plane) => NFData (Vertex r plane)

-- instance IsBoxable (Vertex r plane)




-- | Report all planes passing through a vertex (even possibly redundant ones)
planesOf :: Vertex r plane -> [plane]
planesOf = toList

-- | Report all planes passing through a vertex (even possibly redundant ones)
planesOf' :: Ord plane => Vertex r plane -> Set plane
planesOf' = Set.fromList . planesOf

-- | Compute the exact location of a vertex
location                     :: (Plane_ plane r, Ord r, Fractional r)
                             => Vertex r plane -> Point 3 r
location (Vertex _ _ _ _ v) = v

-- | Compute the projection of of a vertex
location2 :: (Plane_ plane r, Ord r, Fractional r) => Vertex r plane -> Point 2 r
location2 = projectPoint . location

-- (Vertex h1 h2 h3 _) = fromMaybe (error "location2: absurd, no intersection?")
--                                           (vertexLocation2 h1 h2 h3)

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

-- | Given a subset \(R \subseteq H\) of \(r\) out of \(n\) planes,
-- compute the vertices of the lower envelope of R as well as their
-- conflict lists (w.r.t \(H\)) using a brute force method.
--
-- pre: the planes are given in increasing order
--
-- running time: \(O(r^4 + rn)\)
bruteForceVertices                      :: ( Plane_ plane r, Ord r, Fractional r
                                           , Foldable subset, Ord plane
                                           )
                                        => Sample subset plane
                                        -> MonoidalMap (Vertex r plane) [plane]
bruteForceVertices (Sample rs _ rest _) = withUniques mkVertex' rs
  where
    mkVertex' h1 h2 h3 hs = case vertexLocation3 h1 h2 h3 of
      Just v -> case List.partition (\h -> verticalSideTest v h == GT) hs of
                  ([],nonGTs) -> let eqs = filter (\h -> verticalSideTest v h == EQ) nonGTs in
                                 MonoidalMap.singleton (Vertex h1 h2 h3 eqs v)
                                                       [ h | h <- rest, verticalSideTest v h == GT ]
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

  -- TODO: If the input set is actually given in sorted order, the
  -- vertices are also generated in sorted order. So then the unioning
  -- (due to the semigroup) is not needed; and we can just use fromStrictAscList


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


-- foo xs = [(x,y,zs) | (x:ys@(y:zs)) <- tails xs]
-- -- test = foo [1..5]

-- test :: [(Int,Int,Int,[Int])]
-- test = withUniques (\a b c rest -> [(a,b,c,rest)]) [1..5]
