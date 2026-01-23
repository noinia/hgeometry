module Plane.BruteForce
  ( bruteForceVertices


  , Vertex(..), location, location2

  -- , allZippers
  ) where

import Data.Foldable.WithIndex
import Data.Foldable
import Data.Maybe (fromMaybe)
import Plane.Sample
import HGeometry.Kernel
import HGeometry.HyperPlane.Class
import HGeometry.Ext
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.List qualified as List
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MonoidalMap
import HGeometry.Combinatorial.Util
import Data.List (inits, tails)
import HGeometry.List.Util
import Data.Map (Map)
import Data.Map qualified as Map
import HGeometry.Plane.LowerEnvelope.Connected.Primitives
import Control.DeepSeq
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- data Prism r plane


{-
-- | Given two sets H and R of planes; compute the lower envelope of R using a brute force
-- method, and compute the conflict lists of every vertex w.r.t the first set H.
--
-- O(r^4 + rn)
bruteForceEnvelope                       :: Sample subset plane ->
                                            ( TriangulatedEnvelope r plane
                                            , Box (Point 2 r)
                                            )
bruteForceEnvelope (Sample rs _ rest _) = (env, bBox)
  where
    env = undefined
    bBox = undefined

-}

-- type TriangulatedLowerEnvelope plane = Map plane


data Prism r plane = Triangular (Vertex r plane) (Vertex r plane) (Vertex r plane)
                   | Cone (Vertex r plane)
                   | ClippedCone (Vertex r plane) (Vertex r plane)






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


-- | Given a plane h, and the k vertices bounding the region at which h appears on the
-- lower envelope; produce a list of prisms at which the vertex appears.
--
-- for each prism, we additionally compute its (partial) conflict list.
-- partial in the sense that we include only the conflict lists at vertices of the prism.
-- so for unbounded prisms cells may be missing.
--
-- this may prdouce an empty list in case the plane was redundant; i.e. it only appeared
-- on the lower envelope in a point or line segment.
--
-- O(k\log k)
triangulate       :: plane -> NonEmpty (Vertex r plane :+ [plane]) -> [Prism r plane :+ [plane]]
triangulate = undefined
-- triangulate h vs' = case vs' of
--   v1 :| []          -> _ -- cone or nothing
--   v1 :| v2 : []     -> _  -- clipped cone or nothing
--   v1 :| v2 : v3 : _ -> let p  = undefined  -- some point in the triangle
--                            vs = sortAround p vs'
--                        in


data Boundary plane = Unbounded (NonEmpty plane)
                    | Bounded (NonEmpty plane)


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

-- | Report all planes passing through a vertex (even possibly redundant ones)
planesOf :: Vertex r plane -> [plane]
planesOf = toList

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
