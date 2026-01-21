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
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal qualified as MonoidalMap
import HGeometry.Combinatorial.Util
import Data.List (inits, tails)
import HGeometry.List.Util
import Data.Map (Map)
import Data.Map qualified as Map
import HGeometry.Plane.LowerEnvelope.Connected.Primitives

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


data Prism plane = Triangular (Vertex plane) (Vertex plane) (Vertex plane)
                 | Cone (Vertex plane)
                 | ClippedCone (Vertex plane) (Vertex plane)






-- | A (not so great) representation of the Lower envelope;
--
-- the vertices bounding a plane are given in arbitrary order.
--
-- (this assumes there is at least one vertex) in the lower envelope.
type LowerEnvelope plane = MonoidalMap plane (NonEmpty (Vertex plane :+ [plane]))


-- |
-- Given the vertices of the lower envelope; compute the envelope itself; i.e. for every
-- plane, compute the vertices at which it appears on the lower envelope.
--
--
-- O(h\log h), where \(h\) is the complexity of the envelope.
fromVertices :: Ord plane => MonoidalMap (Vertex plane) [plane] -> LowerEnvelope plane
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
                   => Sample subset plane -> LowerEnvelope plane
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
triangulate       :: plane -> NonEmpty (Vertex plane :+ [plane]) -> [Prism plane :+ [plane]]
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
data Vertex plane = Vertex plane plane plane [plane]
                  deriving (Show,Eq,Ord,Foldable,Functor)

-- | Report all planes passing through a vertex (even possibly redundant ones)
planesOf :: Vertex plane -> [plane]
planesOf = toList

-- | Compute the exact location of a vertex
location                     :: (Plane_ plane r, Ord r, Fractional r)
                             => Vertex plane -> Point 3 r
location (Vertex h1 h2 h3 _) = fromMaybe (error "location: absurd, no intersection?")
                                         (vertexLocation3 h1 h2 h3)

-- | Compute the projection of of a vertex
location2                     :: (Plane_ plane r, Ord r, Fractional r)
                              => Vertex plane -> Point 2 r
location2 (Vertex h1 h2 h3 _) = fromMaybe (error "location2: absurd, no intersection?")
                                          (vertexLocation2 h1 h2 h3)

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
-- running time: \(O(r^4 + rn)\)
bruteForceVertices                      :: ( Plane_ plane r, Ord r, Fractional r
                                           , Foldable subset, Ord plane
                                           )
                                        => Sample subset plane -> MonoidalMap (Vertex plane) [plane]
bruteForceVertices (Sample rs _ rest _) = withUniques mkVertex rs
  where
    mkVertex h1 h2 h3 hs = case partition3 (belowVertex' h1 h2 h3) hs of
                             (_,eqs,[]) -> MonoidalMap.singleton (Vertex h1 h2 h3 eqs)
                                                                 (filter (\h -> belowVertex h1 h2 h3 h
                                                                        == Just GT
                                                                 ) rest)
                                           -- the fact that the map is monoidal is somewhat
                                           -- meaningless here; as vertices are generated uniquely
                             _          -> mempty
      where
        mv                  = vertexLocation3 h1 h2 h3
        -- returns whether the given forth plane h passes below (GT),
        -- through (EQ), or above (LT) the common vertex defined by
        -- the three given planes. Returns GT if the first three
        -- planes don't define a common vertex.
        belowVertex' ha hb hc h = fromMaybe GT $ belowVertex ha hb hc h
        belowVertex  _  _  _  h = (\v -> verticalSideTest v h ) <$> mv

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
