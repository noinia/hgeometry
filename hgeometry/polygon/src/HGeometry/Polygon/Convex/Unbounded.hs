{-# LANGUAGE UndecidableInstances  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Convex.Unbounded
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A type for representing unbounded, Convex, polygonal regions.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Convex.Unbounded
  ( UnboundedConvexRegion
  , UnboundedConvexRegionF(..)
  , extremalVertices
  , mapChain


  , toBoundedFrom
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import           HGeometry.Cyclic
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.Point.Either
import           HGeometry.Polygon
import           HGeometry.Polygon.Simple.PossiblyDegenerate
import           HGeometry.Properties
import           HGeometry.Triangle
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | An unbounded polygonal Convex Region
type UnboundedConvexRegion vertex = UnboundedConvexRegionF (NumType vertex) NonEmpty vertex

-- | An unbounded polygonal ConvexRegion whose vertices are stored in an 'nonEmpty'
data UnboundedConvexRegionF r nonEmpty vertex =
  Unbounded (Vector 2 r)
            -- ^ vector indicating the direction of the unbounded edge
            -- incident to the first vertex. Note that this vector
            -- thus points INTO vertex v.
            (nonEmpty vertex)
            -- ^ the vertices in CCW order,
            (Vector 2 r)
            -- ^ the vector indicating the direction of the unbounded
            -- edge incident to the last vertex. The vector points
            -- away from the vertex (i.e. towards +infty).
  deriving stock (Show,Eq,Functor,Foldable,Traversable)

type instance NumType   (UnboundedConvexRegionF r nonEmpty vertex) = r
type instance Dimension (UnboundedConvexRegionF r nonEmpty vertex) = 2

-- | map a function over the sequence of points
mapChain                       :: (nonEmpty vertex -> nonEmpty' vertex')
                               -> UnboundedConvexRegionF r nonEmpty vertex
                               -> UnboundedConvexRegionF r nonEmpty' vertex'
mapChain f (Unbounded v pts w) = Unbounded v (f pts) w

-- | Compute the first and last vertex of the chain. Returns a Left if the first and last
-- are the same.
extremalVertices                            :: UnboundedConvexRegionF r NonEmpty vertex
                                            -> Either vertex (Vector 2 vertex)
extremalVertices (Unbounded _ (p :| pts) _) = case NonEmpty.nonEmpty pts of
                                                Nothing   -> Left p
                                                Just pts' -> Right $ Vector2 p (NonEmpty.last pts')

-- | Computes the two bounding rays of the unbounded region. Note that the rays now are
-- both pointing toward infinity. The second ray has the unbounded region to its left,
-- whereas the first one has it to its right.
boundingRays                          :: (Point_ vertex 2 r, Num r)
                                      => UnboundedConvexRegionF r NonEmpty vertex
                                     -> (Vector 2 (HalfLine vertex))
boundingRays chain@(Unbounded v _ w) = compute $ case extremalVertices chain of
                                     Left p   -> Vector2 p p
                                     Right vs -> vs
  where
    compute (Vector2 p q) = Vector2 (HalfLine p $ negated v) (HalfLine q w)


{-
data Cone r apex = Cone { _apex                   :: apex
                        , _leftBoundingDirection  :: Vector 2 r
                        , _rightBoundingDirection :: Vector 2 r
                        }
                 deriving (Show,Eq)
makeLenses ''Cone

type instance Dimension (Cone r apex) = 2
type instance NumType   (Cone r apex) = r
-}



--------------------------------------------------------------------------------

-- newtype VerticalLine r = VerticalLineThrough r
--   deriving (Show,Read,Eq,Ord,Functor,Foldable,Traversable)

-- type instance NumType   (VerticalLine r) = r
-- type instance Dimension (VerticalLine r) = 2

{-

-- TODO: for this to be useful we probably want to know *which* edges the half line intersects
data LineUnboundedConvexRegionIntersection r vertex =
    Line_x_UnboundedConvexRegion_BoundedEdge (ClosedLineSegment vertex)
  | Line_x_UnboundedConvexRegion_UnBoundedEdge (HalfLine vertex)
  | Line_x_UnboundedConvexRegion_HalfLine      (HalfLine (Point 2 r))
  | Line_x_UnboundedConvexRegion_Tangent vertex
  | Line_x_UnboundedConvexRegion_LineSegment (ClosedLineSegment (Point 2 r))
  -- deriving (Show,Eq)

type instance Intersection (LinePV 2 r) (UnboundedConvexRegionF r nonEmpty vertex)
  = AtMostTwo (Point 2 r)

instance ( Point_ vertex 2 r, Num r, Ord r
         ) => LinePV 2 r `IsIntersectableWith` UnboundedConvexRegionF r NonEmpty vertex where
  line `intersect` reg@(Unbounded _ pts' _) = case (line `intersect'`) <$> boundingRays reg of
      Vector2 Nothing Nothing   -> findIntersections Zero
      Vector2 (Just p) (Just q) -> Two p q
      Vector2 (Just p) Nothing  -> findIntersections (One p)
      Vector2 Nothing  (Just q) -> findIntersections (One q)
    where
      pts    = (^.asPoint) <$> pts'
      edges' = zipWith OpenLineSegment (F.toList pts) (NonEmpty.tail pts)
      findIntersections = findIntersectingVertices  . findIntersectingEdges
      -- | Finds intersection with edges
      findIntersectingEdges z = foldr (\e acc -> case line `intersect` e of
                                               Nothing -> acc
                                               Just p  -> inc p acc
                                  ) z edges'
      findIntersectingVertices z = foldr (\p acc -> if p `intersect` line then inc p acc else acc
                                         ) z pts

      inc p = \case
        Zero  -> One p
        One a -> Two p a
        two   -> two

instance ( Point_ vertex 2 r, Num r, Ord r
         ) => LinePV 2 r `HasIntersectionWith` UnboundedConvexRegionF r NonEmpty vertex where
  line `intersects` reg = case line `intersect` reg of
                            Zero -> True
                            _    -> False



-}


-- type BoundedComponent nonEmpty r vertex =
--   PossiblyDegenerateSimplePolygon vertex (ConvexPolygonF nonEmpty (OriginalOrCanonical vertex))


-- data HalfPlaneUnboundedConvexIntersection r nonEmpty vertex =
--     HalfPlane_x_UnboundedConvexRegion_Bounded
--        (BoundedComponent nonEmpty r vertex)
--   | HalfPlane_x_UnboundedConvexRegion_Unbounded
--        (UnboundedConvexRegionF r nonEmpty (OriginalOrCanonical vertex))


-- deriving instance ( Show r, Show vertex, Point_ vertex 2 r
--                   , SimplePolygon_ (ConvexPolygonF nonEmpty (OriginalOrCanonical vertex))
--                                    (OriginalOrCanonical vertex)
--                   )
-- =>
--   Show (HalfPlaneUnboundedConvexIntersection r nonEmpty vertex)

-- type instance Intersection (HalfPlaneF line) (UnboundedConvexRegionF r nonEmpty vertex)
--   = Maybe (HalfPlaneUnboundedConvexIntersection r nonEmpty vertex)

-- instance ( Point_ vertex 2 r, Num r, Ord r
--          , HyperPlane_ line 2 r
--          , HalfLine vertex `HasIntersectionWith` HalfPlaneF line
--          ) => HalfPlaneF line `IsIntersectableWith` UnboundedConvexRegionF r NonEmpty vertex where
--   h `intersect` (Unbounded u chain v) = undefined



--     do (u',chain0) <- trimChain  tri u chain
--                                            (v',chain') <- trimRChain tri v chain0
--                                            pure

-- c

--     go v pts


--     where

-- -- | Trims the chain; making sure the remaining vertices lie in the convex region
-- trimChain     :: (Point_ point 2 r, Num r, Ord r, Point 2 r `HasIntersectionWith` convexRegion)
--               => convexRegion -- ^ Convex region we are intersecting
--               -> Vector 2 r  -- ^ Vector into the first vertex of the chain
--               -> NonEmpty point -- ^ Convex Chain of vertices
--               -> Maybe (Vector 2 r, NonEmpty point) -- ^ Vector into the vertex of the chain
--               -- and the first point on the chain that lies in the covnex region
-- trimChain tri = go
--   where
--     go u chain@(p :| rest)
--       | (p^.asPoint) `intersects` tri = Just (u,chain)
--       | otherwise                     = case NonEmpty.nonEmpty rest of
--                                           Nothing           -> Nothing
--                                           Just rest'@(q:|_) -> go (q .-. p) rest'

-- trimRChain       :: (Point_ corner 2 r, Point_ point 2 r, Num r, Ord r)
--                  => Triangle corner
--                  -> Vector 2 r -> NonEmpty point -> Maybe (Vector 2 r, NonEmpty point)
-- trimRChain tri v = either (const Nothing) Just . go
--   where
--     -- main idea: we compute either the result (i.e. the vector and the chain), or
--     -- the first point of the suffix  that lies outside the triangle
--     go chain@(p :| rest) = case NonEmpty.nonEmpty rest of
--       Nothing
--         | (p^.asPoint) `intersects` tri -> Right (v,chain)
--         | otherwise                     -> Left p
--       Just rest'                        -> case go rest' of
--         Left q
--           | (p^.asPoint) `intersects` tri -> Right (q .-. p, NonEmpty.singleton p)
--           | otherwise                     -> Left p
--         Right (w,result)                  -> Right (w, p NonEmpty.<| result)
--   -- TODO: hmm, come to think of it; can't the chain enter/exit the triangle multiple times?




-- (Unbounded u chain v)
--                             -> do (u',chain0) <- trimChain  tri u chain
--                                   (v',chain') <- trimRChain tri v chain0


instance ( Point_ vertex 2 r, Num r, Ord r
         , HyperPlane_ line 2 r
         , HalfLine vertex `HasIntersectionWith` HalfPlaneF line
         ) => HalfPlaneF line `HasIntersectionWith` UnboundedConvexRegionF r NonEmpty vertex where
  h `intersects` reg@(Unbounded _ pts _) = any (`intersects` h) (boundingRays reg)
                                        || anyOf (folded.asPoint) (`intersects` h) pts








-- -- | given a vertical line l through point (x,0) that intersects the unbounded convex region
-- -- intersect the convex region with the left halfplane of l.
-- intersectVerticalLeft :: r -> UnboundedConvexRegionF r nonEmpty vertex
--                            -> Either (UnboundedConvexRegionF r nonEmpty vertex)
--                                      (ConvexPolygonF (Cyclic NonEmpty) vertex)
-- intersectVerticalLeft x chain@(Chain v pts w) =
--   case (verticalLine x `intersect`) <$> boundingRays chain of
--     Vector2 Nothing Nothing


--------------------------------------------------------------------------------

type instance Intersection (Triangle corner) (UnboundedConvexRegionF r nonEmpty vertex)
  = Intersection (Triangle corner) (ConvexPolygonF (Cyclic nonEmpty) vertex)

instance (Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `HasIntersectionWith` (UnboundedConvexRegionF r NonEmpty vertex)

instance ( Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `IsIntersectableWith` (UnboundedConvexRegionF r NonEmpty vertex) where
  tri `intersect` region = tri `intersect` (toBoundedFrom tri region)

-- | Given some convex shape S, construct some "big enough" convex polygon B out of the
-- unbounded convex polygon U so that the intersection \(U \cap S\) is the same as \(B
-- \cap S\)
--
-- note: this creates some new vertices; which are "copies" from the extremal vertices.
-- this is to avoid having to introduce yet another level of 'OriginalOrExtra''s
toBoundedFrom :: (Foldable nonEmpty, Point_ point 2 r, Point_ vertex 2 r, Ord r, Fractional r)
              => nonEmpty point -> UnboundedConvexRegionF r NonEmpty vertex
              -> ConvexPolygonF (Cyclic NonEmpty) vertex
toBoundedFrom tri reg@(Unbounded v pts w) = go $ case extremalVertices reg of
                                                   Left p    -> Vector2 p p
                                                   Right vec -> vec

  where
    -- the main idea is to compute the line through two points on the boundary of the of
    -- halflines bounding the region, and consider the halfplane h that is bounded by this
    -- line l and contains the vertices of the unbounded region. We now find the furthest
    -- point pt from h (among the points in tri), and construct a line l' through this
    -- furthest point. It follows all points in tri lie "right" of this line (on the same
    -- side as the vertices of our unbounded region). Hence, we can safely clip the region
    -- using this line l'. (So we compute the intersection point between the two bounding
    -- rays and l') to find the two additional vertices a and b that we have to insert.

    go (Vector2 p q) = uncheckedFromCCWPoints $ b NonEmpty.<| a NonEmpty.<| pts
      where
      l  = lineThrough (p .+^ negated v) (q .+^ w) :: LinePV 2 _
      h' = HalfSpace Negative l
      h  = if q' `intersects` h' then h'&halfSpaceSign .~ Positive else h'
      q' = (q .+^ (w ^+^ w))^.asPoint
        -- make sure that there is at least one point outside the halfplane
      pt = maximumOn (`squaredEuclideanDistTo` h) (q' :| tri^..folded.asPoint)
        -- the furthest point in the direction perpendicular to the halfplane
      l' = l&anchorPoint .~ pt
      a  = intersectionPoint p v l' -- no need to negate v here
      b  = intersectionPoint q w l'


    maximumOn f = maximumBy (comparing f)

    intersectionPoint p v' l = case (LinePV (p^.asPoint) v') `intersect` l of
                                 Just (Line_x_Line_Point a) -> p&xCoord .~ a^.xCoord
                                                                &yCoord .~ a^.yCoord
                                 _                          -> error "intersectionPoint: absurd'"

-- -- TODO: this would make for a good property test: test if the points all lie inside the reegion


  -- case traverse (`intersect` box) rays of
  --   Just (Vector2 (HalfLine_x_Box_LineSegment s1)
  --                 (HalfLine_x_Box_LineSegment s2)) -> f (s1^.end) (s2^.end)
  --   _ -> error "toBoundedFrom: absurd"
  -- where
  --   (a,qSide) = intersectionPoint r1
  --   (b,pSide) = intersectionPoint r2


  --   box = boundingBox $ boxRay r1 <> boxRay r2 <> toNonEmptyOf folded tri
  --   rays@(Vector2 r1 r2) = boundingRays reg

  --   boxRay r = NonEmpty.fromList [ r^.start, r^.start .+^ r^.direction ]

  --   f a b = uncheckedFromCCWPoints $ case cornerInBetween b a box of
  --             Nothing -> b NonEmpty.<|               a NonEmpty.<| pts
  --             Just c  -> b NonEmpty.<| c NonEmpty.<| a NonEmpty.<| pts





-- -- | Given a ray that starts inside the box box, compute the edge (and its index) on
-- -- which the ray intersects (and thus exits) the box
-- --
-- -- pre: the ray starts in the rectangle!
-- intersectionPoint       :: (Point_ corner 2 r, Fractional r, Ord r)
--                         => HalfLine line -> Rectangle corner
--                         -> (Point 2 r :+ VertexIx (Rectangle corner))
-- intersectionPoint r box = case getFirst $ intersectionPoint' r of
--                             Nothing -> error "intersectionPoint: precondititon failed "
--                             Just x  -> x
--   where
--     intersectionPoint' r = flip (ifoldMapOf outerBoundaryEdgeSegments) box $ \(side,_) seg ->
--       case r `intersect` seg of
--         Just (HalfLine_x_LineSegment_Point x) -> First $ Just (x :+ side)
--         _                                     -> First   Nothing


-- -- | Given two vertex indices, computes the list of vertices in between them.
-- cornersInBetween         :: ( HasVertices triangle triangle, Eq (VertexIx triangle)
--                             , Point_ (Vertex triangle) 2 r
--                             )
--                          => VertexIx triangle -> VertexIx triangle
--                          -> triangle -> [Point 2 r]
-- cornersInBetween s e tri = map (^._2.asPoint)
--                          . takeWhile ((/= e) . fst) . dropWhile ((/= s) . fst)
--                          $ cycle (itoListOf vertices tri)


-- -- | Given two points a and b on the sides of the box, computes the first CCW corner in
-- -- between a and b on the box (considered from a)
-- cornerInBetween a b box = case


--   case extremalVertices reg of
--     Right (Vector2 p q) -> compute p q
--     Left p              -> compute p p
--   where
--     box =










--------------------------------------------------------------------------------
