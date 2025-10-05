{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
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
  , UnboundedConvexRegionF(..), chain
  , extremalVertices
  , mapChain
  , boundedCore
  , boundingRays
  , unboundedBoundingHalfplanes

  , toBoundedFrom
  ) where

import HGeometry.Small.TwoOrThree
import Control.Lens
import Data.Foldable1
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Ord (comparing)
import HGeometry.Box as Box
import HGeometry.Cyclic
import HGeometry.HalfLine
import HGeometry.Foldable.Util
import HGeometry.HalfSpace
import HGeometry.HyperPlane.Class
import HGeometry.Intersection
import HGeometry.Line
import HGeometry.Point
import HGeometry.Polygon
import HGeometry.LineSegment
import HGeometry.LineSegment.PossiblyDegenerate
import HGeometry.Polygon.Simple.PossiblyDegenerate
import HGeometry.Properties
import HGeometry.Triangle as Triangle
import HGeometry.Vector
import Data.Kind (Type)
import GHC.Generics (Generic)
import Control.DeepSeq

--------------------------------------------------------------------------------

-- | An unbounded polygonal Convex Region
type UnboundedConvexRegion vertex = UnboundedConvexRegionF (NumType vertex) NonEmpty vertex

-- | An unbounded polygonal ConvexRegion whose vertices are stored in an 'nonEmpty'
data UnboundedConvexRegionF r nonEmpty (vertex :: Type) =
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
  deriving stock (Show,Eq,Functor,Foldable,Traversable,Generic)

type instance NumType   (UnboundedConvexRegionF r nonEmpty vertex) = r
type instance Dimension (UnboundedConvexRegionF r nonEmpty vertex) = 2

instance (NFData r, NFData (nonEmpty vertex)
         ) => NFData (UnboundedConvexRegionF r nonEmpty vertex)

-- | Lens to access the chain of vertices in CCW order
chain :: Lens (UnboundedConvexRegionF r nonEmpty vertex)
              (UnboundedConvexRegionF r nonEmpty' vertex')
              (nonEmpty vertex) (nonEmpty' vertex')
chain = lens (\(Unbounded _ chain' _) -> chain')
             (\(Unbounded u _ v) chain' -> Unbounded u chain' v)

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
                                      -> Vector 2 (HalfLine vertex)
boundingRays chain'@(Unbounded v _ w) = compute $ case extremalVertices chain' of
                                          Left p   -> Vector2 p p
                                          Right vs -> vs
  where
    compute (Vector2 p q) = Vector2 (HalfLine p $ negated v) (HalfLine q w)


-- | Computes the core of the unbounded region; i.e. the convex hull of the
-- vertices of the region.
boundedCore                     :: ( VertexContainer nonEmpty vertex, Point_ vertex 2 r
                                   , HasFromFoldable1 nonEmpty
                                   )
                                => UnboundedConvexRegionF r nonEmpty vertex
                                -> PossiblyDegenerateSimplePolygon vertex
                                        (ConvexPolygonF (Cyclic nonEmpty) vertex)
boundedCore (Unbounded _ pts _) = case toNonEmpty pts of
  (u :| [])  -> DegenerateVertex u
  (u :| [v]) -> DegenerateEdge $ ClosedLineSegment u v
  _          -> ActualPolygon $ uncheckedFromCCWPoints pts


-- | the 2 or three halfplanes bounding the unbounded part of the region
-- in particular: the region minus its bounded core
unboundedBoundingHalfplanes :: (Point_ vertex 2 r, Num r, Ord r)
                            => UnboundedConvexRegionF r NonEmpty vertex
                            -> TwoOrThree (HalfPlaneF (LinePV 2 r))
unboundedBoundingHalfplanes region@(Unbounded v _ w) =
    TwoOrThree . bimap two three $ extremalVertices region
  where
    two (view asPoint -> p) = Two (leftHalfPlane $ LinePV p v) (leftHalfPlane $ LinePV p w)
    three (fmap (view asPoint) -> Vector2 p q) =
      Three (leftHalfPlane $ LinePV p v        )
            (leftHalfPlane $ LinePV q w        )
            (leftHalfPlane $ LinePV p (q .-. p))
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

instance ( Traversable1 nonEmpty
         , Ixed (nonEmpty vertex), IxValue (nonEmpty vertex) ~ vertex
         , Index (nonEmpty vertex) ~ Int
         ) => HasVertices' (UnboundedConvexRegionF r nonEmpty vertex) where
  type Vertex   (UnboundedConvexRegionF r nonEmpty vertex) = vertex
  type VertexIx (UnboundedConvexRegionF r nonEmpty vertex) = Int
  -- I think we could get rid of the Int constraint here, and just use an arbitrary
  -- Index type. In that case, we need a slightly more general version of 'traversed1'
  -- to implement 'vertices' in the 'HasVertices' instance.
  vertexAt i = chain .> iix i
  numVertices = length . view chain

instance ( Traversable1 nonEmpty
         , Ixed (nonEmpty vertex)
         , IxValue (nonEmpty vertex) ~ vertex
         , IxValue (nonEmpty vertex') ~ vertex'
         , Index (nonEmpty vertex) ~ Int, Index (nonEmpty vertex') ~ Int
         ) => HasVertices (UnboundedConvexRegionF r nonEmpty vertex)
                          (UnboundedConvexRegionF r nonEmpty vertex') where
  vertices = chain .> traversed1

--------------------------------------------------------------------------------

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
-- * Intersection of a Point and a Unbounded Convex Polygon




instance (Point_ vertex 2 r, Ord r, Fractional r
         ) => Point 2 r `HasIntersectionWith` UnboundedConvexRegionF r NonEmpty vertex where
  q `intersects` region = all (q `intersects`) (unboundedBoundingHalfplanes region)
                       || q `intersects` boundedCore region

--------------------------------------------------------------------------------
-- * Intersection of a LineSegment with an Unbounded Convex Polygon

-- type instance Intersection (LineSegment endPoint point)
--                            (UnboundedConvexRegionF r nonEmpty vertex) =
--    Intersection (LineSegment endPoint point)
--                 (ConvexPolygonF (Cyclic nonEmpty) vertex)

-- data instance Intersection (LineSegment endPoint point)
--                            (ConvexPolygonF (Cyclic nonEmpty) vertex) =


instance (Point_ vertex 2 r, Ord r, Fractional r
         ) => LinePV 2 r
                `HasIntersectionWith` UnboundedConvexRegionF r NonEmpty vertex where
  line `intersects` region = any (line `intersects`) (boundingRays region)
                          || line `intersects` boundedCore region

-- | Intersection between a halfline and a unbounded convex region
data LineUnboundedConvexRegionIntersection r =
    Line_x_UnboundedConvexRegion_HalfLine    (HalfLine (Point 2 r))
  | Line_x_UnboundedConvexRegion_LineSegment (ClosedLineSegment (Point 2 r))
  | Line_x_UnboundedConvexRegion_Point       (Point 2 r)
  deriving (Show,Eq)

instance (Ord r, Num r
         ) => Point 2 r `HasIntersectionWith` LineUnboundedConvexRegionIntersection r where
  intersects q = \case
    Line_x_UnboundedConvexRegion_HalfLine    hl  -> q `intersects` hl
    Line_x_UnboundedConvexRegion_LineSegment seg -> q `intersects` seg
    Line_x_UnboundedConvexRegion_Point       p   -> p == q


type instance Intersection (LinePV 2 r) (UnboundedConvexRegionF r nonEmpty vertex) =
  Maybe (LineUnboundedConvexRegionIntersection r)





-- boundingRayIntersections line rays = case withIntersection <$> rays of

--   where
--     withIntersection ray = (,ray) <$> line `intersect` ray

-- withIntersection <$> boundingRays

instance (Point_ vertex 2 r, Ord r, Fractional r
         ) => LinePV 2 r
                `IsIntersectableWith` UnboundedConvexRegionF r NonEmpty vertex where
  line `intersect` region = case withIntersection <$> boundingRays region of
      -- somehow both rays intersect the line
      Vector2 (Just (ps,_)) (Just (qs,_)) -> Just $ case ps of
        Line_x_HalfLine_HalfLine hl -> Line_x_UnboundedConvexRegion_HalfLine hl
        Line_x_HalfLine_Point p     -> case qs of
          Line_x_HalfLine_HalfLine hl         -> Line_x_UnboundedConvexRegion_HalfLine hl
          Line_x_HalfLine_Point q | p == q    -> Line_x_UnboundedConvexRegion_Point p
                                  | otherwise -> Line_x_UnboundedConvexRegion_LineSegment
                                                   $ ClosedLineSegment p q

      -- the rays do not intersect the line
      Vector2 Nothing Nothing                 -> boundedIntersection <&> \case
         SinglePoint p     -> Line_x_UnboundedConvexRegion_Point p
         ActualSegment seg -> Line_x_UnboundedConvexRegion_LineSegment seg

      -- exactly one ray intersects the line
      Vector2 ps Nothing               -> intersectBounded <$> ps
      Vector2 Nothing  qs              -> intersectBounded <$> qs
    where
      withIntersection                :: HalfLine vertex
                                      -> Maybe (LineHalfLineIntersection (Point 2 r) (HalfLine (Point 2 r))
                                               ,HalfLine (Point 2 r)
                                               )
      withIntersection (HalfLine p v) = let ray = HalfLine (p^.asPoint) v
                                        in (,ray) <$> line `intersect` ray

      boundedIntersection = line `intersect` boundedCore region

      intersectBounded (ps,ray) = case ps of
          Line_x_HalfLine_HalfLine l  -> Line_x_UnboundedConvexRegion_HalfLine l
          Line_x_HalfLine_Point p     -> case boundedIntersection of
                Nothing                   -> Line_x_UnboundedConvexRegion_HalfLine $ hl p
                Just (SinglePoint q)      -> Line_x_UnboundedConvexRegion_LineSegment $
                                               ClosedLineSegment p q
                Just (ActualSegment seg)  -> let q   = maximumBy cmp seg
                                                 cmp = comparing (squaredEuclideanDist p)
                                                 res = ClosedLineSegment p q
                                             in Line_x_UnboundedConvexRegion_LineSegment res
                                     -- the closest endpoint of seg must lie on
                                     -- the dummy edge of the core (connecting the two extremal)
                                     -- vertices of the chain
        where
          hl p = let v  = line^.direction
                     v' = if p .+^ v `liesLeftOf` ray then v else negated v
                 in HalfLine p v'
          q `liesLeftOf` l = q `onSide` asOrientedLine l == LeftSide


instance ( Point_ vertex 2 r, Point_ point 2 r, Ord r, Fractional r
         , IxValue (endPoint point) ~ point, EndPoint_ (endPoint point)
         , HasOnSegment        (LineSegment endPoint point) 2
         , HasIntersectionWith (ClosedLineSegment (Point 2 r)) (LineSegment endPoint point)
         , HasIntersectionWith (HalfLine (Point 2 r))          (LineSegment endPoint point)
         ) => LineSegment endPoint point
                `HasIntersectionWith` UnboundedConvexRegionF r NonEmpty vertex where
  seg `intersects` region = case supportingLine seg `intersect` region of
      Nothing     -> False
      Just inters -> case inters of
        Line_x_UnboundedConvexRegion_Point p          -> p    `intersects` seg
        Line_x_UnboundedConvexRegion_LineSegment seg' -> seg' `intersects` seg
        Line_x_UnboundedConvexRegion_HalfLine  hl     -> hl   `intersects` seg
        -- TODO: conceviably this could be faster, since we already know they are colinear
        -- so we just ahve to test if the endpoint ordering is ok;
    -- where
    --   seg = view asPoint <$> seg0
      -- (seg^.start.asPoint)  `intersects` seg'
      --         || (seg^.end.asPoint)    `intersects` seg'
      --         || pointOf seg'          `intersects` seg
      -- where
      --         || (seg'^.start.asPoint) `intersects` seg
      --         || (seg'^.end.asPoint)   `intersects` seg

--------------------------------------------------------------------------------
-- * Intersection of a Triangle with an Unbounded Convex Polygon

type instance Intersection (Triangle corner) (UnboundedConvexRegionF r nonEmpty vertex)
  = Intersection (Triangle corner) (ConvexPolygonF (Cyclic nonEmpty) vertex)

instance (Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `HasIntersectionWith` UnboundedConvexRegionF r NonEmpty vertex where
  tri `intersects` region = anyOf (vertices.asPoint) (`intersects` tri) region
                         || anyOf (vertices.asPoint) (`intersects` region) tri
                         || anyOf outerBoundaryEdgeSegments (`intersects` region) (view asPoint <$> tri)



instance ( Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Triangle corner `IsIntersectableWith` UnboundedConvexRegionF r NonEmpty vertex where
  tri `intersect` region = tri `intersect` toBoundedFrom tri region

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
        l  = lineThrough (p .-^ v) (q .+^ w) :: LinePV 2 _
        h' = HalfSpace Negative l
        h  = if q' `intersects` h' then h'&halfSpaceSign .~ Positive else h'
        q' = (q .+^ (w ^+^ w))^.asPoint
          -- make sure that there is at least one point outside the halfplane
        pt = maximumOn (`squaredEuclideanDistTo` h) (q' :| tri^..folded.asPoint)
          -- the furthest point in the direction perpendicular to the halfplane
        l' = l&anchorPoint .~ (pt .+^ w) -- move even a bit further to make sure
                                         -- that the new vertices (a,b) we get are strictly
                                         -- outside the input triangle tri
        a  = intersectionPoint p v l' -- no need to negate v here
        b  = intersectionPoint q w l'


    maximumOn f = maximumBy (comparing f)

    intersectionPoint p v' l = case LinePV (p^.asPoint) v' `intersect` l of
      Just (Line_x_Line_Point a) -> p&xCoord .~ a^.xCoord
                                     &yCoord .~ a^.yCoord
      _                          -> error "intersectionPoint: absurd'"

-- -- TODO: this would make for a good property test: test if the points all lie inside the reegion


--------------------------------------------------------------------------------
-- * Intersection of a Rectangle with an Unbounded Convex Polygon

type instance Intersection (Rectangle corner) (UnboundedConvexRegionF r nonEmpty vertex)
  = Intersection (Rectangle corner) (ConvexPolygonF (Cyclic nonEmpty) vertex)

instance (Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Rectangle corner `HasIntersectionWith` UnboundedConvexRegionF r NonEmpty vertex

instance ( Point_ vertex 2 r, Point_ corner 2 r, Ord r, Fractional r
         ) => Rectangle corner `IsIntersectableWith` UnboundedConvexRegionF r NonEmpty vertex where
  rect `intersect` region = rect `intersect` toBoundedFrom (Box.corners rect) region



--------------------------------------------------------------------------------



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
