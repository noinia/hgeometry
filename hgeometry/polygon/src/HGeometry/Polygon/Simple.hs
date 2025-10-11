{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Simple polygon and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.Polygon.Simple
  ( SimplePolygon_(..)
  , SimplePolygon
  , SimplePolygonF(..)
  , toCyclic
  , VertexContainer
  , HasInPolygon(..)
  , inSimplePolygon
  , containedIn
  , hasNoSelfIntersections
  , module HGeometry.Polygon.Simple.Class
  ) where

import HGeometry.Ext
import Control.Lens
import Data.Foldable qualified as F
import Data.Foldable1
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import HGeometry.Boundary
import HGeometry.Cyclic
import HGeometry.Foldable.Util
import HGeometry.HalfSpace
import HGeometry.HyperPlane.Class
import HGeometry.Intersection
import HGeometry.LineSegment.Intersection.BentleyOttmann
import HGeometry.Point
import HGeometry.Line
import HGeometry.Polygon.Class
import HGeometry.Polygon.Simple.Class
import HGeometry.Polygon.Simple.Implementation
import HGeometry.Polygon.Simple.InPolygon
import HGeometry.Polygon.Simple.Type
import HGeometry.Transformation
import HGeometry.Vector.NonEmpty.Util ()
--------------------------------------------------------------------------------


-- instance VertexContainer f point => HasEdges' (SimplePolygonF f point) where
--   -- ^ An edge (v_i,v_{i+1})
--   type Edge   (SimplePolygonF f point) = (point, point)
--    -- ^ every edge (v_i,v_{i+1}) is identified by (the index of) its preceding vertex v_i
--   type EdgeIx (SimplePolygonF f point) = Int
--   edgeAt i = \pEfE pg -> indexed pEfE i (pg^?!vertexAt i, pg^?!vertexAt (succ i))
--                          <&> \(u',v') -> pg&vertexAt i        .~ u'
--                                            &vertexAt (succ i) .~ v'
--     -- we first just run the function pEfE on the particular edge in question,
--     -- we get some updated vertices out of this; which we then update appropriately.
--   {-# INLINE edgeAt #-}
--   numEdges = numVertices

-- instance VertexContainer f point
--        => HasEdges (SimplePolygonF f point) (SimplePolygonF f point) where
--   -- ^ Warning for when using this as a traversal: when applying the function on an edge
--   -- (v_i,v_{i+1}) that modifies vertex v_{i+1}, this modification is discarded.
--   edges = \pEfE pg -> let pvFv i v = fst <$> indexed pEfE i (v, pg^?!vertexAt (succ i))
--                       in vertices pvFv pg
--
-- I've uncommented these for now; since the above behaviour would be inconsistent between
-- edgeAt (which would modify both vertices), and edges, which would modify only one vertex.
-- that is a bit too weird.

instance ( VertexContainer f point
         ) => HasOuterBoundary (SimplePolygonF f point) where
  outerBoundary = _SimplePolygonF . traversed1
  outerBoundaryVertexAt i = singular (vertexAt i)

  ccwOuterBoundaryFrom i = _SimplePolygonF.traverseRightFrom i
  cwOuterBoundaryFrom  i = _SimplePolygonF.traverseLeftFrom  i

instance HasHoles (SimplePolygonF f point)

instance ( Point_ point 2 r
         , HasFromFoldable1 f
         , VertexContainer f point
         ) => Polygon_ (SimplePolygonF f point) point r where

  ccwPredecessorOf u = \pvFv pg -> let n = numVertices pg
                                       p = (pred u) `mod` n
                                       l = singular $ vertexAt p
                                   in l pvFv pg
  -- make sure to wrap the index to make sure we report the right index.
  ccwSuccessorOf   u = \pvFv pg -> let n = numVertices pg
                                       s = (succ u) `mod` n
                                       l = singular $ vertexAt s
                                   in l pvFv pg

instance ( Point_ point 2 r
         , VertexContainer f point
         , HasFromFoldable1 f
         ) => SimplePolygon_ (SimplePolygonF f point) point r where
  uncheckedFromCCWPoints = MkSimplePolygon . fromFoldable1

  fromPoints rawPts = do pts@(_:|_:_:_) <- removeRepeated <$> toNonEmpty' rawPts
                         -- note that the pattern match makes sure there are at least 3 pts
                         let pg    = uncheckedFromCCWPoints pts
                             area' = signedArea2X pg
                             pg'   = uncheckedFromCCWPoints $ NonEmpty.reverse pts
                         case area' of
                           0                      -> Nothing -- the points are all colinear
                           _ | area' == abs area' -> Just pg -- the points are given in CCW order
                             | otherwise          -> Just pg'
                             -- pts were in CW order, so we reversed them.
    where
      toNonEmpty' = NonEmpty.nonEmpty . F.toList

  -- TODO: verify that:
  --      no self intersections, and

-- -- | Make sure that we have at least three points
-- requireThree     :: NonEmpty point -> Maybe (NonEmpty point)
-- requireThree pts = case pts of
--     (_ :| (_ : _ : _)) -> Just pts
--     _                  -> Nothing


-- | Makes sure there are no repeated vertices.
--
-- note that we treat f as a cyclic sequence
removeRepeated    :: (Point_ point 2 r, Eq r)
                  => NonEmpty point -> NonEmpty point
removeRepeated = checkFirst
               . foldrMap1 (\(l :| _)         -> (l, NonEmpty.singleton l))
                           (\(x :| _) (l,acc) -> (l, x NonEmpty.<| acc))
               . NonEmpty.groupWith1 (^.asPoint)
  where
    -- make sure that the first and last element are also distinct
    checkFirst (last', acc@(first' :| rest')) = case NonEmpty.nonEmpty rest' of
      Nothing                                           -> acc
        -- Apparently there is only one element, (first' == last')
      Just rest | (first'^.asPoint) == (last'^.asPoint) -> rest
                  -- in this case the first elem of rest is distinct from first' (due to
                  -- the groupwith), and and thus distinct from the last of the rest
                | otherwise                             -> acc


instance ( Show point
         , SimplePolygon_ (SimplePolygonF f point) point r
         ) => Show (SimplePolygonF f point) where
  showsPrec = showsPrecSimplePolygon "SimplePolygon"

instance ( Read point
         , SimplePolygon_ (SimplePolygonF f point) point r
         ) => Read (SimplePolygonF f point) where
  readsPrec = readsPrecSimplePolygon "SimplePolygon"


{-
instance (SimplePolygon_ (SimplePolygonF f) point r, Fractional r, Ord r)
         => HasSquaredEuclideanDistance (SimplePolygonF f point) where
  pointClosestToWithDistance = pointClosestToWithDistanceSimplePolygon
-}

--------------------------------------------------------------------------------

_testPoly :: SimplePolygon (Point 2 Int)
_testPoly = uncheckedFromCCWPoints $ NonEmpty.fromList [Point2 10 20, origin, Point2 0 100]


--------------------------------------------------------------------------------

instance SimplePolygon_ (SimplePolygonF f point) point r
         => HasInPolygon (SimplePolygonF f point) point r

instance ( SimplePolygon_ (SimplePolygonF f point) point r
         , Num r, Ord r
         ) => HasIntersectionWith (Point 2 r) (SimplePolygonF f point) where
  q `intersects` pg = q `inSimplePolygon` pg /= StrictlyOutside

type instance Intersection (Point 2 r) (SimplePolygonF f point) = Maybe (Point 2 r)

instance ( SimplePolygon_ (SimplePolygonF f point) point r
         , Num r, Ord r
         ) => IsIntersectableWith (Point 2 r) (SimplePolygonF f point) where
  q `intersect` pg | q `intersects` pg = Just q
                   | otherwise         = Nothing
  -- this implementation is a bit silly but ok

instance ( VertexContainer f point
         , DefaultTransformByConstraints (SimplePolygonF f point) 2 r
         , Point_ point 2 r
         , IsTransformable point
         , HasFromFoldable1 f, Eq r
         ) => IsTransformable (SimplePolygonF f point) where
  transformBy t = fromMaybe err . fromPoints . fmap (transformBy t) . view _SimplePolygonF
    where
      err = error "SimplePolygonF; transformBy: no longer a simple polygon!"
  -- Note that we use fromPoints again, since the transformation may
  -- e.g. reorient the vertices; e.g. when they were given CCW before, they may now
  -- end up CW. This was actually an issue before when reading the worled file.

instance ( Point_ point 2 r, Num r, Ord r, VertexContainer f point
         , HyperPlane_ line 2 r
         ) => HasIntersectionWith (HalfSpaceF line) (SimplePolygonF f point) where
  halfPlane `intersects` poly = anyOf (vertices.asPoint) (`intersects` halfPlane) poly


--------------------------------------------------------------------------------

instance ( Num r, Ord r
         , SimplePolygon_ (SimplePolygonF nonEmpty vertex) vertex r
         )
         => LinePV 2 r `HasIntersectionWith` SimplePolygonF nonEmpty vertex where
  l `intersects` poly = case (onSide p l, onSide q l) of
                          (OnLine, _) -> True
                          (_, OnLine) -> True
                          (sp, sq)    -> sp /= sq
    where
      (p,q) = extremes (perpendicularTo l ^. direction) poly


--------------------------------------------------------------------------------

-- | verify that some sequence of points has no self intersecting edges.
hasNoSelfIntersections    :: forall f point r.
                             (Foldable1 f, Functor f, Point_ point 2 r, Ord r, Real r)
                          => f point -> Bool
hasNoSelfIntersections vs = let vs' = (\p -> (p^.asPoint)&coordinates %~ toRational) <$> vs
                                pg :: SimplePolygon (Point 2 Rational)
                                pg = uncheckedFromCCWPoints vs'
                            in Map.null $ interiorIntersections $ pg^..outerBoundaryEdgeSegments
  -- outerBoundaryEdgeSegments interiorIntersections pg



--------------------------------------------------------------------------------
-- * Instances involving Ext

instance HasIntersectionWith geom (SimplePolygonF f vertex)
         => HasIntersectionWith geom (SimplePolygonF f vertex :+ extra) where
  q `intersects` (pg :+ _) = q `intersects` pg
