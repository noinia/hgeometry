{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.Line
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional lines.
--
--------------------------------------------------------------------------------
module Geometry.Line
  ( Line(Line), anchorPoint, direction
  , lineThrough
  , verticalLine, horizontalLine
  , perpendicularTo
  , bisector

  , fromLinearFunction, toLinearFunction

  , isPerpendicularTo
  , isParallelTo, isParallelTo2
  , isIdenticalTo

  , onLine, onLine2
  , pointAt

  , toOffset, toOffset'

  , HasSupportingLine(..)

  , SideTestUpDown(..)
  , OnSideUpDownTest(..)
  , SideTest(..)
  , onSide, liesAbove, liesBelow

  , cmpSlope
  ) where

import           Control.Lens
import           Data.Bifunctor
import           Data.Ext
import           Geometry.Boundary
import           Geometry.Box
import           Geometry.Line.Internal
import           Geometry.Line.Class
import           Geometry.LineSegment
import           Geometry.Point.Internal
import           Geometry.Properties
import           Geometry.SubLine
import           Geometry.Transformation
import           Geometry.Vector
import qualified Data.List as L
import           Data.Maybe (mapMaybe)
import           Data.Proxy
import           Data.UnBounded
import           Data.Vinyl.CoRec
import           Data.Vinyl.Core
import           Data.Vinyl.Lens
import           GHC.TypeLits

--------------------------------------------------------------------------------


-- | Lines are transformable, via line segments
instance (Fractional r, Arity d, Arity (d + 1)) => IsTransformable (Line d r) where
  transformBy t = supportingLine . transformPointFunctor t . toLineSegment'
    where
      toLineSegment' :: (Num r, Arity d) => Line d r -> LineSegment d () r
      toLineSegment' = toLineSegment

type instance IntersectionOf (Point d r) (Line d r) = [NoIntersection, Point d r]


instance (Eq r, Fractional r, Arity d)      => Point d r `HasIntersectionWith` Line d r where
  intersects = onLine
instance {-# OVERLAPPING #-} (Ord r, Num r) => Point 2 r `HasIntersectionWith` Line 2 r where
  intersects = onLine2

instance (Eq r, Fractional r, Arity d)      => Point d r `IsIntersectableWith` Line d r where
  nonEmptyIntersection = defaultNonEmptyIntersection
  p `intersect` l | p `intersects` l = coRec p
                  | otherwise        = coRec NoIntersection

instance {-# OVERLAPPING #-} (Ord r, Num r) => Point 2 r `IsIntersectableWith` Line 2 r where
  nonEmptyIntersection = defaultNonEmptyIntersection
  p `intersect` l | p `intersects` l = coRec p
                  | otherwise        = coRec NoIntersection

type instance IntersectionOf (Line 2 r) (Boundary (Rectangle p r)) =
  [ NoIntersection, Point 2 r, (Point 2 r, Point 2 r) , LineSegment 2 () r]

instance (Ord r, Fractional r)
         => Line 2 r `HasIntersectionWith` Boundary (Rectangle p r)
instance (Ord r, Fractional r)
         => Line 2 r `IsIntersectableWith` Boundary (Rectangle p r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  line' `intersect` (Boundary rect)  = case asAP segP of
      [sl'] -> case fromUnbounded sl' of
        Nothing   -> error "intersect: line x boundary rect; unbounded line? absurd"
        Just sl'' -> coRec $ first (either id id) $ sl''^.re _SubLine
      []    -> case nub' $ asAP pointP of
        [p]   -> coRec p
        [p,q] -> coRec (p,q)
        _     -> coRec NoIntersection
      _     -> error "intersect; line x boundary rect; absurd"
    where
      Sides t r b l = sides' rect
      ints = map (\s -> sl `intersect` toSL s) [t,r,b,l]

      nub' = map L.head . L.group . L.sort

      sl = fromLine line'
      -- wrap a segment into an potentially unbounded subline
      toSL  :: LineSegment 2 p r -> SubLine 2 () (UnBounded r) r
      toSL s = s^._SubLine.re _unBounded.to dropExtra

      asAP  :: forall proxy t. (t ∈ IntersectionOf (SubLine 2 () (UnBounded r) r)
                                                   (SubLine 2 () (UnBounded r) r))
             => proxy t -> [t]
      asAP _ = mapMaybe (asA @t) ints

      segP   = Proxy :: Proxy (SubLine 2 (Either () ()) (UnBounded r) r)
      pointP = Proxy :: Proxy (Point 2 r)


type instance IntersectionOf (Line 2 r) (Rectangle p r) =
  [ NoIntersection, Point 2 r, LineSegment 2 () r]

instance (Ord r, Fractional r)
         => Line 2 r `HasIntersectionWith` Rectangle p r
instance (Ord r, Fractional r)
         => Line 2 r `IsIntersectableWith` Rectangle p r where
  nonEmptyIntersection = defaultNonEmptyIntersection

  line' `intersect` rect  = match (line' `intersect` Boundary rect) $
       H coRec -- NoIntersection
    :& H coRec -- Point2
    :& H (\(p,q)          -> coRec $ ClosedLineSegment (ext p) (ext q))
    :& H coRec -- LineSegment
    :& RNil
