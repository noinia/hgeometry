{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.LineSegment
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Line segment data type and some basic functions on line segments
--
--------------------------------------------------------------------------------
module Data.Geometry.LineSegment
  ( LineSegment(LineSegment, LineSegment', ClosedLineSegment, OpenLineSegment)
  , endPoints

  , _SubLine
  , module Data.Geometry.Interval

  , toLineSegment
  , orderedEndPoints
  , segmentLength
  , sqSegmentLength
  , sqDistanceToSeg, sqDistanceToSegArg
  , flipSegment

  , interpolate, sampleLineSegment
  ) where

-- import           Control.Lens
import           Data.Ext
-- import qualified Data.Foldable as F
import           Data.Geometry.Boundary
import           Data.Geometry.Box.Internal
import           Data.Geometry.Box.Sides
import           Data.Geometry.Interval hiding (width, midPoint)
import           Data.Geometry.LineSegment.Internal
import           Data.Geometry.Point
import           Data.Geometry.Properties
-- import           Data.Geometry.SubLine
import           Data.Util
import Data.Geometry.LineSegment (LineSegment(ClosedLineSegment))
-- import           Data.Vinyl.CoRec
-- import           Data.Bifunctor
-- import           Data.Either
-- import           Data.Maybe (mapMaybe)



--------------------------------------------------------------------------------


type instance IntersectionOf (LineSegment 2 p r) (Boundary (Rectangle q r)) =
  [ NoIntersection, Point 2 r, Two (Point 2 r) , LineSegment 2 () r ]


type instance IntersectionOf (LineSegment 2 p r) (Rectangle q r) =
  [ NoIntersection, Point 2 r, LineSegment 2 (Maybe p) r ]

instance (Fractional r, Ord r)
         => LineSegment 2 p r `HasIntersectionWith` Boundary (Rectangle q r) where
  seg `intersects` (Boundary rect) = any (seg `intersects`) $ sides rect

instance (Fractional r, Ord r) => LineSegment 2 p r `HasIntersectionWith` Rectangle q r where
  seg@(LineSegment p q) `intersects` rect =
      inRect p || inRect q || any (seg `intersects`) (sides rect)
    where
      inRect = \case
        Open   (a :+ _) -> a `insideBox`  rect -- if strictly inside the seg intersects.
        Closed (a :+ _) -> a `inBox`      rect -- in or on the boundary is fine

-- instance (Num r, Ord r)
--          => (LineSegment 2 p r) `IsIntersectableWith` (Boundary (Rectangle q r)) where
--   seg `intersect` (Boundary rect) = case partitionEithers res of
--     (s : _, _)    -> coRec s -- if we find a segment that should be the
--                              -- answer; we shouldn't fine more than one
--                              -- by the way.
--     ([], [])      -> coRec  NoIntersection
--     ([], [p])     -> coRec p
--     ([], (p:q:_)) -> coRec $ Two p q
--                      -- more than two points is impossible anwyay
--     where
--       res = mapMaybe (\side -> match (seg `intersect` side) $
--                        (H $ \NoIntersection            -> Nothing)
--                     :& (H $ \(p :: Point 2 r)          -> Just $ Right p)
--                     :& (H $ \(s :: LineSegment 2 () r) -> Just $ Left s)
--                     :& RNil
--              ) . F.toList $ sides rect



-- -- instance (Num r, Ord r) => (LineSegment 2 p r) `IsIntersectableWith` (Rectangle q r) where
-- --   seg@(LineSegment' (p :+ _) (q :+ _)) `intersect` rect =
-- --       case (p `intersects` rect, q `intersects` rect) of
-- --         (True,True)   -> coRec seg'
-- --         (False,False) -> match boundaryIntersection $ -- both endpoints outside
-- --              (H $ \NoIntersection   -> coRec NoIntersection)
-- --           :& (H $ \(a :: Point 2 r) -> coRec a)
-- --           :& (H $ \(Two a b)        -> coRec $ ClosedLineSegment (ext a) (ext b))
-- --           :& (H $ \s                -> coRec s)
-- --           :& RNil
-- --         (True,False)  -> withInside p (\other -> LineSegment p' (closed other))
-- --         (False,True)  -> withInside q (\other -> LineSegment (closed other) q')
-- --     where
-- --       seg'@(LineSegment p' q') = first (const ()) seg

-- --       boundaryIntersection = seg `intersect` (Boundary rect)
-- --       closed :: Point 2 r -> EndPoint (Point 2 r :+ ())
-- --       closed = Closed . ext

-- --       -- the given endpoint endPt is inside the box [*], while the
-- --       -- other endpoint is not. The second arg is a function that
-- --       -- rebuilds the segment given the replacement endpoint, compute
-- --       -- the right segment that is inside the rectangle.
-- --       --
-- --       -- [*] We require that the *point* lies in or on the box. If the
-- --       -- endpoint was open, it may still be the case that we do not
-- --       -- actually intersect the rectangle (i.e. if the open endPoint
-- --       -- was on a corner of the rect).
-- --       -- withInside                      :: Point 2 r
-- --       --                                 -> (Point 2 r -> LineSegment 2 () r)
-- --       --                                 -> IntersectionOf ....
-- --       withInside endPt mkSeg = match boundaryIntersection $
-- --            (H $ \NoIntersection   -> coRec NoIntersection)
-- --            -- seems this should happen only if the endpoint that was
-- --            -- suposedly in/on the rect was open.
-- --         :& (H $ \(a :: Point 2 r) -> coRec . mkSeg $ a)
-- --         :& (H $ \(Two a b)        -> coRec . mkSeg $ if a == endPt then b else a)
-- --         :& (H $ \s                -> coRec s)
-- --         :& RNil
