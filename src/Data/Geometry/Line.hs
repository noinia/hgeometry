{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
module Data.Geometry.Line( module Data.Geometry.Line.Internal
                         ) where

import Control.Lens ((^.), re, bimap)
import Data.Ext
import Data.Geometry.Boundary
import Data.Geometry.Box
import Data.Geometry.Line.Internal
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.Properties
import Data.Geometry.SubLine
import Data.Geometry.Transformation
import Data.Geometry.Vector
import Data.Maybe (mapMaybe)
import Data.Proxy
import Data.UnBounded
import Data.Vinyl.Lens
import Data.Vinyl.Core
import Frames.CoRec
import qualified Data.List as L

--------------------------------------------------------------------------------

-- | Lines are transformable, via line segments
instance (Num r, AlwaysTruePFT d) => IsTransformable (Line d r) where
  transformBy t = supportingLine . transformPointFunctor t . toLineSegment'
    where
      toLineSegment' :: (Num r, Arity d) => Line d r -> LineSegment d () r
      toLineSegment' = toLineSegment


type instance IntersectionOf (Line 2 r) (Boundary (Rectangle p r)) =
  [ NoIntersection, Point 2 r, (Point 2 r, Point 2 r) , LineSegment 2 () r]


instance (Ord r, Fractional r)
         => (Line 2 r) `IsIntersectableWith` (Boundary (Rectangle p r)) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  line' `intersect` (Boundary rect)  = case asA' segP of
      [sl'] -> coRec . bimap id unVal $ sl'^.re _SubLine
      []    -> case nub' . map (fmap unVal) $ asA' pointP of
        [p]   -> coRec p
        [p,q] -> coRec (p,q)
        _     -> coRec NoIntersection
      _     -> error "intersect; ine x boundary rect; absurd"
    where
      (t,r,b,l) = sides' rect
      ints = map (\s -> sl `intersect` toSL s) [t,r,b,l]

      nub' = map head . L.group . L.sort

      sl = fromLine line'
      -- wrap a segment into an potentially unbounded subline
      toSL s = bimap (const ()) Val $ s^._SubLine

      unVal (Val x) = x
      unVal _       = error "intersect; line x boundary rect: unVal Unbounded"

      asA'    :: (t ∈ IntersectionOf (SubLine 2 () (UnBounded r))
                                     (SubLine 2 () (UnBounded r)))
              => proxy t -> [t]
      asA' px = mapMaybe (asA px) ints

      segP   = Proxy :: Proxy (SubLine 2 () (UnBounded r))
      pointP = Proxy :: Proxy (Point 2      (UnBounded r))


type instance IntersectionOf (Line 2 r) (Rectangle p r) =
  [ NoIntersection, Point 2 r, LineSegment 2 () r]


instance (Ord r, Fractional r)
         => (Line 2 r) `IsIntersectableWith` (Rectangle p r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  line' `intersect` rect  = match (line' `intersect` (Boundary rect)) $
       (H $ \NoIntersection -> coRec NoIntersection)
    :& (H $ \p@(Point2 _ _) -> coRec p)
    :& (H $ \(p,q)          -> coRec $ ClosedLineSegment (ext p) (ext q))
    :& (H $ \s              -> coRec s)
    :& RNil
