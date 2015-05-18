module Algorithms.Geometry.ConvexHull.GrahamScan( ConvexHull(..)
                                                , DegenerateCH
                                                , convexHull
                                                , upperHull
                                                , lowerHull
                                                ) where

import           Control.Lens((^.))
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.List as L
import           Data.Monoid


-- | Two dimensional convex hulls
newtype ConvexHull p r = ConvexHull { _hull :: (SimplePolygon p r) }
--                       deriving (Show,Eq)

type DegenerateCH p r = Maybe (Point 2 r :+ p)


-- | O(n log n) time ConvexHull using Graham-Scan
convexHull     :: (Ord r, Num r)
               => [Point 2 r :+ p] -> Either (DegenerateCH p r) (ConvexHull p r)
convexHull []  = Left Nothing
convexHull [p] = Left (Just p)
convexHull ps  = let ps' = L.sortBy incXdecY ps
                     uh  = tail . hull' $         ps'
                     lh  = tail . hull' $ reverse ps'
                 in Right . ConvexHull . fromPoints $ lh ++ uh

upperHull  :: (Ord r, Num r)
           => [Point 2 r :+ p] -> Either (DegenerateCH p r) [Point 2 r :+ p]
upperHull = hull id


lowerHull  :: (Ord r, Num r)
           => [Point 2 r :+ p] -> Either (DegenerateCH p r) [Point 2 r :+ p]
lowerHull = hull reverse


-- | Helper function so that that can compute both the upper or the lower hull, depending
-- on the function f
hull       :: (Ord r, Num r)
           => ([Point 2 r :+ p] -> [Point 2 r :+ p])
           -> [Point 2 r :+ p]
           -> Either (DegenerateCH p r) [Point 2 r :+ p]
hull _ []  = Left Nothing
hull _ [p] = Left (Just p)
hull f ps  = Right . hull' . f . L.sortBy incXdecY $ ps



incXdecY  :: Ord r => (Point 2 r) :+ p -> (Point 2 r) :+ q -> Ordering
incXdecY (Point2 px py :+ _) (Point2 qx qy :+ _) =
  compare px qx <> compare qy py


-- | Precondition: The list of input points is sorted
hull'          :: (Ord r, Num r) => [Point 2 r :+ p] -> [Point 2 r :+ p]
hull' (a:b:ps) = hull'' [b,a] ps
  where
    hull'' h  []    = h
    hull'' h (p:ps) = hull'' (cleanMiddle (p:h)) ps

    cleanMiddle [b,a]                           = [b,a]
    cleanMiddle h@(c:b:a:rest)
      | rightTurn (a^.core) (b^.core) (c^.core) = h
      | otherwise                               = cleanMiddle (c:a:rest)


rightTurn       :: (Ord r, Num r) => Point 2 r -> Point 2 r -> Point 2 r -> Bool
rightTurn a b c = ccw a b c == CW
