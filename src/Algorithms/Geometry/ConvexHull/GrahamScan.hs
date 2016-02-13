module Algorithms.Geometry.ConvexHull.GrahamScan( convexHull
                                                , upperHull
                                                , lowerHull
                                                , module Types
                                                ) where

import           Algorithms.Geometry.ConvexHull.Types as Types
import           Control.Lens((^.))
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid
import           Data.List.NonEmpty(NonEmpty(..))


-- | O(n log n) time ConvexHull using Graham-Scan. The resulting polygon is
-- given in clockwise order.
convexHull            :: (Ord r, Num r)
                      => NonEmpty (Point 2 r :+ p) -> ConvexHull p r
convexHull (p :| []) = ConvexHull . fromPoints $ [p]
convexHull ps        = let ps' = NonEmpty.toList . NonEmpty.sortBy incXdecY $ ps
                           uh  = NonEmpty.tail . hull' $         ps'
                           lh  = NonEmpty.tail . hull' $ reverse ps'
                       in ConvexHull . fromPoints . reverse $ lh ++ uh

upperHull  :: (Ord r, Num r) => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
upperHull = hull id


lowerHull :: (Ord r, Num r) => NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
lowerHull = hull reverse


-- | Helper function so that that can compute both the upper or the lower hull, depending
-- on the function f
hull               :: (Ord r, Num r)
                   => ([Point 2 r :+ p] -> [Point 2 r :+ p])
                   -> NonEmpty (Point 2 r :+ p) -> NonEmpty (Point 2 r :+ p)
hull f h@(_ :| []) = h
hull f pts         = hull' .  f
                   . NonEmpty.toList . NonEmpty.sortBy incXdecY $ pts

incXdecY  :: Ord r => (Point 2 r) :+ p -> (Point 2 r) :+ q -> Ordering
incXdecY (Point2 px py :+ _) (Point2 qx qy :+ _) =
  compare px qx <> compare qy py


-- | Precondition: The list of input points is sorted
hull'          :: (Ord r, Num r) => [Point 2 r :+ p] -> NonEmpty (Point 2 r :+ p)
hull' (a:b:ps) = NonEmpty.fromList $ hull'' [b,a] ps
  where
    hull'' h  []    = h
    hull'' h (p:ps) = hull'' (cleanMiddle (p:h)) ps

    cleanMiddle [b,a]                           = [b,a]
    cleanMiddle h@(c:b:a:rest)
      | rightTurn (a^.core) (b^.core) (c^.core) = h
      | otherwise                               = cleanMiddle (c:a:rest)


rightTurn       :: (Ord r, Num r) => Point 2 r -> Point 2 r -> Point 2 r -> Bool
rightTurn a b c = ccw a b c == CW
