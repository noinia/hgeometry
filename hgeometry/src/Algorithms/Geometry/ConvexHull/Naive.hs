module Algorithms.Geometry.ConvexHull.Naive where

import           Control.Lens
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.HyperPlane
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Triangle
import           Data.Geometry.Vector
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (listToMaybe, isNothing)
import           Data.Util

--------------------------------------------------------------------------------

type ConvexHull d p r = [Triangle 3 p r]

lowerHull'                 :: forall r p. (Ord r, Fractional r, Show r)
                           => NonEmpty (Point 3 r :+ p) -> ConvexHull 3 p r
lowerHull' (toList -> pts) = let mkT (Three p q r) = Triangle p q r in
    [ t | t <- mkT <$> uniqueTriplets pts, isNothing (isValidTriangle t pts) ]


-- | Tests if this is a valid triangle for the lower envelope. That
-- is, if all point lie above the plane through these points. Returns
-- a Maybe; if the result is a Nothing the triangle is valid, if not
-- it returns a counter example.
--
-- >>> let t = (Triangle (ext origin) (ext $ Point3 1 0 0) (ext $ Point3 0 1 0))
-- >>> isValidTriangle t [ext $ Point3 5 5 0]
-- Nothing
-- >>> let t = (Triangle (ext origin) (ext $ Point3 1 0 0) (ext $ Point3 0 1 0))
-- >>> isValidTriangle t [ext $ Point3 5 5 (-10)]
-- Just (Point3 [5,5,-10] :+ ())
isValidTriangle                      :: (Num r, Ord r)
                                     => Triangle 3 p r -> [Point 3 r :+ q] -> Maybe (Point 3 r :+ q)
isValidTriangle (Triangle p q r) pts = listToMaybe $ filter (\a -> (a^.core) `liesBelow` h) pts
  where
    h' = from3Points (p^.core) (q^.core) (r^.core)
    c  = p&core.zCoord -~ 1
    h  = if (c^.core) `liesBelow` h' then h' else h'&normalVec %~ ((-1) *^)
    a `liesBelow` plane = (a `onSideUpDown` plane) == Below




reorder                  :: Ord p => Triangle 3 p r -> Triangle 3 p r
reorder (Triangle p q r) = let [p',q',r'] = List.sortOn (^.extra) [p,q,r] in Triangle p' q' r'


myPts :: NonEmpty (Point 3 Double :+ Int)
myPts = NonEmpty.fromList $ [ Point3 5  5  0  :+ 2
                            , Point3 1  1  10 :+ 1
                            , Point3 0  10 20 :+ 0
                            , Point3 12 1  1  :+ 3
                            , Point3 22 20  1  :+ 4
                            ]

-- myResult = [1 2 3
--             2 3 4
--             0 1 2
--             0 2 4
--            ]

myPts' :: NonEmpty (Point 3 Double :+ Int)
myPts' = NonEmpty.fromList $ [ Point3 5  5  0  :+ 2
                             , Point3 1  1  10 :+ 1
                             , Point3 0  10 20 :+ 0
                             , Point3 12 1  1  :+ 3
                             ]

-- 1 2 3
-- 0 1 2
-- 0 2 3

test = mapM_ print . map reorder $ lowerHull' myPts

test' = mapM_ print . map reorder $ lowerHull' myPts'
