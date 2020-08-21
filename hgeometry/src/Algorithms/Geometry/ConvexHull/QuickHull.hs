module Algorithms.Geometry.ConvexHull.QuickHull( convexHull ) where

import           Control.Lens ((^.))
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex (ConvexPolygon(..))
import           Data.Geometry.Triangle
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Ord (comparing)
import           Data.Util


-- import Data.Ratio
-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified Algorithms.Geometry.ConvexHull.GrahamScan as GC
-- import Debug.Trace
--------------------------------------------------------------------------------

-- | ConvexHull using Quickhull. The resulting polygon is given in
-- clockwise order.
--
-- running time: \(O(n^2)\)
convexHull            :: (Ord r, Fractional r, Show r, Show p)
                      => NonEmpty (Point 2 r :+ p) -> ConvexPolygon p r
convexHull (p :| []) = ConvexPolygon . fromPoints $ [p]
convexHull ps        = ConvexPolygon . fromPoints
                     $ [l] <> hull l r above <> [r] <> (reverse $ hull l r below)
  where
    STR l r mids  = findExtremes ps
    m             = lineThrough (l^.core) (r^.core)
    (above,below) = List.partition (\(p :+ _) -> p `liesAbove` m) mids

-- | Finds the leftmost and rightmost point in the list
findExtremes            :: Ord r
                        => NonEmpty (Point 2 r :+ q)
                        -> STR (Point 2 r :+ q) (Point 2 r :+ q) [Point 2 r :+ q]
findExtremes (p :| pts ) = foldr f (STR p p []) pts
  where
    f q (STR l r ms) = case (incXdecY q l, incXdecY q r) of
                         (LT,_)  -> STR q r (addIfNot r l ms)
                         (EQ,_)  -> STR l r ms -- ditch q; it is the same as l
                         (GT,GT) -> STR l q (addIfNot l r ms)
                         (GT,EQ) -> STR l r ms -- ditch q; it is the same as r
                         (GT,LT) -> STR l r (q:ms)

    addIfNot y x xs | (x^.core) /= (y^.core) = x:xs
                    | otherwise              = xs

-- findExtremesBy         :: (a -> a -> Ordering)
--                        -> NonEmpty a
--                        -> STR a a [a]
-- findExtremesBy cmp pts = let l = F.minimumBy cmp pts
--                              r = F.maximumBy cmp pts
--                              a /=. b = a `cmp` b /= EQ
--                          in STR l r [p | p <- F.toList pts, p /=. l, p /=. r]


incXdecY  :: Ord r => (Point 2 r) :+ p -> (Point 2 r) :+ q -> Ordering
incXdecY (Point2 px py :+ _) (Point2 qx qy :+ _) =
  compare px qx <> compare qy py

-- | include neigher left or right
--
hull         :: (Fractional r, Ord r)
             => Point 2 r :+ p -> Point 2 r :+ p -> [Point 2 r :+ p] -> [Point 2 r :+ p]
hull _ _ []  = []
hull l r pts = hull l mid ls <> [mid] <> hull mid r rs
  where
    m       = lineThrough (l^.core) (r^.core)
    mid     = F.maximumBy (comparing dist) pts

    dist (p :+ _) = p `sqDistanceTo` m
    t       = Triangle l mid r
    -- line through l and mid, which splits the remaining points in a left half and a right half.
    splitL   = lineThrough (l^.core) (mid^.core)
    rightSide = (r^.core) `onSide` splitL -- define the side containing r the right side

    (ls,rs) = List.partition (\(p :+ _) -> p `onSide` splitL /= rightSide)
            . filter (\(p :+ _) -> not $ p `onTriangle` t) $ pts


-- mPoint2 [x,y] = Point2 x y

-- -- testPoints = NonEmpty.fromList
-- --   [ mPoint2 [22536303956634 % 7570647828779,(-5816376064439) % 1228319866920] :+ 1
-- --   , mPoint2 [(-3136920648983) % 824638230353,(-14583744643665) % 9604445576558] :+ 2
-- --   , mPoint2 [(-11653462784667) % 6525086575987,(-598434515815) % 1364557986096] :+ 3
-- --   , mPoint2 [(-7841595901661) % 3282967141364,(-207167076115) % 482378191549] :+ 4
-- --   ]


-- testPoints :: NonEmpty (Point 2 Rational :+ Int)
-- testPoints = read "(Point2 [(-11199966464450) % 1365514034959,4065659138075 % 2296468530516] :+ 1) :| [Point2 [86686001553073 % 2736621704548,(-63774454571048) % 1880665081093] :+ 2,Point2 [(-77322324895231) % 8260610289790,(-41165682123514) % 2291705829063] :+ 3,Point2 [2292642905947 % 2659329735076,87045289726355 % 2752214350419] :+ 4]"


-- toDouble          :: Point 2 Rational :+ a -> Point 2 Double :+ a
-- toDouble (p :+ x) = (realToFrac <$> p) :+ x


--        -- Falsified (after 36 tests and 4 shrinks):



-- testPoints2 :: NonEmpty (Point 2 Rational :+ Int)
-- testPoints2 = read "(Point2 [(-9876468593885) % 9254762894818,(-34982972348716) % 7450362538495] :+ 1) :| [Point2 [(-11974177119403) % 7705693443554,(-37634868551543) % 9311528788922] :+ 2,Point2 [(-32383659855458) % 9565531378857,20253950785876 % 8268868939819] :+ 3,Point2 [42425655100996 % 8786996213535,(-7972873491283) % 1604043452399] :+ 4]"
