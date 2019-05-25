module Algorithms.Geometry.GrahamGADT( convexHull
                                 , upperHull
                                 , lowerHull, fromP
                                 ) where


import           Control.Lens ((^.))
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Polygon.Convex (ConvexPolygon(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid
import           Control.DeepSeq
import GHC.TypeLits


data MyPoint (d :: Nat) r where
  MyPoint :: !r -> !r -> MyPoint 2 r
  MyP     :: Point d r -> MyPoint d r


-- instance (NFData r, Arity d) => NFData (MyPoint d r)  where
--   rnf (MyPoint x y) = rnf (x,y)
--   rnf (MyP p)       = rnf p

toP                    :: MyPoint 2 r :+ e -> Point 2 r :+ e
toP (MyPoint x y :+ e) = Point2 x y :+ e

fromP                   :: Point 2 r :+ e -> MyPoint 2 r :+ e
fromP (Point2 x y :+ e) = MyPoint x y :+ e


subt :: Num r => MyPoint 2 r -> MyPoint 2 r -> MyPoint 2 r
(MyPoint x y) `subt` (MyPoint a b) = MyPoint (x-a) (y-b)

-- | \(O(n \log n)\) time ConvexHull using Graham-Scan. The resulting polygon is
-- given in clockwise order.
convexHull            :: (Ord r, Num r)
                      => NonEmpty (MyPoint 2 r :+ p) -> ConvexPolygon p r
convexHull (p :| []) = ConvexPolygon . fromPoints $ [toP p]
convexHull ps        = let ps' = NonEmpty.toList . NonEmpty.sortBy incXdecY $ ps
                           uh  = NonEmpty.tail . hull' $         ps'
                           lh  = NonEmpty.tail . hull' $ reverse ps'
                       in ConvexPolygon . fromPoints . map toP . reverse $ lh ++ uh

upperHull  :: (Ord r, Num r) => NonEmpty (MyPoint 2 r :+ p) -> NonEmpty (MyPoint 2 r :+ p)
upperHull = hull id


lowerHull :: (Ord r, Num r) => NonEmpty (MyPoint 2 r :+ p) -> NonEmpty (MyPoint 2 r :+ p)
lowerHull = hull reverse


-- | Helper function so that that can compute both the upper or the lower hull, depending
-- on the function f
hull               :: (Ord r, Num r)
                   => ([MyPoint 2 r :+ p] -> [MyPoint 2 r :+ p])
                   -> NonEmpty (MyPoint 2 r :+ p) -> NonEmpty (MyPoint 2 r :+ p)
hull _ h@(_ :| []) = h
hull f pts         = hull' .  f
                   . NonEmpty.toList . NonEmpty.sortBy incXdecY $ pts

incXdecY  :: Ord r => (MyPoint 2 r) :+ p -> (MyPoint 2 r) :+ q -> Ordering
incXdecY (MyPoint px py :+ _) (MyPoint qx qy :+ _) =
  compare px qx <> compare qy py


-- | Precondition: The list of input points is sorted
hull'          :: (Ord r, Num r) => [MyPoint 2 r :+ p] -> NonEmpty (MyPoint 2 r :+ p)
hull' (a:b:ps) = NonEmpty.fromList $ hull'' [b,a] ps
  where
    hull'' h []      = h
    hull'' h (p:ps') = hull'' (cleanMiddle (p:h)) ps'

    cleanMiddle h@[_,_]                         = h
    cleanMiddle h@(z:y:x:rest)
      | rightTurn (x^.core) (y^.core) (z^.core) = h
      | otherwise                               = cleanMiddle (z:x:rest)
    cleanMiddle _                               = error "cleanMiddle: too few points"

rightTurn       :: (Ord r, Num r) => MyPoint 2 r -> MyPoint 2 r -> MyPoint 2 r -> Bool
rightTurn a b c = ccwP a b c == CW



ccwP :: (Ord r, Num r) => MyPoint 2 r -> MyPoint 2 r -> MyPoint 2 r -> CCW
ccwP p q r = case z `compare` 0 of
              LT -> CW
              GT -> CCW
              EQ -> CoLinear
     where

       MyPoint ux uy = q `subt` p
       MyPoint vx vy = r `subt` p
       z             = ux * vy - uy * vx
