module ConvexHull.GrahamInt( convexHull
                           , upperHull
                           , lowerHull, fromP
                           ) where


import           Control.DeepSeq
import           Control.Lens ((^.))
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid
import           GHC.Generics
import           HGeometry.Point



data MyPoint = MyPoint {-# UNPACK #-}!Int
                       {-# UNPACK #-}!Int
             deriving (Show,Eq,Ord,Generic)

instance NFData MyPoint

fromP (Point2_ x y) = MyPoint x y

(MyPoint x y) `subt` (MyPoint a b) = MyPoint (x-a) (y-b)


newtype ConvexPolygon = ConvexPolygon [MyPoint] deriving (NFData)

-- | \(O(n \log n)\) time ConvexHull using Graham-Scan. The resulting polygon is
-- given in clockwise order.
convexHull            :: NonEmpty MyPoint -> ConvexPolygon
convexHull (p :| []) = ConvexPolygon [p]
convexHull ps        = let ps' = NonEmpty.toList . NonEmpty.sortBy incXdecY $ ps
                           uh  = NonEmpty.tail . hull' $         ps'
                           lh  = NonEmpty.tail . hull' $ reverse ps'
                       in ConvexPolygon . reverse $ lh ++ uh

upperHull  :: NonEmpty MyPoint -> NonEmpty MyPoint
upperHull = hull id

lowerHull :: NonEmpty MyPoint -> NonEmpty MyPoint
lowerHull = hull reverse


-- | Helper function so that that can compute both the upper or the lower hull, depending
-- on the function f
hull               :: ([MyPoint] -> [MyPoint])
                   -> NonEmpty MyPoint -> NonEmpty MyPoint
hull _ h@(_ :| []) = h
hull f pts         = hull' .  f
                   . NonEmpty.toList . NonEmpty.sortBy incXdecY $ pts

incXdecY :: MyPoint -> MyPoint -> Ordering
incXdecY (MyPoint px py) (MyPoint qx qy) =
  compare px qx <> compare qy py


-- | Precondition: The list of input points is sorted
hull'          :: [MyPoint] -> NonEmpty MyPoint
hull' (a:b:ps) = NonEmpty.fromList $ hull'' [b,a] ps
  where
    hull'' h []      = h
    hull'' h (p:ps') = hull'' (cleanMiddle (p:h)) ps'

    cleanMiddle h@[_,_]           = h
    cleanMiddle h@(z:y:x:rest)
      | rightTurn x y z           = h
      | otherwise                 = cleanMiddle (z:x:rest)
    cleanMiddle _                 = error "cleanMiddle: too few points"

rightTurn       :: MyPoint -> MyPoint -> MyPoint -> Bool
rightTurn a b c = ccwP a b c == CW


ccwP       :: MyPoint -> MyPoint -> MyPoint -> CCW
ccwP p q r = case z `compare` 0 of
              LT -> CW
              GT -> CCW
              EQ -> CoLinear
     where

       MyPoint ux uy = q `subt` p
       MyPoint vx vy = r `subt` p
       z             = ux * vy - uy * vx
