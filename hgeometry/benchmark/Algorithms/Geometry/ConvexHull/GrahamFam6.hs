{-# LANGUAGE UndecidableInstances #-}
module Algorithms.Geometry.ConvexHull.GrahamFam6( convexHull
                                                , upperHull
                                                , lowerHull, fromP
                                                ) where

import           Control.DeepSeq
import           Control.Lens ((^.))
import           Data.Ext
import           Data.Geometry.Point
import qualified Data.Geometry.Vector.VectorFamily6 as VF
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid
import           GHC.TypeLits


newtype MyPoint d r = MyPoint (VF.Vector d r)

deriving instance (VF.Arity d, Eq r)  => Eq (MyPoint d r)
deriving instance (VF.Arity d, Ord r) => Ord (MyPoint d r)
deriving instance (VF.Arity d, Show r) => Show (MyPoint d r)
deriving instance (NFData (VF.Vector d r)) => NFData (MyPoint d r)

pattern MyPoint2 x y = MyPoint (VF.Vector2 x y)


-- instance (NFData r, Arity d) => NFData (MyPoint d r)  where
--   rnf (MyPoint x y) = rnf (x,y)
--   rnf (MyP p)       = rnf p

toP                    :: MyPoint 2 r :+ e -> Point 2 r :+ e
toP (MyPoint2 x y :+ e) = Point2 x y :+ e

fromP                   :: Point 2 r :+ e -> MyPoint 2 r :+ e
fromP (Point2 x y :+ e) = MyPoint2 x y :+ e


subt :: Num r => MyPoint 2 r -> MyPoint 2 r -> MyPoint 2 r
(MyPoint2 x y) `subt` (MyPoint2 a b) = MyPoint2 (x-a) (y-b)

newtype ConvexPolygon p r = ConvexPolygon [Point 2 r :+ p] deriving (Show,Eq,NFData)

-- | \(O(n \log n)\) time ConvexHull using Graham-Scan. The resulting polygon is
-- given in clockwise order.
convexHull            :: (Ord r, Num r)
                      => NonEmpty (MyPoint 2 r :+ p) -> ConvexPolygon p r
convexHull (p :| []) = ConvexPolygon $ [toP p]
convexHull ps        = let ps' = NonEmpty.toList . NonEmpty.sortBy incXdecY $ ps
                           uh  = NonEmpty.tail . hull' $         ps'
                           lh  = NonEmpty.tail . hull' $ reverse ps'
                       in ConvexPolygon . map toP . reverse $ lh ++ uh

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
incXdecY (MyPoint2 px py :+ _) (MyPoint2 qx qy :+ _) =
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

       MyPoint2 ux uy = q `subt` p
       MyPoint2 vx vy = r `subt` p
       z              = ux * vy - uy * vx
