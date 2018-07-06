{-# LANGUAGE UndecidableInstances #-}
module Algorithms.Geometry.ConvexHull.GrahamFamPeano( convexHull
                                                    , upperHull
                                                    , lowerHull, fromP
                                                    ) where

import           Control.DeepSeq
import           Control.Lens ((^.))
import           Data.Ext
import           Data.Geometry.Point
import qualified Data.Vector.Fixed.Cont as V
import qualified Data.Geometry.Vector.VectorFamilyPeano as VF
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid
import           GHC.TypeLits
import qualified Linear.V2 as V2


newtype MyPoint d r = MyPoint (VF.VectorFamily d r)

deriving instance (VF.ImplicitArity d, Eq r)  => Eq (MyPoint d r)
deriving instance (VF.ImplicitArity d, Ord r) => Ord (MyPoint d r)
deriving instance (VF.ImplicitArity d, Show r) => Show (MyPoint d r)
deriving instance (NFData (VF.VectorFamily d r)) => NFData (MyPoint d r)

pattern Vector2 x y = VF.VectorFamily (V2.V2 x y)

pattern MyPoint2 x y = MyPoint (Vector2 x y)


-- instance (NFData r, Arity d) => NFData (MyPoint d r)  where
--   rnf (MyPoint x y) = rnf (x,y)
--   rnf (MyP p)       = rnf p

toP                    :: MyPoint VF.Two r :+ e -> Point 2 r :+ e
toP (MyPoint2 x y :+ e) = Point2 x y :+ e

fromP                   :: Point 2 r :+ e -> MyPoint VF.Two r :+ e
fromP (Point2 x y :+ e) = MyPoint2 x y :+ e


subt :: Num r => MyPoint VF.Two r -> MyPoint VF.Two r -> MyPoint VF.Two r
(MyPoint2 x y) `subt` (MyPoint2 a b) = MyPoint2 (x-a) (y-b)

newtype ConvexPolygon p r = ConvexPolygon [Point 2 r :+ p] deriving (Show,Eq,NFData)

-- | \(O(n \log n)\) time ConvexHull using Graham-Scan. The resulting polygon is
-- given in clockwise order.
convexHull            :: (Ord r, Num r)
                      => NonEmpty (MyPoint VF.Two r :+ p) -> ConvexPolygon p r
convexHull (p :| []) = ConvexPolygon $ [toP p]
convexHull ps        = let ps' = NonEmpty.toList . NonEmpty.sortBy incXdecY $ ps
                           uh  = NonEmpty.tail . hull' $         ps'
                           lh  = NonEmpty.tail . hull' $ reverse ps'
                       in ConvexPolygon . map toP . reverse $ lh ++ uh

upperHull  :: (Ord r, Num r) => NonEmpty (MyPoint VF.Two r :+ p) -> NonEmpty (MyPoint VF.Two r :+ p)
upperHull = hull id


lowerHull :: (Ord r, Num r) => NonEmpty (MyPoint VF.Two r :+ p) -> NonEmpty (MyPoint VF.Two r :+ p)
lowerHull = hull reverse


-- | Helper function so that that can compute both the upper or the lower hull, depending
-- on the function f
hull               :: (Ord r, Num r)
                   => ([MyPoint VF.Two r :+ p] -> [MyPoint VF.Two r :+ p])
                   -> NonEmpty (MyPoint VF.Two r :+ p) -> NonEmpty (MyPoint VF.Two r :+ p)
hull _ h@(_ :| []) = h
hull f pts         = hull' .  f
                   . NonEmpty.toList . NonEmpty.sortBy incXdecY $ pts

incXdecY  :: Ord r => (MyPoint VF.Two r) :+ p -> (MyPoint VF.Two r) :+ q -> Ordering
incXdecY (MyPoint2 px py :+ _) (MyPoint2 qx qy :+ _) =
  compare px qx <> compare qy py


-- | Precondition: The list of input points is sorted
hull'          :: (Ord r, Num r) => [MyPoint VF.Two r :+ p] -> NonEmpty (MyPoint VF.Two r :+ p)
hull' (a:b:ps) = NonEmpty.fromList $ hull'' [b,a] ps
  where
    hull'' h []      = h
    hull'' h (p:ps') = hull'' (cleanMiddle (p:h)) ps'

    cleanMiddle h@[_,_]                         = h
    cleanMiddle h@(z:y:x:rest)
      | rightTurn (x^.core) (y^.core) (z^.core) = h
      | otherwise                               = cleanMiddle (z:x:rest)
    cleanMiddle _                               = error "cleanMiddle: too few points"

rightTurn       :: (Ord r, Num r) => MyPoint VF.Two r -> MyPoint VF.Two r -> MyPoint VF.Two r -> Bool
rightTurn a b c = ccwP a b c == CW



ccwP :: (Ord r, Num r) => MyPoint VF.Two r -> MyPoint VF.Two r -> MyPoint VF.Two r -> CCW
ccwP p q r = case z `compare` 0 of
              LT -> CW
              GT -> CCW
              EQ -> CoLinear
     where

       MyPoint2 ux uy = q `subt` p
       MyPoint2 vx vy = r `subt` p
       z              = ux * vy - uy * vx
