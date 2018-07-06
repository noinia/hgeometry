{-# Language DeriveGeneric #-}
module Algorithms.Geometry.ConvexHull.GrahamV2( convexHull
                                              , upperHull
                                              , lowerHull, fromP
                                              ) where


import           Control.DeepSeq
import           Control.Lens ((^.))
import           Data.Ext
import           Data.Geometry.Point
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid
import           GHC.Generics
import qualified Linear.V2 as V2



newtype MyPoint r = MKPoint (V2.V2 r) deriving (Show,Eq,Ord,Generic)
-- data MyPoint r = MyPoint !r !r deriving (Show,Eq,Ord,Generic)

pattern MyPoint x y = MKPoint (V2.V2 x y)

instance NFData r => NFData (MyPoint r)


toP (MyPoint x y :+ e) = Point2 x y :+ e
fromP (Point2 x y :+ e) = MyPoint x y :+ e

(MyPoint x y) `subt` (MyPoint a b) = MyPoint (x-a) (y-b)


newtype ConvexPolygon p r = ConvexPolygon [Point 2 r :+ p] deriving (Show,Eq,NFData)

-- | \(O(n \log n)\) time ConvexHull using Graham-Scan. The resulting polygon is
-- given in clockwise order.
convexHull            :: (Ord r, Num r)
                      => NonEmpty (MyPoint r :+ p) -> ConvexPolygon p r
convexHull (p :| []) = ConvexPolygon $ [toP p]
convexHull ps        = let ps' = NonEmpty.toList . NonEmpty.sortBy incXdecY $ ps
                           uh  = NonEmpty.tail . hull' $         ps'
                           lh  = NonEmpty.tail . hull' $ reverse ps'
                       in ConvexPolygon . map toP . reverse $ lh ++ uh

upperHull  :: (Ord r, Num r) => NonEmpty (MyPoint r :+ p) -> NonEmpty (MyPoint r :+ p)
upperHull = hull id


lowerHull :: (Ord r, Num r) => NonEmpty (MyPoint r :+ p) -> NonEmpty (MyPoint r :+ p)
lowerHull = hull reverse


-- | Helper function so that that can compute both the upper or the lower hull, depending
-- on the function f
hull               :: (Ord r, Num r)
                   => ([MyPoint r :+ p] -> [MyPoint r :+ p])
                   -> NonEmpty (MyPoint r :+ p) -> NonEmpty (MyPoint r :+ p)
hull _ h@(_ :| []) = h
hull f pts         = hull' .  f
                   . NonEmpty.toList . NonEmpty.sortBy incXdecY $ pts

incXdecY  :: Ord r => (MyPoint r) :+ p -> (MyPoint r) :+ q -> Ordering
incXdecY (MyPoint px py :+ _) (MyPoint qx qy :+ _) =
  compare px qx <> compare qy py


-- | Precondition: The list of input points is sorted
hull'          :: (Ord r, Num r) => [MyPoint r :+ p] -> NonEmpty (MyPoint r :+ p)
hull' (a:b:ps) = NonEmpty.fromList $ hull'' [b,a] ps
  where
    hull'' h []      = h
    hull'' h (p:ps') = hull'' (cleanMiddle (p:h)) ps'

    cleanMiddle h@[_,_]                         = h
    cleanMiddle h@(z:y:x:rest)
      | rightTurn (x^.core) (y^.core) (z^.core) = h
      | otherwise                               = cleanMiddle (z:x:rest)
    cleanMiddle _                               = error "cleanMiddle: too few points"

rightTurn       :: (Ord r, Num r) => MyPoint r -> MyPoint r -> MyPoint r -> Bool
rightTurn a b c = ccwP a b c == CW



ccwP :: (Ord r, Num r) => MyPoint r -> MyPoint r -> MyPoint r -> CCW
ccwP p q r = case z `compare` 0 of
              LT -> CW
              GT -> CCW
              EQ -> CoLinear
     where

       MyPoint ux uy = q `subt` p
       MyPoint vx vy = r `subt` p
       z             = ux * vy - uy * vx
