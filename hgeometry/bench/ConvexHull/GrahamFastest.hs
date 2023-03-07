module ConvexHull.GrahamFastest( convexHull
                               , upperHull
                               , lowerHull, fromP
                               ) where


import           Control.DeepSeq
import           Control.Lens ((^.))
import           HGeometry.Foldable.Sort
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid
import qualified Data.Vector.Unboxed as Vector
import           GHC.Generics
import           HGeometry.Point
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

--------------------------------------------------------------------------------

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
convexHull ps        = let ps' = Vector.toList . sortBy incXdecY $ ps
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
                   . Vector.toList . sortBy incXdecY $ pts

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



--------------------------------------------------------------------------------

-- | elements of the vector are stored consecutively
newtype instance UMV.MVector s MyPoint = MV_MyPoint (UMV.MVector s Int)
newtype instance UV.Vector     MyPoint = V_MyPoint  (UV.Vector     Int)

instance GMV.MVector UMV.MVector MyPoint where
  basicLength (MV_MyPoint v) = GMV.basicLength v `div` 2
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (MV_MyPoint v) = MV_MyPoint $ GMV.basicUnsafeSlice (2*s) (2*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps  (MV_MyPoint v) (MV_MyPoint v') = GMV.basicOverlaps v v'
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_MyPoint <$> GMV.basicUnsafeNew (2*n)
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MV_MyPoint v) = GMV.basicInitialize v
  {-# INLINE basicInitialize#-}
  basicUnsafeRead (MV_MyPoint v) i = do x <- GMV.basicUnsafeRead v  (2*i)
                                        y <- GMV.basicUnsafeRead v (2*i+1)
                                        pure $ MyPoint x y
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_MyPoint v) i (MyPoint x y) = do GMV.basicUnsafeWrite v (2*i)   x
                                                       GMV.basicUnsafeWrite v (2*i+1) y
  {-# INLINE basicUnsafeWrite #-}


-- type instance GV.Mutable UV.Vector2

instance GV.Vector UV.Vector MyPoint where

  basicUnsafeFreeze (MV_MyPoint mv) = V_MyPoint <$> GV.basicUnsafeFreeze mv
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_MyPoint v) = MV_MyPoint <$> GV.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_MyPoint v) = GV.basicLength v `div` 2
  {-# INLINE basicLength #-}
  basicUnsafeSlice s l (V_MyPoint v) = V_MyPoint $ GV.basicUnsafeSlice (2*s) (2*l) v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_MyPoint v) i = MyPoint <$> GV.basicUnsafeIndexM v (2*i)
                                              <*> GV.basicUnsafeIndexM v (2*i+1)
  {-# INLINE basicUnsafeIndexM #-}

instance UV.Unbox MyPoint
