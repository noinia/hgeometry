module ConvexHull.GrahamClassy( convexHull
                              , sort'
                              -- , upperHull
                              -- , lowerHull
                              , fromP
                              ) where


import           Control.DeepSeq
import           Control.Lens ((^.))
import           HGeometry.Foldable.Sort
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Monoid
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.Generics
import qualified HGeometry.ConvexHull.GrahamScan as GrahamScan
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Properties
import           HGeometry.Vector
-- import qualified Int.Unpacked
--------------------------------------------------------------------------------

sort' :: NonEmpty MyPoint -> NonEmpty MyPoint
sort' = NonEmpty.fromList . UV.toList . sort


-- data MyVector = MyVector {-# UNPACK #-}!Int
--                          {-# UNPACK #-}!Int
--              deriving (Show,Eq,Ord,Generic)

type MyPoint = PointF Int.Unpacked.Vec2

-- type instance NumType MyVector = Int
-- type instance Dimension MyVector = 2

-- instance NFData MyVector

-- instance Ix

-- instance Vector_ MyVector 2 Int where



fromP (Point2_ x y) = Point $ Int.Unpacked.Vec2 x y

-- (MyVector x y) `subt` (MyVector a b) = Int.Unpacked.Vec2 (x-a) (y-b)


-- newtype ConvexPolygon = ConvexPolygon [MyVector] deriving (NFData)

-- | \(O(n \log n)\) time ConvexHull using Graham-Scan. The resulting polygon is
-- given in clockwise order.
convexHull :: NonEmpty MyPoint -> ConvexPolygon MyPoint
convexHull = GrahamScan.convexHull


-- convexHull (p :| []) = ConvexPolygon [p]
-- convexHull ps        = let ps' = Vector.toList . sortBy incXdecY $ ps
--                            uh  = NonEmpty.tail . hull' $         ps'
--                            lh  = NonEmpty.tail . hull' $ reverse ps'
--                        in ConvexPolygon . reverse $ lh ++ uh

-- upperHull  :: NonEmpty MyVector -> NonEmpty MyVector
-- upperHull = hull id

-- lowerHull :: NonEmpty MyVector -> NonEmpty MyVector
-- lowerHull = hull reverse


-- -- | Helper function so that that can compute both the upper or the lower hull, depending
-- -- on the function f
-- hull               :: ([MyVector] -> [MyVector])
--                    -> NonEmpty MyVector -> NonEmpty MyVector
-- hull _ h@(_ :| []) = h
-- hull f pts         = hull' .  f
--                    . Vector.toList . sortBy incXdecY $ pts

-- incXdecY :: MyVector -> MyVector -> Ordering
-- incXdecY (MyVector px py) (MyVector qx qy) =
--   compare px qx <> compare qy py


-- -- | Precondition: The list of input points is sorted
-- hull'          :: [MyVector] -> NonEmpty MyVector
-- hull' (a:b:ps) = NonEmpty.fromList $ hull'' [b,a] ps
--   where
--     hull'' h []      = h
--     hull'' h (p:ps') = hull'' (cleanMiddle (p:h)) ps'

--     cleanMiddle h@[_,_]           = h
--     cleanMiddle h@(z:y:x:rest)
--       | rightTurn x y z           = h
--       | otherwise                 = cleanMiddle (z:x:rest)
--     cleanMiddle _                 = error "cleanMiddle: too few points"

-- rightTurn       :: MyVector -> MyVector -> MyVector -> Bool
-- rightTurn a b c = ccwP a b c == CW


-- ccwP       :: MyVector -> MyVector -> MyVector -> CCW
-- ccwP p q r = case z `compare` 0 of
--               LT -> CW
--               GT -> CCW
--               EQ -> CoLinear
--      where

--        MyVector ux uy = q `subt` p
--        MyVector vx vy = r `subt` p
--        z             = ux * vy - uy * vx



--------------------------------------------------------------------------------

-- -- | elements of the vector are stored consecutively
-- newtype instance UMV.MVector s MyVector = MV_MyVector (UMV.MVector s Int)
-- newtype instance UV.Vector     MyVector = V_MyVector  (UV.Vector     Int)

-- instance GMV.MVector UMV.MVector MyVector where
--   basicLength (MV_MyVector v) = GMV.basicLength v `div` 2
--   {-# INLINE basicLength #-}
--   basicUnsafeSlice s l (MV_MyVector v) = MV_MyVector $ GMV.basicUnsafeSlice (2*s) (2*l) v
--   {-# INLINE basicUnsafeSlice #-}
--   basicOverlaps  (MV_MyVector v) (MV_MyVector v') = GMV.basicOverlaps v v'
--   {-# INLINE basicOverlaps #-}
--   basicUnsafeNew n = MV_MyVector <$> GMV.basicUnsafeNew (2*n)
--   {-# INLINE basicUnsafeNew #-}
--   basicInitialize (MV_MyVector v) = GMV.basicInitialize v
--   {-# INLINE basicInitialize#-}
--   basicUnsafeRead (MV_MyVector v) i = do x <- GMV.basicUnsafeRead v  (2*i)
--                                          y <- GMV.basicUnsafeRead v (2*i+1)
--                                          pure $ MyVector x y
--   {-# INLINE basicUnsafeRead #-}
--   basicUnsafeWrite (MV_MyVector v) i (MyVector x y) = do GMV.basicUnsafeWrite v (2*i)   x
--                                                          GMV.basicUnsafeWrite v (2*i+1) y
--   {-# INLINE basicUnsafeWrite #-}


-- -- type instance GV.Mutable UV.Vector2

-- instance GV.Vector UV.Vector MyVector where

--   basicUnsafeFreeze (MV_MyVector mv) = V_MyVector <$> GV.basicUnsafeFreeze mv
--   {-# INLINE basicUnsafeFreeze #-}
--   basicUnsafeThaw (V_MyVector v) = MV_MyVector <$> GV.basicUnsafeThaw v
--   {-# INLINE basicUnsafeThaw #-}
--   basicLength (V_MyVector v) = GV.basicLength v `div` 2
--   {-# INLINE basicLength #-}
--   basicUnsafeSlice s l (V_MyVector v) = V_MyVector $ GV.basicUnsafeSlice (2*s) (2*l) v
--   {-# INLINE basicUnsafeSlice #-}
--   basicUnsafeIndexM (V_MyVector v) i = MyVector <$> GV.basicUnsafeIndexM v (2*i)
--                                                 <*> GV.basicUnsafeIndexM v (2*i+1)
--   {-# INLINE basicUnsafeIndexM #-}

-- instance UV.Unbox MyVector
