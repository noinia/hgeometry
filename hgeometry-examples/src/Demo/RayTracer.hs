module Demo.RayTracer where

-- import Algorithms.Geometry.HiddenSurfaceRemoval (compareDepthOrder, Tri)
import           Data.Ord (comparing)
import qualified Data.List as List
import           Data.Maybe (mapMaybe)
import           Data.Ext
import           Control.Lens
import           Data.Geometry.Line
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.Geometry.Triangle
import           Data.Range
import           Data.Util
import           Graphics.Camera

--------------------------------------------------------------------------------

type Picture = ()


render                    ::  (Fractional r, Ord r)
                          => Vector 2 Int -- ^ Screen size (i.e. number of pixels)
                          -> Camera r -> [Triangle 3 p r :+ f] -> Picture
render (Vector2 w h) c ts = fromPixels
              [ fmap colorOf . firstInRange vr
                $ shootRay (c^.cameraPosition) (toPoint x y) ts
              | x <- [1..w], y <- [1..h]
              ]
  where

    vr = ClosedRange (c^.nearDist) (c^.farDist)

    toPoint x y = undefined

fromPixels :: [Maybe Color] -> Picture
fromPixels = undefined

type Color = ()

colorOf   :: Triangle 3 p r :+ f -> Color
colorOf _ = undefined


firstInRange    :: Ord r => Range r -> [SP (Triangle 3 p r :+ f) r]
                -> Maybe (Triangle 3 p r :+ f)
firstInRange vr = fmap (^._1) . minimumOn (^._2) . filter ((`inRange` vr) . (^._2))


minimumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn f = go
  where
    go [] = Nothing
    go xs = Just $ List.minimumBy (comparing f) xs


-- | Shoot a ray from p through q. Report the triangles intersected by the ray
-- and their distance from p.
--
--
shootRay        :: (Fractional r, Ord r) => Point 3 r -> Point 3 r -> [Triangle 3 p r :+ f]
                -> [SP (Triangle 3 p r :+ f) r]
shootRay p q ts = mapMaybe ((lineThrough p q) `intersectT`) ts

-- | reports the intersection point with the squared distance to the intersection point
intersectT     :: Line 3 r -> Triangle 3 p r :+ f -> Maybe (SP (Triangle 3 p r :+ f) r)
intersectT = undefined
-- intersectT l t = case l `intersects` t of
--                    p -> Just (SP t $ quadrance (l^.origin) p)
--                    _ -> Nothing
