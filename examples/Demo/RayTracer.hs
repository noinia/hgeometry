module Demo.RayTracer where

import Data.Ext
import Data.Util
import Graphics.Camera
import Data.Geometry.Point
import Data.Geometry.Line
import Data.Geometry.Triangle
import Algorithms.Geometry.HiddenSurfaceRemoval(compareDepthOrder, Tri)

--------------------------------------------------------------------------------

type Picture = ()


render      :: Camera r -> [Triangle 3 p r :+ f] -> Picture
render c ts = fromPixels
              [ colorOf . firstInRange vr $ shootRay (c^.cameraPosition) (toPoint x y) ts
              | x <- [0..(w-1)], y <- [0..(h-1)]
              ]
  where
    Vector w h = c^.screenDimensions
    vr = ClosedRange (c^.nearDist) (c^.farDist)

    toPoint x y = undefined

fromPixels :: [Color] -> Picture
fromPixels = undefined

type Color = ()

colorOf   :: Triangle 3 p r :+ f :+ r -> Color
colorOf _ = undefined


firstInRange    :: Range r -> [SP (Triangle 3 p r :+ f) r]
                -> Maybe (Triangle 3 p r :+ f)
firstInRange vr = fmap (^._1) . minimumOn (^._2) . filter ((`inRange`) . (^._2))


minimumOn   :: Ord b => (a -> b) -> [a] -> Maybe a
minimumOn f = go
  where
    go [] = Nothing
    go xs = minimumBy (comparing `on` f) xs


-- | Shoot a ray from p through q. Report the triangles intersected by the ray
-- and their distance from p.
--
--
shootRay        :: Point 3 r -> Point 3 r -> [Triangle 3 p r :+ f]
                -> [SP (Triangle 3 p r :+ f) r]
shootRay p q ts = mapMaybe ((lineThrough p q) `intersectT`) ts

-- | reports the intersection point with the squared distance to the intersection point
-- intersectT     :: Line 3 r -> Triangle 3 p r :+ f -> Maybe (SP (Triangle 3 p r :+ f) r)
-- intersectT l t = case l `intersects` t of
--                    p -> Just (SP t $ quadrance (l^.origin) p)
--                    _ -> Nothing
