module Graphics.CameraSpec where

import Control.Lens
import Data.Ext
import Data.Geometry.Point
import Data.Geometry.Transformation
import Data.Geometry.Triangle
import Data.Geometry.Vector
import Graphics.Camera
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = pure ()


myCamera :: Camera Rational
myCamera = Camera (Point3 50 0 50)
                  (Vector3 0 0 (-1))
                  (Vector3 0 1 0)
                  10
                  15
                  55
                  (Vector2 800 600)


myCamera1 :: Camera Double
myCamera1 = Camera origin
                  (Vector3 0 0 (-1))
                  (Vector3 0 1 0)
                  10
                  10
                  50 -- we can see up to the origin
                  (Vector2 60 40)

testProjection   :: Camera Double -> [Vector 3 Double]
testProjection c = map (transformBy t) [Vector3 30 30 (-10), Vector3 (30*50/10) 30 (-50)]
  where
    t = perspectiveProjection c

myT :: Triangle 3 () Rational
myT = Triangle (ext $ Point3 1  1  10)
               (ext $ Point3 20 1  10)
               (ext $ Point3 20 30 10)



testToWorld   :: Camera Double -> [Vector 3 Double]
testToWorld c = map (transformBy t) [u, v, n, Vector3 80 20 40]
  where
    u = (c^.rawViewUp) `cross` n
    v = n `cross` u
    n = (-1) *^ c^.rawCameraNormal -- we need the normal from the scene *into* the camera
    t = worldToView c


testRotate   :: Camera Double -> [Vector 3 Double]
testRotate c = map (transformBy t) [u, v, n]
  where
    u = (c^.rawViewUp) `cross` n
    v = n `cross` u
    n = (-1) *^ c^.rawCameraNormal -- we need the normal from the scene *into* the camera
    t = rotateCoordSystem c
