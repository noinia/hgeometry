{-# LANGUAGE QuasiQuotes #-}
module Main(main) where

import           Codec.Picture
import           Control.Lens
import           Data.Foldable as F
import           Data.Maybe
import           Data.Monoid
import           Data.Word
import           HGeometry.Ball
import           HGeometry.Ext
import           HGeometry.HalfLine
import           HGeometry.Intersection
import           HGeometry.Point
import           HGeometry.Vector
import           Paths_hgeometry_examples
import qualified System.File.OsPath as File
import           System.OsPath

--------------------------------------------------------------------------------

type Color = PixelRGBA8

type Scene = [Ball (Point 3 R) :+ Color]

type R = Double

type Ray = HalfLine (Point 3 R)

rayThrough     :: Int -> Int -> Ray
rayThrough x y = let q = Point3 x y 0 &coordinates %~ fromIntegral
                 in HalfLine cameraPos (q .-. cameraPos)

-- | Intersect to try and get the color
ballColor            :: Ray -> Ball (Point 3 R) :+ Color -> Maybe Color
ballColor r (b :+ c)
  | r `intersects` b = Just c
  | otherwise        = Nothing

shootRay   :: Ray -> Scene -> Color
shootRay r = fromMaybe backgroundColor . getFirst . foldMap (First . ballColor r)

render                     :: Vector 2 Int -> Scene -> Image PixelRGBA8
render (Vector2 w h) scene = generateImage f w h
  where
    f     :: Int -> Int -> PixelRGBA8
    f x y = let ray = rayThrough x y
            in shootRay ray scene

    clamped     :: Int -> Int -> Word8
    clamped x m = fromIntegral $ (255 * x) `div` m

backgroundColor :: PixelRGBA8
backgroundColor = PixelRGBA8 maxBound maxBound maxBound 0

theScene :: Scene
theScene = [ Ball (Point3 128 128 128) 20 :+ PixelRGBA8 200 0 0 255
           ]

cameraPos = Point3 0 0 10000


main :: IO ()
main = do
  let bs = encodePng $ render (Vector2 640 480) theScene
  File.writeFile [osp|foo.png|] bs
