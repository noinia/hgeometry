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
import           System.ProgressBar

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

renderPixel                         :: Vector 2 Int -> Scene -> Int -> Int -> PixelRGBA8
renderPixel (Vector2 w h) scene x y = shootRay ray scene
  where
    ray = rayThrough x y

    -- clamped     :: Int -> Int -> Word8
    -- clamped x m = fromIntegral $ (255 * x) `div` m

renderWithProgress :: IO () -> Vector 2 Int -> Scene -> IO (Image PixelRGBA8)
renderWithProgress reportProgress dims@(Vector2 w h) scene =
    withImage w h $ \x y -> let !pix = renderPixel dims scene x y
                            in pix <$ reportProgress

--------------------------------------------------------------------------------
-- * Settings

backgroundColor :: PixelRGBA8
backgroundColor = PixelRGBA8 maxBound maxBound maxBound 0

theScene :: Scene
theScene = [ Ball (Point3 128 128 128) 50 :+ PixelRGBA8 200 0 0 255
           ]


cameraPos = Point3 0 0 10000


aspectRatio :: Rational
aspectRatio = 16 / 9

outputWidth :: Int
outputWidth = 400

outputDimensions :: Vector 2 Int
outputDimensions = Vector2 outputWidth (ceiling $ fromIntegral outputWidth / aspectRatio)

refreshRate :: Double
refreshRate = 10

--------------------------------------------------------------------------------

amountOfWork (Vector2 w h) = w * h

main :: IO ()
main = do
  let initialProgress = Progress 0 (amountOfWork outputDimensions) ()
  progressBar <- newProgressBar defStyle refreshRate initialProgress

  imageData <- renderWithProgress (incProgress progressBar 1)
                                  outputDimensions theScene

  let bs = encodePng imageData
  File.writeFile [osp|foo.png|] bs
