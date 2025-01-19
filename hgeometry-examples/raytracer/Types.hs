{-# LANGUAGE TemplateHaskell #-}
module Types where

import           Control.Lens
import           Control.Monad (replicateM, (<=<), (=<<))
import           Data.Colour as Colour
import           Data.Colour.Names
import           Data.Colour.SRGB (toSRGB24, RGB(..), sRGB)
import           Data.Default.Class
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe
import           Data.Monoid
import           Data.Semialign
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Direction
import           HGeometry.Ext
import           HGeometry.Graphics.Camera
import           HGeometry.HalfLine
import           HGeometry.Intersection
import           HGeometry.LineSegment
import           HGeometry.Number.Radical
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Triangle
import           HGeometry.Unbounded
import           HGeometry.Vector
import           Prelude hiding (zipWith)
import qualified System.File.OsPath as File
import           System.OsPath
import           System.ProgressBar
import           System.Random.Stateful

--------------------------------------------------------------------------------


-- | The color type we use
type Color = AlphaColour Double

-- | The type of Real numbers
type R = Double

-- | A ray; i.e. a halfline
type Ray = HalfLine (Point 3 R)

-- | Supported geometry types
data SceneGeom = ABall     (Ball (Point 3 R))
               | ATriangle (Triangle (Point 3 R))
               deriving (Show,Eq)

type instance NumType   SceneGeom = R
type instance Dimension SceneGeom = 3


type instance Intersection Ray SceneGeom = Maybe (Point 3 R :+ R)
-- we just compute the first intersection;

instance Ray `HasIntersectionWith` SceneGeom
instance Ray `IsIntersectableWith` SceneGeom where
  intersect ray =  \case
    ABall b      -> ray `intersect` b <&> \case
      Line_x_Ball_Point p     -> p
      Line_x_Ball_Segment seg -> seg^.start
    ATriangle tri -> ray `intersect` tri <&> \case
      Line_x_Triangle_Point p         -> p
      Line_x_Triangle_LineSegment seg -> seg^.start

instance CanComputeNormalVector SceneGeom R where
  normalVectorAt q = \case
    ABall b     -> normalVectorAt q b
    ATriangle t -> normalVectorAt q t

-- | Every object is some geometric object and a color
data SceneObject = SceneObject { _geom        :: SceneGeom
                               , _objectColor :: Color
                               }
                 deriving (Show,Eq)
makeLenses ''SceneObject

-- | A scene is just a list of objects
type Scene = [SceneObject]

type RayDepth = Int

--------------------------------------------------------------------------------


-- | We describe the viewport by its topleft corner and its bottom right corner.
type ViewPort = Vector 2 (Point 3 R)

data PixelInfo = PixelInfo { _coords :: Point 2 Int
                           -- ^ The pixel coordinates
                           , _pixelViewport :: ViewPort
                           -- ^ The part of the viewport corresponding to this pixel.
                           , _pixelDimensions :: Vector 2 (Vector 3 R)
                           -- ^ the pixel axes
                           } deriving (Show,Eq)

makeLenses ''PixelInfo
