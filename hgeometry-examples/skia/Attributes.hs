{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Attributes
  ( Attributes(..)
  , HasColoring(..)

  , thickness
  , Thickness(..)
  ) where

import           Color
import           Control.Lens hiding (view, element)
import           Control.Monad (forM_)
import           Control.Monad.Error.Class
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Colour.SRGB
import           Data.Default.Class
import qualified Data.IntMap as IntMap
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import           GHC.TypeNats
import           GHCJS.Marshal
import           GHCJS.Types
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.Interval
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Vector
import           HGeometry.Viewport (ZoomConfig(..), currentLevel, range)
import           HGeometry.VoronoiDiagram
import qualified Language.Javascript.JSaddle as JSAddle
import           Language.Javascript.JSaddle.Object (jsg1, jsg2, jsf, js1, jsg)
import qualified Language.Javascript.JSaddle.Object as JS
import           Layers
import           Miso
import           Miso.Bulma.Color
import           Miso.Bulma.Columns
import           Miso.Bulma.Generic
import qualified Miso.Bulma.JSAddle as Run
import           Miso.String (MisoString,ToMisoString(..), ms)
import           Modes
import           Options
import qualified SkiaCanvas
import           SkiaCanvas (mouseCoordinates, dimensions, canvasKitRefs, surfaceRef)
import qualified SkiaCanvas.CanvasKit as CanvasKit
import           SkiaCanvas.CanvasKit hiding (Style(..))
import qualified SkiaCanvas.Render as Render
import           StrokeAndFill

--------------------------------------------------------------------------------

data family Attributes geom

--------------------------------------------------------------------------------

data Thickness = Thin | Normal | Thick deriving (Show,Read,Eq,Ord)

toInt :: Thickness -> MisoString
toInt = ms @Int . \case
  Thin   -> 1
  Normal -> 2
  Thick  -> 3

instance Default Thickness where
  def = Normal

--------------------------------------------------------------------------------
-- * Point Attributes

data instance Attributes (Point 2 r) =
  PointAttributes { _coloring :: Coloring
                  } deriving (Show,Eq)

instance Default (Attributes (Point 2 r)) where
  def = PointAttributes def


class HasColoring t where
  -- | Lens to access the colloring attribute
  coloring :: Lens' t Coloring

instance HasColoring (Attributes (Point 2 r)) where
  coloring = lens _coloring (\ats c -> ats { _coloring = c })

--------------------------------------------------------------------------------
-- * PolyLine Attributes

data instance Attributes (PolyLineF f (Point 2 r)) =
  PolyLineAttributes { _polyLineStrokeColor :: {-# UNPACK #-} !Color
                     , _thickness           :: {-# UNPACK #-} !Thickness
                     } deriving (Show,Eq)

instance HasColor (Attributes (PolyLineF f (Point 2 r))) where
  color = lens _polyLineStrokeColor (\ats c -> ats {_polyLineStrokeColor = c})
  {-# INLINE color #-}

-- | Lens to access the Thickness attribute
thickness :: Lens' (Attributes (PolyLineF f (Point 2 r))) Thickness
thickness = lens _thickness (\ats t -> ats { _thickness = t })


instance Default (Attributes (PolyLineF f (Point 2 r))) where
  def = PolyLineAttributes def def


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * PolyLine Attributes

data instance Attributes (Rectangle (Point 2 r)) =
  RectangleAttributes { _rectColoring    :: {-# UNPACK #-} !Coloring
                      , _rectThickness   :: {-# UNPACK #-} !Thickness
                      } deriving (Show,Eq)

instance HasColoring (Attributes (Rectangle (Point 2 r))) where
  coloring = lens _rectColoring (\ats c -> ats { _rectColoring = c })

instance Default (Attributes (Rectangle (Point 2 r))) where
  def = RectangleAttributes def def
