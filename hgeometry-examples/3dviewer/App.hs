{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module App(main) where


import           Control.Lens
import           Miso hiding (view)
import           Control.Lens hiding (view, element)
import           Data.Foldable (toList)
import           HGeometry.Number.Real.Rational
import           HGeometry
import           HGeometry.Ext
import           Miso.Html hiding (style_)
import           HGeometry.Kernel
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Miso.Svg
import           HGeometry.Miso.Svg.Canvas (Canvas, blankCanvas, mouseCoordinates)
import qualified HGeometry.Miso.Svg.Canvas as Canvas
import           RenderProps
import           Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NonEmpty
import           Ipe hiding (pages, View)
import           Ipe.Color
import           HGeometry.Graphics.Camera
import           HGeometry.PlaneGraph
import           HGeometry.Box (Corners(..))
import HGeometry.Box qualified as Box
import           Miso.CSS (style_, border)
import           Attr

--------------------------------------------------------------------------------

type R = RealNumber 5


data Model = Model { _triangeles :: NonEmpty (Triangle (Point 3 R) :+ RenderProps)
                   , _camera     :: Camera Double
                   , _canvas     :: Canvas Double
                   }
             deriving stock (Eq)

makeLenses ''Model


----------------------------------------

myTriangles :: NonEmpty (Triangle (Point 3 R) :+ RenderProps)
myTriangles = -- scaleUniformlyBy 5 <$>
            NonEmpty.fromList $
        [ -- ground plane
          Triangle origin (Point3 1 0 0) (Point3 1 1 0) :+ props blue
        , Triangle origin (Point3 1 1 0) (Point3 0 1 0) :+ props blue

        -- left side
        , Triangle origin (Point3 0 1 0) (Point3 0 1 1) :+ props green
        , Triangle origin (Point3 0 1 1) (Point3 0 0 1) :+ props green

        -- front plane
        -- , Triangle origin (Point3 1 0 0) (Point3 1 0 1) :+ red
        -- , Triangle origin (Point3 1 0 1) (Point3 0 1 1) :+ red


        -- back plane
        , Triangle (Point3 0 1 0) (Point3 1 1 0) (Point3 1 1 1) :+ props orange
        , Triangle (Point3 0 1 0) (Point3 1 1 1) (Point3 0 1 1) :+ props orange
        ]
        <> ((\tri -> tri&extra %~ getColor
                         &vertices.coordinates %~ realToFrac
              ) <$> myTriangles'
             )

props c = RenderProps Nothing (Just $ attr SFill c)

getColor :: core :+ RenderProps -> RenderProps
getColor = view extra

myTriangles' :: [Triangle (Point 3 R) :+ (Plane R :+ RenderProps)]
myTriangles' = asTrianglesAbove domain planes

planes :: NonEmpty (Plane R :+ RenderProps)
planes = NonEmpty.fromList
           [ Plane 0 0 (0.5)   :+ props red
           -- , Plane 0 (-0.25) 1 :+ props gray
           ]


-- | fit to something that fits in this rectangle
screenBox :: Rectangle (Point 2 Double)
screenBox = Rectangle origin (Point2 500 500)


-- | The domain in which we want to render the planes
domain :: Rectangle (Point 2 R)
domain = Rectangle origin (Point2 2 1)

-- | Given a rectangular domain, and a set of planes, generates the
-- triangles that represent the planes above the domain.
asTrianglesAbove      :: (Plane_ plane r, Num r, Foldable f)
                      => Rectangle (Point 2 r)
                      -> f plane -> [Triangle (Point 3 r) :+ plane]
asTrianglesAbove rect = foldMap (foldMap (:[]) . asTrianglePairAbove rect)

-- | Given a rectangular domain, and a plane h, generate two triangles
-- that represent the plane above the domain.
--
-- The triangles are in counterclockwise order.
asTrianglePairAbove        :: (Plane_ plane r, Num r)
                           => Rectangle (Point 2 r)
                           -> plane -> Vector 2 (Triangle (Point 3 r) :+ plane)
asTrianglePairAbove rect h = Vector2 (Triangle tl br tr :+ h)
                                     (Triangle tl bl br :+ h)
  where
    Corners tl tr br bl = (\p@(Point2 x y) -> Point3 x y (evalAt p h)) <$> Box.corners rect


initialModel :: Model
initialModel = Model myTriangles blenderCamera (blankCanvas 1024  576)

--------------------------------------------------------------------------------

data Action = CanvasAction Canvas.InternalCanvasAction
            deriving (Show,Eq)

--------------------------------------------------------------------------------

main :: IO ()
main = startApp (Canvas.withCanvasEvents defaultEvents) $
         Miso.component initialModel updateModel viewModel

wrap       :: (model -> action -> Effect parent model action') -> action
           -> Effect parent model action'
wrap f act = get >>= flip f act


--------------------------------------------------------------------------------

updateModel :: Action -> Effect parent Model Action
updateModel = \case
  CanvasAction ca  -> zoom canvas $ wrap Canvas.handleInternalCanvasAction ca

--------------------------------------------------------------------------------

viewModel       :: Model -> View Model Action
viewModel model = div_ [ ]
                       [ either CanvasAction id <$>
                         Canvas.svgCanvas_ (model^.canvas)
                                [ style_ [border "1px solid black"]
                                ]
                                canvasBody
                       ]
  where
    canvasBody = []


--------------------------------------------------------------------------------
