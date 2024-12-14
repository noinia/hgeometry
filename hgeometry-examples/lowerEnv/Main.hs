{-# LANGUAGE QuasiQuotes #-}
module Main(main) where

import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB (RGB(..),toSRGB24)
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import           Data.Ord (comparing)
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope
import           HGeometry.Plane.LowerEnvelope.Connected
import           HGeometry.PlaneGraph
import           HGeometry.PlaneGraph.Instances
import           HGeometry.Point
import           HGeometry.Polygon
import           HGeometry.Polygon.Convex
import           HGeometry.Transformation
import           HGeometry.Vector
import           HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane, voronoiDiagram)
import           Ipe
import           PLY.Writer
import           System.OsPath
-- import           Test.QuickCheck

--------------------------------------------------------------------------------

type R = RealNumber 5


class HasColour plane where
  colourOf :: plane -> Colour Double
instance HasColour (plane :+ (i,point,Colour Double)) where
  colourOf (_ :+ (_,_,c)) = c

instance Ord (Colour Double) where
  compare = comparing (\c -> case toSRGB24 c of
                               RGB r g b -> (r,g,b)
                      )

pointToPlane' :: (Fractional r, Ord r) => Point 2 r -> Plane r
pointToPlane' = fmap (/ 10) . pointToPlane



myPlanes = NonEmpty.zipWith (\i (p :+ c) -> pointToPlane' p :+ (i,p,c)) (NonEmpty.fromList [0..])
         $ myPoints
myPoints = NonEmpty.fromList $
           [ Point2 10 0   :+ red
           , Point2 0  10  :+ green
           , Point2 30 10  :+ blue
           , Point2 10 30  :+ (yellow :: Colour Double)
           ]


           -- [ Point2 16 80
           -- , Point2 64 48
           -- , Point2 208 128
           -- , Point2 176 48
           -- , Point2 96 112
           -- , Point2 128 80
           -- , Point2 48 144
           -- ]

-- verticesOf    = NonEmpty.fromList . foldMap F.toList . trianglesOf
-- trianglesOf _ = [ Triangle (origin :+ 0) (Point3 10 0 1  :+ 1) (Point3 0 10 2  :+ 2) ]

toPolygons :: (Plane_ plane r, Ord r, Fractional r)
           => MinimizationDiagram r plane
           -> NonEmpty (plane, ConvexPolygonF NonEmpty (Point 2 r :+ r))
toPolygons = fmap render . NEMap.toAscList . asMap
  where
    render (h,reg) = (h, case toConvexPolygonIn rect reg of
                           Left pg  -> pg&vertices %~ \v -> (v^.asPoint :+ evalAt v h)
                           Right pg -> pg&vertices %~ \v -> (v^.asPoint :+ evalAt v h)
                     )
    m = 100
    rect = Box (Point2 (negate m) (negate m)) (Point2 m m)

type Vtx r = (Int, Point 3 r :+ VertexAttributes 'Coloured)

-- | Render a minimization diagram
renderMinimizationDiagram     :: (Plane_ plane r, Ord r, Fractional r, HasColour plane)
                              => MinimizationDiagram r plane
                              -> ( NonEmpty (Vtx r)
                                 , NonEmpty (NonEmpty Int)
                                 )
renderMinimizationDiagram env = (NonEmpty.fromList vs, NonEmpty.fromList fs)
  where
    (_,vs,fs) = foldr render (0,[],[])
              -- . NonEmpty.fromList . NonEmpty.take 1
              . toPolygons $ env

    render                            :: HasColour plane
                                      => ( plane
                                         , ConvexPolygonF NonEmpty (Point 2 r :+ r))
                                      -> (Int, [Vtx r], [NonEmpty Int])
                                      -> (Int, [Vtx r], [NonEmpty Int])
    render (h,pg) acc@(i,vsAcc,fsAcc) =
      let vs    = (\(j, Point2 x y :+ z) -> (i+j, Point3 x y z :+ ats))
                  <$> toNonEmptyOf (vertices.withIndex) pg
          face' = fst <$> vs
          ats   = VertexAttributes (colourOf h)
      in (i + length vs, F.toList vs <> vsAcc, face' : fsAcc)


main :: IO ()
main = do
         let myEnvelope = lowerEnvelope myPlanes
             m       = 20
             maxP    = Point3 m m m
             box     = Box (maxP&vector %~ negated) maxP
         print box

         putStrLn "Voronoi Diagram"
         print $ voronoiDiagram myPoints
         print "Lower envelope"

         print myEnvelope




         case myEnvelope of
           ParallelStrips _      -> pure ()
           ConnectedEnvelope env -> do
                                      putStrLn "Regions:"
                                      mapM_ print $ toPolygons env
                                      -- print vs

                                      renderOutputToFile [osp|myLowerEnv.ply|] vs fs
             where
               -- vs'     = vs&traverse._2 %~ scaleBy (Vector3 1 1 (1/100))
               (vs,fs) = renderMinimizationDiagram env



-- boundedVertices   :: Fold (MinimizationDiagram r plane) (Point 2 r)
boundedVertices f = foldMap (\case
                                Bounded   pts     -> foldMap f pts
                                Unbounded _ pts _ -> foldMap f pts
                            ) . asMap


-- | compute some sufficiently large rectangle to which we can clip the minimization diagram.
clippingBox :: (Num r, Ord r) => MinimizationDiagram r plane -> Rectangle (Point 2 r)
clippingBox = grow 10 . boundingBox . NonEmpty.fromList . boundedVertices (:[])


grow                 :: (Num r, Point_ point d r) => r -> Box point -> Box point
grow delta (Box p q) = Box (p&coordinates %~ subtract delta)
                           (q&coordinates %~ (+delta))
