{-# LANGUAGE QuasiQuotes #-}
module Main(main) where

import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.Palette.ColorSet
import           Data.Colour.SRGB (RGB(..),toSRGB24)
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import           Data.Maybe (fromMaybe)
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
import           HGeometry.Polygon.Simple.Class
import           HGeometry.Polygon.Triangulation
import           HGeometry.Transformation
import           HGeometry.Vector
import           HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane, voronoiDiagram)
import           Ipe
import           PLY.Writer
import           System.OsPath
-- import           Test.QuickCheck

import           Debug.Trace
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
pointToPlane' = fmap (/ 100) . pointToPlane


myPlanes :: NonEmpty (Plane R :+ (Int, Point 2 R, Colour Double))
myPlanes = NonEmpty.zipWith (\i (p :+ c) -> pointToPlane' p :+ (i,p,c)) (NonEmpty.fromList [0..])
         $ myPoints

-- myPoints :: NonEmpty (Point 2 R :+ Colour Double)
-- myPoints = NonEmpty.fromList $
--            [ Point2 10 0   :+ red
--            , Point2 0  10  :+ green
--            , Point2 30 10  :+ blue
--            , Point2 10 30  :+ (yellow :: Colour Double)
--            ]

myColors :: NonEmpty (Colour Double)
myColors = NonEmpty.fromList infiniteWebColors

myPoints :: NonEmpty (Point 2 R :+ Colour Double)
myPoints = NonEmpty.zipWith (flip (:+)) myColors $
           NonEmpty.fromList $
           [ Point2 16 80
           , Point2 64 48
           , Point2 208 128
           , Point2 176 48
           , Point2 96 112
           , Point2 128 80
           , Point2 48 144
           ]

-- verticesOf    = NonEmpty.fromList . foldMap F.toList . trianglesOf
-- trianglesOf _ = [ Triangle (origin :+ 0) (Point3 10 0 1  :+ 1) (Point3 0 10 2  :+ 2) ]


-- | Renders the a plane
renderPlaneIn             :: (Plane_ plane r, Point_ corner 2 r, Num r, Ord r
                             , Show corner, Show r)
                          => Rectangle corner -> plane -> ConvexPolygonF NonEmpty (corner :+ r)
renderPlaneIn rect' h = uncheckedFromCCWPoints
                      . NonEmpty.reverse . toNonEmpty -- the corners are listed in CW order
                      . fmap eval
                      $ corners rect'
  where
    eval p = p :+ evalAt p h

toPolygons :: (Plane_ plane r, Ord r, Fractional r, Point_ vertex 2 r)
           => MinimizationDiagram r vertex plane
           -> NonEmpty (plane, ConvexPolygonF NonEmpty (Point 2 r :+ r))
toPolygons = fmap render . NEMap.toAscList . asMap
  where
    render (h,reg) = (h, case toConvexPolygonIn myRect reg of
                           Left pg  -> pg&vertices %~ \v -> (v^.asPoint :+ evalAt v h)
                           Right pg -> pg&vertices %~ \v -> (v^.asPoint :+ evalAt v h)
                     )


myRect :: Num r => Rectangle (Point 2 r)
myRect = let m = 1000 in Box (Point2 (negate m) (negate m)) (Point2 m m)


type Vtx r = (Int, Point 3 r :+ VertexAttributes 'Coloured)

-- | Triangulate the faces.
--
-- maybe actually move this into the ply-writer, since both blender and the weird online
-- viewer apparently could not deal well with non-triangular faces to begin with.
triangulate'        :: (ConvexPolygon_ convexPolygon point r, Ord r, Num r)
                    => (plane, convexPolygon)
                    -> NonEmpty (plane, ConvexPolygon point)
triangulate' (h,pg) = fmap (\simple -> (h, review _UncheckedConvexPolygon $
                                           simple&vertices %~ view core
                                       )
                           )
                    . NonEmpty.fromList
                    . toListOf interiorFacePolygons . triangulate $ pg


-- | Render a minimization diagram
renderMinimizationDiagram     :: (Plane_ plane r, Ord r, Fractional r, HasColour plane
                                 , Point_ vertex 2 r
                                 )
                              => MinimizationDiagram r vertex plane
                              -> ( NonEmpty (Vtx r)
                                 , NonEmpty (NonEmpty Int)
                                 )
renderMinimizationDiagram env = (NonEmpty.fromList vs, NonEmpty.fromList fs)
  where
    (_,vs,fs) = foldr render (0,[],[])
              -- . NonEmpty.fromList . NonEmpty.take 1
              . foldMap1 triangulate'
              . toPolygons $ env

    render                            :: HasColour plane
                                      => ( plane
                                         , ConvexPolygon (Point 2 r :+ r))
                                      -> (Int, [Vtx r], [NonEmpty Int])
                                      -> (Int, [Vtx r], [NonEmpty Int])
    render (h,pg) acc@(i,vsAcc,fsAcc) =
      let vs'    = (\(j, Point2 x y :+ z) -> (i+j, Point3 x y z :+ ats))
                   <$> toNonEmptyOf (vertices.withIndex) pg
          face'  = fst <$> vs'
          ats    = VertexAttributes (colourOf h)
      in (i + length vs', F.toList vs' <> vsAcc, face' : fsAcc)

-- | Draws all the planes
renderPlanes    :: (Plane_ plane r, Ord r, Fractional r, HasColour plane, Foldable1 nonEmpty

                   , Show r
                   )
                => nonEmpty plane -> ( NonEmpty (Vtx r)
                                     , NonEmpty (NonEmpty Int)
                                     )
renderPlanes hs = (NonEmpty.fromList vs, NonEmpty.fromList fs)
  where
    (_,vs,fs) = foldr render (0,[],[]) hs

    render h acc@(i,vsAcc,fsAcc) =
      let pg  = renderPlaneIn myRect h
          vs'    = (\(j, Point2 x y :+ z) -> (i+j, Point3 x y z :+ ats))
                   <$> toNonEmptyOf (vertices.withIndex) pg
          face'  = fst <$> vs'
          ats    = VertexAttributes (colourOf h)
      in (i + length vs', F.toList vs' <> vsAcc, face' : fsAcc)

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
                                      mapM_ (\xs@(h,_) ->
                                               print $ (h, verifyOnPlane xs)
                                            ) $ toPolygons env
                                      -- print vs

                                      renderOutputToFile [osp|myLowerEnv.ply|] vs fs
                                      renderOutputToFile [osp|planes.ply|] vs' fs'
             where
               -- vs'     = vs&traverse._2 %~ scaleBy (Vector3 1 1 (1/100))
               (vs,fs)   = renderMinimizationDiagram env
               (vs',fs') = renderPlanes . NEMap.keys . asMap $ env


-- | make sure that all vertices lie on the plane
verifyOnPlane        :: (Plane_ plane r, Ord r, Fractional r)
                     => (plane, ConvexPolygonF NonEmpty (Point 2 r :+ r)) -> Bool
verifyOnPlane (h,pg) = allOf vertices onPlane pg
  where
    onPlane (Point2 x y :+ z) = onHyperPlane (Point3 x y z) h

-- boundedVertices   :: Fold (MinimizationDiagram r plane) (Point 2 r)
boundedVertices f = foldMap (\case
                                Bounded   pts     -> foldMap f pts
                                Unbounded _ pts _ -> foldMap f pts
                            ) . asMap


-- | compute some sufficiently large rectangle to which we can clip the minimization diagram.
clippingBox :: (Num r, Ord r, Point_ vertex 2 r, IsBoxable vertex
               ) => MinimizationDiagram r vertex plane -> Rectangle (Point 2 r)
clippingBox = grow 10 . boundingBox . NonEmpty.fromList . boundedVertices (:[])


grow                 :: (Num r, Point_ point d r) => r -> Box point -> Box point
grow delta (Box p q) = Box (p&coordinates %~ subtract delta)
                           (q&coordinates %~ (+delta))
