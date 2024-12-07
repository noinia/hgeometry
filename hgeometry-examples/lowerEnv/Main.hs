{-# LANGUAGE QuasiQuotes #-}
module Main(main) where

import           Control.Lens
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Number.Real.Rational
import           HGeometry.Plane.LowerEnvelope
import           HGeometry.Plane.LowerEnvelope.Connected
import           HGeometry.PlaneGraph
import           HGeometry.PlaneGraph.Instances
import           HGeometry.Point
import           HGeometry.Transformation
import           HGeometry.Vector
import           HGeometry.VoronoiDiagram.ViaLowerEnvelope (pointToPlane)
import           Ipe
import           PLY.Writer
import           System.OsPath
-- import           Test.QuickCheck

--------------------------------------------------------------------------------

type R = RealNumber 5


myPlanes = NonEmpty.fromList $ zipWith (\i p -> pointToPlane p :+ (i,p)) [0..]
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


-- | Render a minimization diagram
renderMinimizationDiagram     :: (Plane_ plane r, Ord r, Fractional r)
                              => MinimizationDiagram r plane
                              -> ( NonEmpty (Point 3 r :+ Int)
                                 , NonEmpty (NonEmpty Int)
                                 )
renderMinimizationDiagram env = (NonEmpty.fromList vs, NonEmpty.fromList fs)
  where
    (_,vs,fs) = foldr f (0,[],[]) . Map.toAscList . asMap $ env
    f (h,reg) = case reg of
      Bounded pts       -> g h pts
      Unbounded u pts v -> let p  = NonEmpty.head pts
                               q  = NonEmpty.last pts
                               p' = p .-^ (10 *^ v) -- TODO: clip this somewhere
                               q' = q .+^ (10 *^ u) -- TODO: clip this somewhere
                               pts' = q' : p' : F.toList pts
                           in g h pts'

    g h pts acc@(i,vsAcc,fsAcc) = let k     = length pts
                                      vs'   = zipWith (\q@(Point2 x y) j ->
                                                         let z = 0 -- evalAt q h
                                                         in  Point3 x y z :+ j) pts face'
                                      face' = take k [i,(i+1)..]
                                  in ( i+k
                                     , vs' <> vsAcc
                                     , NonEmpty.fromList face' : fsAcc
                                     )

main :: IO ()
main = do
         let myEnvelope = lowerEnvelope myPlanes
             m       = 20
             maxP    = Point3 m m m
             box     = Box (maxP&vector %~ negated) maxP
         print box
         print myEnvelope
         case myEnvelope of
           ParallelStrips _      -> pure ()
           ConnectedEnvelope env -> renderOutputToFile [osp|myLowerEnv.ply|] vs' fs
             where
               vs'     = fitToBox box . scaleBy (Vector3 1 1 (1/100)) $ vs
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
