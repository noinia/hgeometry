module RenderCanvas where

import           Control.Lens
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Box
import           SDL.Cairo.Canvas (Canvas)
import qualified SDL.Cairo.Canvas as Canvas

import Data.Geometry.Ipe.Types hiding (ipeObject', width)

rectangle    :: (Real r, Ord r, Num r) => Rectangle p r -> Canvas ()
rectangle r' = let r                  = bimap id realToFrac r'
                   (Point2 x y :+ _,_,_,_) = corners r
               in Canvas.rect $ Canvas.D x y (width r) (height r)

polygon     :: Real r => SimplePolygon p r -> Canvas ()
polygon pg' = let pg = bimap id realToFrac pg'
              in Canvas.polygon $ pg^..outerBoundary.traverse.core.vector.to toV2

lineSegment    :: Real r => LineSegment 2 p r -> Canvas ()
lineSegment s' = let s = bimap id realToFrac s'
                 in Canvas.line (s^.start.core.vector.to toV2) (s^.end.core.vector.to toV2)

polyLine    :: Real r => PolyLine 2 p r -> Canvas ()
polyLine p' = let p = bimap id realToFrac p'
              in Canvas.shape Canvas.ShapeLines $ p^..points.traverse.core.vector.to toV2

point   :: Real r => Point 2 r -> Canvas ()
point p = Canvas.point . fmap realToFrac $ p^.vector.to toV2


pathSegment                     :: Real r => PathSegment r -> Canvas ()
pathSegment (PolyLineSegment p) = polyLine p
pathSegment (PolygonPath p)     = polygon p
pathSegment _                   = error "pathSegment: Not implemented yet"


ipeUse              :: Real r => IpeSymbol r -> Canvas ()
ipeUse (Symbol p _) = Canvas.circle' (realToFrac <$> p^.vector.to toV2) 10

ipePath          :: Real r => Path r -> Canvas ()
ipePath (Path p) = mapM_ pathSegment p

ipeObject' :: (g r -> Canvas ())
           -> g r :+ IpeAttributes g r
           -> Canvas ()
ipeObject' f (i :+ ats) = do
                            Canvas.pushMatrix
                            applyAttributes ats
                            f i
                            Canvas.popMatrix

ipeObject :: Real r => IpeObject r -> Canvas ()
ipeObject (IpeGroup _)     = undefined
ipeObject (IpeImage _)     = undefined
ipeObject (IpeTextLabel _) = undefined
ipeObject (IpeMiniPage _)  = undefined
ipeObject (IpeUse p)       = ipeObject' ipeUse  p
ipeObject (IpePath p)      = ipeObject' ipePath p


-- applyAttributes   :: IpeAttributes g r -> Canvas ()
applyAttributes   :: t -> Canvas ()
applyAttributes _ = Canvas.stroke $ Canvas.blue 128
