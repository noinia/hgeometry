{-# LANGUAGE ScopedTypeVariables #-}
module Data.PlaneGraph.Draw where

import           Data.Ext
import           Data.Geometry.Ipe
import           Data.Geometry.Properties
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Maybe (catMaybes)
import           Data.PlaneGraph
import qualified Data.Vector as V

--------------------------------------------------------------------------------

-- | Draws a planegraph using Marks, LineSegments, and simple polygons for
-- vertices, edges, and faces, respectively. Uses the default IpeOuts to draw
-- these elements.
drawPlaneGraph :: forall s v e f r. IpeOut (PlaneGraph s v e f r) Group r
drawPlaneGraph = drawPlaneGraphWith defIO' defIO' defIO'
  where
    defIO'     :: (HasDefaultIpeOut g, NumType g ~ r) => g -> _x -> Maybe (IpeObject r)
    defIO' p _ = Just . iO $ defIO p

-- | Draws a planegraph using Marks, LineSegments, and simple polygons for
-- vertices, edges, and faces, respectively.
drawPlaneGraphWith            :: (Point 2 r         -> v -> Maybe (IpeObject r))
                              -> (LineSegment 2 v r -> e -> Maybe (IpeObject r))
                              -> (SimplePolygon v r -> f -> Maybe (IpeObject r))
                              -> IpeOut (PlaneGraph s v e f r) Group r
drawPlaneGraphWith vF eF fF g = ipeGroup $ concatMap (catMaybes . V.toList) [vs, es, fs]
  where
    vs = (\(_,VertexData p v) -> vF p v) <$> vertices g
    es = (\(_,s :+ e)         -> eF s e) <$> edgeSegments g
    fs = (\(_,p :+ f)         -> fF p f) <$> rawFacePolygons g


-- | Draw a planegraph using the given functions. Fully generic in how we draw
-- the objects.
genericDrawPlaneGraphWith            :: (VertexId' s :+ v -> IpeObject r)
                                     -> (Dart s :+ e      -> IpeObject r)
                                     -> (FaceId' s :+ f   -> IpeObject r)
                                     -> IpeOut (PlaneGraph s v e f r) Group r
genericDrawPlaneGraphWith vF eF fF g = ipeGroup $ concatMap V.toList [vs, es, fs]
  where
    vs = (\(v,VertexData _ x) -> vF $ v :+ x) <$> vertices g
    es = wrap eF <$> edges g
    fs = wrap fF <$> faces g

    wrap f (a,b) = f $ a :+ b
