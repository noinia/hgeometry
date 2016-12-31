module Data.Geometry.Ipe.PlanarSubdivision where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Ipe.Attributes
import           Data.Geometry.Ipe
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.PlanarSubdivision
import           Data.Geometry.Polygon hiding (vertices)
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.PlaneGraph
import           Data.Semigroup
import qualified Data.Seq2 as S2
import           Data.Text (Text)
import qualified Data.Vector as V



drawPlaneGraph :: IpeOut (PlaneGraph s Primal_ v e f r) (IpeObject' Group r)
drawPlaneGraph = IpeOut drawPlaneGraph'


drawPlaneGraph'    :: PlaneGraph s w v e f r -> IpeObject' Group r
drawPlaneGraph' gr = Group
                   [ vxs
                   , egs
                   ] :+ mempty
  where
    -- the vertices
    vxs = asIpeGroup . map (drawVertex . snd) . V.toList . vertices $ gr
    egs = asIpeGroup . map (drawEdge gr) . V.toList . edges    $ gr


drawVertex :: Point 2 r :+ p -> IpeObject r
drawVertex = asIpeObject' mempty . (^.core)

-- drawEdge :: _ -> (Dart s, e) -> IpeObject r
drawEdge gr (d,e) = asIpeObject seg $ mempty <> attr SArrow normalArrow
  where
    (ui,vi) = endPoints d gr
    (u,v)   = (gr^.vDataOf ui, gr^.vDataOf vi)
    seg     = ClosedLineSegment u v


draw g = printAsIpeSelection $ asIpeObjectWith drawPlaneGraph g mempty
