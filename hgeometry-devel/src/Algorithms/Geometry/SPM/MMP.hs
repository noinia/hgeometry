module Algorithms.Geometry.SPM.MMP
  (
  ) where

import qualified Data.EnumMap.Strict as EnumMap
import           Data.EnumMap.Strict (EnumMap)
import           Data.Ext
import           Data.PlaneGraph
import qualified Data.Set as Set
import           Data.UnBounded
import           Geometry.PlanarSubdivision
import           Geometry.Point

--------------------------------------------------------------------------------


-- ideally we want one world per face,
data MyWorld

data WithDistance r v = WithDistance { _distanceToSource    :: !r
                                     -- ^ distance to the source
                                     , _predecessorToSource :: !(Maybe (VertexId' s))
                                     -- ^ Nothing if this is the source itself.
                                     }
                      deriving (Show,Eq)

-- | every vertex is labeled with its distance every fragment contains
-- a planar subdivision htat has the SPM in that fragment.
type ShortestPathMap v e f r =
  PlanarSubdivision (WithDistance r v) e f r

-- | Triangulated planar subdivision.
type Triangles s v e f r =
  PlanarSubdivision s v e f r



-- | Compteu the shortest path map
shortestPathMap :: Point 2 r -- ^ the source point
                -> PlanarSubdivision s v e f r
                -> ShortestPathMap v e f r
shortestPathMap = undefined



-- |
shortestPathMap' :: VertexId' s -- ^ the source point
                 -> Triangles s v e f r --
                 -> ShortestPathMap v e f r
shortestPathMap' = undefined

--------------------------------------------------------------------------------


-- | A generator gives rise to a candidate interval on an edge.
data Generator s r = Generator { _unfoldedRoot :: !(Point 2 r)
                               , _root         :: !(VertexId' s)
                               , _initialDist  :: !r
                               } deriving (Show)

newtype EdgeSubdivision s r =
  EdgeSubdivision (Set.Set (Generator s r))


data EdgeSPM s r = EdgeSPM { _vertexDistances  :: EnumMap (VertexId' s) r
                           , _edgeSubdivisions :: EnumMap (Dart s)      (EdgeSubdivision s r)
                           }
                 deriving (Show)

-- | Shortest path map decomposition of the edges
edgeShortestPathMap :: VertexId' s -- ^ the source
                    -> Triangles s v e f r
                    -> PlanarSubdivision s (WithDistance r v)
                                           (e, EdgeSubdivision s r)
                                           f
                                           r
edgeShortestPathMap = undefined



-- | for every portal edge the subidision up to f-free paths
edgeShortestPathMapPortalEdge :: VertexId' s
                              -> Triangles s v e f r
                              -> EdgeSPM s r
edgeShortestPathMapPortalEdge = undefined
