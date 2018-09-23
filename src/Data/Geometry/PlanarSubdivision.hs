{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Geometry.PlanarSubdivision( module Data.Geometry.PlanarSubdivision.Basic
                                      , fromPolygon, fromPolygons
                                      ) where

-- import           Algorithms.Geometry.PolygonTriangulation.Triangulate
import           Control.Lens hiding (holes, holesOf, (.=))
import qualified Data.CircularSeq as CSeq
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Box
import           Data.Geometry.PlanarSubdivision.Basic
import           Data.Geometry.Polygon
import           Data.Geometry.Vector
import qualified Data.Geometry.Vector as Vec
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Vector as V
import qualified Data.PlaneGraph as PG
import           Data.Proxy


-- | Construct a planar subdivision from a polygon. Since our PlanarSubdivision
-- models only connected planar subdivisions, this may add dummy/invisible
-- edges.
--
-- running time: \(O(n)\) for a simple polygon, \(O(n\log n)\) for a polygon
-- with holes.
fromPolygon                              :: forall proxy t p f r s.
                                            (Ord r, Fractional r) => proxy s
                                         -> Polygon t p r
                                         -> f -- ^ data inside
                                         -> f -- ^ data outside the polygon
                                         -> PlanarSubdivision s p () f r
fromPolygon p pg@(SimplePolygon _) iD oD = fromSimplePolygon p pg iD oD
fromPolygon _ (MultiPolygon vs hs) iD oD = PlanarSubdivision cs vd dd fd
  where
    wp = Proxy :: Proxy (Wrap s)

    -- the components
    cs = undefined
    cs' = PG.fromSimplePolygon wp (SimplePolygon vs) iD oD
        : map (\h -> PG.fromSimplePolygon wp h oD iD) hs

    vd = undefined
    dd = undefined
    fd = undefined




-- | Given a list of *disjoint* polygons, construct a planarsubdivsion
-- representing them. This may create dummy vertices which have no vertex data,
-- hence the 'Maybe p' data type for the vertices.
--
-- running time: \(O(n\log n)\)
fromPolygons           :: (Ord r, Fractional r)
                       => proxy s
                       -> NonEmpty (SimplePolygon p r :+ f)
                       -> f -- ^ data outside the polygons
                       -> PlanarSubdivision s (Maybe p) () f r
fromPolygons px pgs oD = undefined
  -- subd&planeGraph.faceData .~ faceData'
  --                            &planeGraph.vertexData.traverse %~ getP
  -- where
  --   faceData' = fmap (\(fi, FaceData hs _) -> FaceData hs (getFData fi)) . faces $ subd

  --   -- given a faceId lookup the
  --   getFData fi = let v = boundaryVertices fi subd V.! 0
  --                 in subd^.dataOf v.to holeData

  --   -- note that we intentionally reverse the order of iDd and oD in the call below,
  --   -- as our holes are now outside
  --   subd = fromPolygon px (MultiPolygon (CSeq.fromList [a,b,c,d]) holes') (Just oD) Nothing

  --   -- for every polygon, construct a hole.
  --   holes' = map withF . F.toList $ pgs
  --   -- add the facedata to the vertex data
  --   withF (pg :+ f) = bimap (\p -> Hole f p) id pg

  --   -- corners of the slightly enlarged boundingbox
  --   (a,b,c,d) = corners . bimap (const $ Outer oD) id
  --             . grow 1 . boundingBoxList . fmap (^.core) $ pgs

    --TODO: We need to mark the edges of the outer square as invisible.

    -- Main Idea: Assign the vertices the hole-number on which they occur. For
    -- each face we then find an incident vertex to find the data corresponding
    -- to that face.

data HoleData f p = Outer !f | Hole !f !p deriving (Show,Eq)

holeData            :: HoleData f p -> f
holeData (Outer f)  = f
holeData (Hole f _) = f

getP            :: HoleData f p -> Maybe p
getP (Outer _)  = Nothing
getP (Hole _ p) = Just p
