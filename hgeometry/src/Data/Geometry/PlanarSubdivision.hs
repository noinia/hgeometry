{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.PlnarSubdivision
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type to represent a PlanarSubdivision
--
--------------------------------------------------------------------------------
module Data.Geometry.PlanarSubdivision( module Data.Geometry.PlanarSubdivision.Basic
                                      , fromSimplePolygons
                                      -- , module Data.Geometry.PlanarSubdivision.Build
                                      , fromPolygon
                                      ) where

-- import           Algorithms.Geometry.PolygonTriangulation.Triangulate
import           Data.Ext
import           Data.Semigroup.Foldable
import           Data.Geometry.PlanarSubdivision.Basic
import           Data.Geometry.PlanarSubdivision.Merge
import           Data.BinaryTree(asBalancedBinLeafTree, foldUp, Elem(..))
import           Data.Geometry.Polygon
import qualified Data.PlaneGraph as PG
import           Data.Proxy


import Data.Geometry.Point
import qualified Data.List.NonEmpty as NonEmpty

--------------------------------------------------------------------------------

-- Constructs a planar subdivision from a collection of \(k\) disjoint
-- simple polygons of total complexity \(O(n)\).
--
-- runningtime: \(O(n\log k)\)
fromSimplePolygons       :: (Foldable1 t, Ord r, Fractional r)
                         => proxy s
                         -> f -- ^ outer face data
                         -> t (SimplePolygon p r :+ f) -- ^ the disjoint simple polygons
                         -> PlanarSubdivision s p () f r
fromSimplePolygons px oD = foldUp (\l _ r -> merge l r)
                                  (\(Elem (pg :+ iD)) -> fromSimplePolygon px pg iD oD)
                         . asBalancedBinLeafTree
                         . toNonEmpty

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

--------------------------------------------------------------------------------

data Test = Test
data Id a = Id a


simplePg  = fromSimplePolygon (Id Test) simplePg' Inside Outside
simplePg' = toCounterClockWiseOrder . fromPoints $ map ext $ [ Point2 160 736
                                                             , Point2 128 688
                                                             , Point2 176 672
                                                             , Point2 256 672
                                                             , Point2 272 608
                                                             , Point2 384 656
                                                             , Point2 336 768
                                                             , Point2 272 720
                                                             ]

triangle :: PlanarSubdivision Test () () PolygonFaceData Rational
triangle = (\pg -> fromSimplePolygon (Id Test) pg Inside Outside)
         $ trianglePG

trianglePG = fromPoints . map ext $ [origin, Point2 10 0, Point2 10 10]


mySubDiv = fromSimplePolygons (Id Test)
                              0
                              (NonEmpty.fromList [simplePg' :+ 1, trianglePG :+ 2])
