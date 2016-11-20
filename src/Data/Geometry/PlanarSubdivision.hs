{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.PlanarSubdivision where

import           Algorithms.Geometry.LineSegmentIntersection.BentleyOttmann
import           Control.Lens
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon hiding (vertices)
import           Data.Geometry.Properties
import qualified Data.Map as M
import           Data.PlanarGraph
import           Data.PlaneGraph
import           Data.Semigroup
import           Data.Util
import qualified Data.Vector as V

--------------------------------------------------------------------------------

-- | The Face data consists of the data itself and a list of holes
data FaceData h f = FaceData { _fData :: f
                             , _holes :: [h]
                             } deriving (Show,Eq,Ord)
makeLenses ''FaceData


newtype PlanarSubdivision s v e f r = PlanarSubdivision { _graph ::
    PlaneGraph s Primal_ v e (FaceData (PlanarSubdivision s v e f r) f) r }
  deriving (Show,Eq)
  -- each hole itself stores a planar subdivision
makeLenses ''PlanarSubdivision

type instance NumType (PlanarSubdivision s v e f r) = r

--------------------------------------------------------------------------------

-- | Construct a planar subdivision from a polygon
--
-- running time: $O(n)$.
fromPolygon                            :: proxy s
                                       -> Polygon t p r
                                       -> f -- ^ data inside
                                       -> f -- ^ data outside the polygon
                                       -> PlanarSubdivision s p () f r
fromPolygon p (SimplePolygon vs) iD oD = PlanarSubdivision $ g&faceData .~ fData'
  where
    g      = fromVertices p vs
    fData' = V.fromList [FaceData iD [], FaceData oD []]
fromPolygon p (MultiPolygon vs hs) iD oD = PlanarSubdivision $ g&faceData .~ fData'
  where
    g      = fromVertices p vs
    fData' = V.fromList [FaceData iD hs', FaceData oD []]
    hs'    = map (\h -> fromPolygon p h oD iD) hs


fromVertices      :: proxy s
                  -> C.CSeq (Point 2 r :+ p)
                  -> PlaneGraph s Primal_ p () () r
fromVertices _ vs = g&vertexData .~ vData'
  where
    n = length vs
    g = planarGraph [ [ (Dart (Arc i)               Positive, ())
                      , (Dart (Arc $ (i+1) `mod` n) Negative, ())
                      ]
                    | i <- [0..(n-1)]]
    vData' = V.fromList . F.toList $ vs


--------------------------------------------------------------------------------


data W = W

test = _graph $ fromPolygon (Identity W) simplePoly True False

simplePoly :: Polygon 'Simple () Integer
simplePoly = SimplePolygon . C.fromList . map ext $ [ point2 0 0
                                                    , point2 10 0
                                                    , point2 10 10
                                                    , point2 5 15
                                                    , point2 1 11
                                                    ]


--------------------------------------------------------------------------------



fromSegments      :: (Foldable f, Ord r, Num r)
                  => proxy s
                  -> f (LineSegment 2 p r :+ e)
                  -> PlaneGraph s Primal_ [p] e () r
fromSegments _ ss = (planarGraph dts)&vertexData .~ vxData
  where
    pts         = M.fromListWith (<>) . concatMap f . zipWith g [0..] . F.toList $ ss
    f (s :+ e)  = [ ( s^.start.core
                    , SP [s^.start.extra] [(s^.end.core)   :+ h Positive e])
                  , ( s^.end.core
                    , SP [s^.end.extra]   [(s^.start.core) :+ h Negative e])
                  ]
    g i (s :+ e) = s :+ (Arc i :+ e)
    h d (a :+ e) = (Dart a d, e)

    vts    = map (\(p,sp) -> (p,map (^.extra) . sortArround (ext p) <$> sp))
           . M.assocs $ pts
    -- vertex Data
    vxData = V.fromList . map (\(p,sp) -> p :+ sp^._1) $ vts
    -- The darts
    dts    = map (^._2._2) vts

    -- TOOD: assign id's
