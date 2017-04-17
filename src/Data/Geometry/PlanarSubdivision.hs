{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.PlanarSubdivision where

import           Control.Lens
import qualified Data.BalBST as SS
import           Data.Bifunctor.Apply
import qualified Data.CircularSeq as C
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Interval
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.Polygon
import           Data.Geometry.Properties
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import           Data.PlanarGraph
import           Data.PlaneGraph
import           Data.Semigroup
import           Data.Util
import qualified Data.Vector as V

-- | Note that the functor instance is in v
data VertexData r v = VertexData { _location :: !(Point 2 r)
                                 , _vData    :: !v
                                 } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)
makeLenses ''VertexData

vtxDataToExt                  :: VertexData r v -> Point 2 r :+ v
vtxDataToExt (VertexData p v) = p :+ v

instance Bifunctor VertexData where
  bimap f g (VertexData p v) = VertexData (fmap f p) (g v)


-- | Planar-subdivsions are internally represented as a *connected* planar
-- graph. We distuinish two types of edges in this graph representation:
-- Visible edges, which also appear in the original planar subdivision, and
-- Invisible edges, which are essentially dummy edges making sure that the
-- entire graph is connected.
data EdgeType = Visible | Invisible deriving (Show,Read,Eq,Ord)

data EdgeData e = EdgeData { _edgeType :: !EdgeType
                           , _eData    :: !e
                           } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)
makeLenses ''EdgeData


-- | The Face data consists of the data itself and a list of holes
data FaceData h f = FaceData { _holes :: [h]
                             , _fData :: !f
                             } deriving (Show,Eq,Ord,Functor,Foldable,Traversable)
makeLenses ''FaceData


newtype PlanarSubdivision s v e f r = PlanarSubdivision { _graph ::
    PlanarGraph s Primal_ (VertexData r v) (EdgeData e) (FaceData (Dart s) f) }
      deriving (Show,Eq)
makeLenses ''PlanarSubdivision

instance Functor (PlanarSubdivision s v e f) where
  fmap f s = s&graph.vertexData.traverse.location %~ fmap f


--------------------------------------------------------------------------------

data PolygonFaceData = Inside | Outside deriving (Show,Read,Eq)

-- | Construct a planar subdivision from a polygon
--
-- running time: \(O(n)\).
fromPolygon                            :: proxy s
                                       -> SimplePolygon p r
                                       -> f -- ^ data inside
                                       -> f -- ^ data outside the polygon
                                       -> PlanarSubdivision s p () f r
fromPolygon p (SimplePolygon vs) iD oD = PlanarSubdivision g'
  where
    g      = fromVertices p vs
    fData' = V.fromList [FaceData [] iD, FaceData [] oD]

    g'     = g & faceData             .~ fData'
               & dartData.traverse._2 .~ EdgeData Visible ()
-- The following does not really work anymore
-- frompolygon p (MultiPolygon vs hs) iD oD = PlanarSubdivision g'
--   where
--     g      = fromVertices p vs
--     hs'    = map (\h -> fromPolygon p h oD iD) hs
--            -- note that oD and iD are exchanged
--     fData' = V.fromList [FaceData iD hs', FaceData oD []]


fromVertices      :: proxy s
                  -> C.CSeq (Point 2 r :+ p)
                  -> PlanarGraph s Primal_ (VertexData r p) () ()
fromVertices _ vs = g&vertexData .~ vData'
  where
    n = length vs
    g = planarGraph [ [ (Dart (Arc i)               Positive, ())
                      , (Dart (Arc $ (i+1) `mod` n) Negative, ())
                      ]
                    | i <- [0..(n-1)]]
    vData' = V.fromList . map (\(p :+ e) -> VertexData p e) . F.toList $ vs


-- | Constructs a connected planar subdivision.
--
-- pre: the segments form a single connected component
-- running time: \(O(n\log n)\)
fromConnectedSegments       :: (Foldable f, Ord r, Num r)
                            => proxy s
                            -> f (LineSegment 2 p r :+ EdgeData e)
                            -> PlanarSubdivision s (NonEmpty.NonEmpty p) e () r
fromConnectedSegments px ss = PlanarSubdivision $
    fromConnectedSegments' px ss & faceData.traverse %~ FaceData []

-- | Constructs a planar graph
--
-- pre: The segments form a single connected component
--
-- running time: \(O(n\log n)\)
fromConnectedSegments'      :: (Foldable f, Ord r, Num r)
                            => proxy s
                            -> f (LineSegment 2 p r :+ e)
                            -> PlanarGraph s Primal_
                                  (VertexData r (NonEmpty.NonEmpty p)) e ()
fromConnectedSegments' _ ss = planarGraph dts & vertexData .~ vxData
  where
    pts         = M.fromListWith (<>) . concatMap f . zipWith g [0..] . F.toList $ ss
    f (s :+ e)  = [ ( s^.start.core
                    , SP (sing $ s^.start.extra) [(s^.end.core)   :+ h Positive e])
                  , ( s^.end.core
                    , SP (sing $ s^.end.extra)   [(s^.start.core) :+ h Negative e])
                  ]
    g i (s :+ e) = s :+ (Arc i :+ e)
    h d (a :+ e) = (Dart a d, e)

    sing x = x NonEmpty.:| []

    vts    = map (\(p,sp) -> (p,map (^.extra) . sortArround (ext p) <$> sp))
           . M.assocs $ pts
    -- vertex Data
    vxData = V.fromList . map (\(p,sp) -> VertexData p (sp^._1)) $ vts
    -- The darts
    dts    = map (^._2._2) vts


--------------------------------------------------------------------------------

-- | Reports all visible segments as line segments
edgeSegments    :: PlanarSubdivision s v e f r -> [(Dart s, LineSegment 2 v r :+ e)]
edgeSegments ps = mapMaybe withSegment . F.toList . edges $ ps^.graph
  where
    withSegment (d,EdgeData et e) = let (p,q) = bimap vtxDataToExt vtxDataToExt
                                              $ ps^.graph.endPointDataOf d
                                        seg   = ClosedLineSegment p q
                                    in case et of
                                         Visible   -> Just (d, seg :+ e)
                                         Invisible -> Nothing


-- -- | Lists all faces of the planar graph. This ignores invisible edges
-- rawFacePolygons    :: PlanarSubdivision s v e f r -> [(FaceId s, SomePolygon v r :+ f)]
-- rawFacePolygons ps =
