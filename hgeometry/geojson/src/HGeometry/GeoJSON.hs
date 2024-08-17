{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.GeoJSON
  ( GeoPositionWithoutCRS'
  , RestGeoPosition(..)
  , _GeoPositionWithoutCRS
  ) where

import           Control.Lens
import           Data.Bifunctor
import           Data.Coerce
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Functor.Apply (Apply, (<.*>), MaybeApply(..))
import           Data.Geospatial
import           Data.LinearRing
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe (fromMaybe)
import           Data.Semigroup.Traversable
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           HGeometry.Cyclic
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.Point.Class
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           HGeometry.Properties
import           HGeometry.Sequence.NonEmpty
import           HGeometry.Vector

--------------------------------------------------------------------------------
-- * PointXY

type instance NumType   PointXY = Double
type instance Dimension PointXY = 2

instance HasVector PointXY PointXY where
  vector = lens (\(PointXY x y) -> Vector2 x y) (\_ (Vector2 x y) -> PointXY x y)

instance HasCoordinates PointXY PointXY
instance Affine_ PointXY 2 Double
instance Point_  PointXY 2 Double

--------------------------------------------------------------------------------
-- * PointXYZ

type instance NumType   PointXYZ = Double
type instance Dimension PointXYZ = 3

instance HasVector PointXYZ PointXYZ where
  vector = lens (\(PointXYZ x y z) -> Vector3 x y z) (\_ (Vector3 x y z) -> PointXYZ x y z)

instance HasCoordinates PointXYZ PointXYZ
instance Affine_ PointXYZ 3 Double
instance Point_  PointXYZ 3 Double

--------------------------------------------------------------------------------
-- * PointXYZM

type instance NumType   PointXYZM = Double
type instance Dimension PointXYZM = 4

instance HasVector PointXYZM PointXYZM where
  vector = lens (\(PointXYZM x y z m) -> Vector4 x y z m)
                (\_ (Vector4 x y z m) -> PointXYZM x y z m)

instance HasCoordinates PointXYZM PointXYZM
instance Affine_ PointXYZM 4 Double
instance Point_  PointXYZM 4 Double

--------------------------------------------------------------------------------
-- * GeoPositionWithoutCRS

type GeoPositionWithoutCRS' = PointXY :+ Maybe RestGeoPosition

type instance NumType GeoPositionWithoutCRS = Double

data RestGeoPosition = Z  {-#UNPACK#-}!Double
                     | ZM {-#UNPACK#-}!Double {-#UNPACK#-}!Double
                     deriving (Show,Eq,Ord)

-- | Convert between a GeoPositionWithoutCRS and a PointXY
_GeoPositionWithoutCRS :: Prism' GeoPositionWithoutCRS (PointXY :+ Maybe RestGeoPosition)
_GeoPositionWithoutCRS = prism' toGeoP fromGeoP
  where
    toGeoP (p@(PointXY x y) :+ mr) = case mr of
      Nothing       -> GeoPointXY p
      Just (Z z)    -> GeoPointXYZ $ PointXYZ x y z
      Just (ZM z m) -> GeoPointXYZM $ PointXYZM x y z m
    fromGeoP = \case
      GeoEmpty                         -> Nothing
      GeoPointXY p                     -> Just $ p             :+ Nothing
      GeoPointXYZ (PointXYZ x y z)     -> Just $ (PointXY x y) :+ Just (Z z)
      GeoPointXYZM (PointXYZM x y z m) -> Just $ (PointXY x y) :+ Just (ZM z m)


-- | An unsafe version, pretending  that GeoEmpty is not possible
_GeoPositionWithoutCRS' :: Iso' GeoPositionWithoutCRS (PointXY :+ Maybe RestGeoPosition)
_GeoPositionWithoutCRS' = iso (^?!_GeoPositionWithoutCRS)
                              (^?!re _GeoPositionWithoutCRS)
  where
    err = error "_GeoPositionWithoutCRS'"

--------------------------------------------------------------------------------
-- * Polygon

type instance NumType   GeoPolygon = Double

type instance Dimension GeoPolygon = 2

instance Wrapped GeoPolygon
instance Rewrapped GeoPolygon GeoPolygon

----------------------------------------

type SimpleGeoPolygon point = SimplePolygonF (Cyclic ViewL1) point

type SimpleGeoPolygon' = SimpleGeoPolygon GeoPositionWithoutCRS'

-- | pre: the sequence has at leat 3 elements
_RingViewL1 :: (Eq b, Show b)
            => Iso (LinearRing a) (LinearRing b) (Cyclic ViewL1 a) (Cyclic ViewL1 b)
_RingViewL1 = iso (Cyclic . ringToViewL1)
               (fromMaybe err . viewL1ToRing . coerce)
  where
    err = error "_RingSeq: failed"

-- | Transform a ViewL1 into a LinearRing
ringToViewL1   :: LinearRing a -> ViewL1 a
ringToViewL1 r = case toSeq r of
  (x :<| (s :|> _)) -> x :<< s
    -- the last element is duplicated, we have at least three so this is s afe.
  _                 -> error "ringToViewL1"

-- | try to transform a ViewL1 into a LinearRing
viewL1ToRing           :: (Eq b, Show b) => ViewL1 b -> Maybe (LinearRing b)
viewL1ToRing (x :<< s) = either (const Nothing) Just . fromSeq $ x <| (s |> x)

--------------------------------------------------------------------------------

-- | Access the underlying container
_UncheckedSimplePolygon :: Iso (SimplePolygonF (Cyclic ViewL1) point)
                               (SimplePolygonF (Cyclic ViewL1) point')
                               (Cyclic ViewL1 point)
                               (Cyclic ViewL1 point')
_UncheckedSimplePolygon = coerced

-- | Convert a ring to a simple polygon
_UncheckedRingSimplePolygon' :: Iso' (LinearRing GeoPositionWithoutCRS)
                                    (SimplePolygonF (Cyclic ViewL1) GeoPositionWithoutCRS)
_UncheckedRingSimplePolygon' = _RingViewL1
                             . from _UncheckedSimplePolygon

-- | Treat a LinearRing as a simple polygon
_UncheckedRingSimplePolygon :: Iso' (LinearRing GeoPositionWithoutCRS)
                                    (SimplePolygonF (Cyclic ViewL1) GeoPositionWithoutCRS')
_UncheckedRingSimplePolygon  = _UncheckedRingSimplePolygon' . convert
  where
    convert :: Iso'  (SimplePolygonF (Cyclic ViewL1) GeoPositionWithoutCRS)
                     (SimplePolygonF (Cyclic ViewL1) GeoPositionWithoutCRS')
    convert = iso (over vertices (view _GeoPositionWithoutCRS'))
                  (over vertices (view $ from _GeoPositionWithoutCRS'))

-- --------------------------------------------------------------------------------

-- instance HasVertices' GeoPolygon where
--   type VertexIx GeoPolygon = (Int,Int) -- first one is the ringId
--   type Vertex   GeoPolygon = GeoPositionWithoutCRS'

--   vertexAt (i,j) = _Wrapped .> iix i <.> _RingAsSimplePolygon .> vertexAt j
--                  <. _GeoPositionWithoutCRS

--   numVertices (GeoPolygon ss) = F.foldl' (\a s -> a + ringLength s - 1) 0 ss
--   -- -- the ring incluces a copy of the first element, so it overestimates the length by 1

-- instance HasVertices GeoPolygon GeoPolygon where
--   vertices = _Wrapped .> traversed1Seq <.> _RingAsSimplePolygon .> vertices
--           <. singular _GeoPositionWithoutCRS
-- -- TODO: the internal ones should be reversed

-- withRing   :: Int -> IndexedLens' Int GeoPolygon (SimplePolygonF Seq1 GeoPositionWithoutCRS)
-- withRing i = _Wrapped .> singular (iix i) <. _RingAsSimplePolygon

-- firstRing :: IndexedLens' Int GeoPolygon (SimplePolygonF Seq1 GeoPositionWithoutCRS)
-- firstRing = withRing 0

-- -- | unsafe conversion wrapping an edge
-- edge' :: Iso' (GeoPositionWithoutCRS, GeoPositionWithoutCRS)
--               (GeoPositionWithoutCRS', GeoPositionWithoutCRS')
-- edge' = iso (\(u,v) -> (u^?!_GeoPositionWithoutCRS, v^?!_GeoPositionWithoutCRS))
--             (\(u,v) -> (u^?!re _GeoPositionWithoutCRS, v^?!re _GeoPositionWithoutCRS))

-- reIndexEdge :: Indexable ( (Int,Int) , (Int,Int)) p
--             => (Indexed (Int,(Int,Int)) a b -> r) -> p a b -> r
-- reIndexEdge = reindexed (\(i,(u,v)) -> ((i,u), (i,v)))

-- instance HasOuterBoundary GeoPolygon where
--   outerBoundaryVertexAt (i,j)
--     | i == 0    = firstRing <.> singular (vertexAt j) <. singular _GeoPositionWithoutCRS
--     | otherwise = error "outerBoundaryVertex: not on first ring"
--   -- the first ring is outer boundary apparently

--   ccwOuterBoundaryFrom (i,j)
--     | i == 0    = firstRing <.> ccwOuterBoundaryFrom j <. singular _GeoPositionWithoutCRS
--     | otherwise = error "ccwOuterBoundaryFrom: not on first ring"

--   outerBoundary = firstRing <.> vertices <. singular _GeoPositionWithoutCRS

--   cwOuterBoundaryFrom (i,j)
--     | i == 0    = firstRing <.> cwOuterBoundaryFrom j <. singular _GeoPositionWithoutCRS
--     | otherwise = error "cwOuterBoundaryFrom: not on first ring"

--   outerBoundaryEdges = reIndexEdge
--                      $ firstRing <.> outerBoundaryEdges <. edge'


--   outerBoundaryEdgeAt (_,j) = reIndexEdge
--                             $ firstRing <.> outerBoundaryEdgeAt j <. edge'

-- -- instance Polygon_ GeoPolygon GeoPositionWithoutCRS' Double where
-- --   area pg = case toNonEmptyOf (_Wrapped.from _Seq1Seq.traverse1._RingAsSimplePolygon') pg of
-- --     outer :| inners -> area outer - sum (map area inners)

--   -- ccwPredecessorOf (i,j) = withRing i <.> ccwPredecessorOf j <. singular _GeoPositionWithoutCRS
--   -- ccwSuccessorOf   (i,j) = withRing i <.> ccwSuccessorOf   j <. singular _GeoPositionWithoutCRS

-- {-

-- instance SimplePolygon_ GeoPolygon GeoPositionWithoutCRS' Double where
--   uncheckedFromCCWPoints = undefined
--   fromPoints = undefined


-- -- featureCollection ::

-- -- just make instances

-- -}

-- -- parseGeoJSONFile      :: OsPath -> IO ()
-- -- parseGeoJSONFile path =
