{-# OPTIONS_GHC -Wno-orphans #-}
module HGeometry.GeoJSON
  ( GeoPositionWithoutCRS'
  , RestGeoPosition(..)
  , _GeoPositionWithoutCRS
  ) where

import           Control.Lens
import           Data.Bifunctor
import           Data.Coerce
import           Data.Foldable1
import           Data.Functor.Apply (Apply, (<.*>), MaybeApply(..))
import           Data.Geospatial
import           Data.LinearRing
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


--------------------------------------------------------------------------------
-- * Polygon

type instance NumType   GeoPolygon = Double

type instance Dimension GeoPolygon = 2


instance Wrapped GeoPolygon
instance Rewrapped GeoPolygon GeoPolygon

--------------------------------------------------------------------------------

newtype Seq1 a = Seq1 (Seq a)
  deriving newtype (Show,Eq,Ord,Functor,Foldable)
  deriving Generic

_Seq1Seq :: Iso (Seq1 a) (Seq1 b) (Seq a) (Seq b)
_Seq1Seq = coerced

instance Traversable Seq1 where
  traverse f (Seq1 s) = Seq1 <$> traverse f s

type instance Index   (Seq1 a) = Int -- Index   (Seq a)
type instance IxValue (Seq1 a) = a   -- IxValue (Seq a)

instance Ixed (Seq1 a) where
  ix i = _Seq1Seq . ix i

instance Wrapped   (Seq1 a) -- where
  -- type Unwrapped (Seq1 a) = Seq a
  -- _Wrapped' = _Seq1Seq

instance Rewrapped (Seq1 a) (Seq1 b)

instance FunctorWithIndex Int Seq1 where
  imap f (Seq1 s) = Seq1 $ imap f s

instance FoldableWithIndex Int Seq1 where
  ifoldMap f (Seq1 s) = ifoldMap f s

instance TraversableWithIndex Int Seq1 where
  itraverse f (Seq1 s) = Seq1 <$> itraverse f s

instance Foldable1 Seq1 where
  foldMap1 = foldMap1Default
instance Traversable1 Seq1 where
  traverse1 f (Seq1 s) = Seq1 <$> traverse1Seq f s

instance HasFromFoldable1 Seq1 where
  fromNonEmpty = Seq1 . fromNonEmpty

-- | Traverse a non-empty sequence
traverse1Seq   :: Apply f => (a -> f b) -> Seq a -> f (Seq b)
traverse1Seq f = \case
  Seq.Empty -> error "traverse1Seq: precondition violated"
  (x :<| s) -> (:<|) <$> f x <.*> traverse1Maybe f s

-- | Indexed traversal of a non-empty Sequence
traversed1Seq :: IndexedTraversal1 Int (Seq a) (Seq b) a b
traversed1Seq = conjoined traverse1Seq (indexing traverse1Seq)







-- type instance Index   (LinearRing a) = Int
-- type instance IxValue (LinearRing a) = a

--------------------------------------------------------------------------------

-- | pre: the sequence has at leat 3 elements
_RingSeq :: (Eq b, Show b) => Iso (LinearRing a) (LinearRing b) (Seq a) (Seq b)
_RingSeq = iso ringToSeq (fromMaybe err . seqToRing)
  where
    err = error "_RingSeq: failed"

ringToSeq   :: LinearRing a -> Seq a
ringToSeq r = let (s :|> _) = toSeq r in s

seqToRing             :: (Eq b, Show b) => Seq b -> Maybe (LinearRing b)
seqToRing s@(x :<| _) = either (const Nothing) Just . fromSeq $ s |> x

-- | pre: the sequence has at least three elements
_RingSeq1 :: (Eq b, Show b) => Iso (LinearRing a) (LinearRing b) (Seq1 a) (Seq1 b)
_RingSeq1 = _RingSeq . from _Seq1Seq


-- traverse1Ring :: Apply f => (a -> f b) -> LinearRing a -> f (LinearRing b)
-- traverse1Ring f

-- | convert a ring into a polygon
--
-- pre: the ring should be in CCW order.
--
-- O(1)
uncheckedRingAsPolygon :: LinearRing GeoPositionWithoutCRS
                       -> SimplePolygonF Seq GeoPositionWithoutCRS
uncheckedRingAsPolygon = MkSimplePolygon . ringToSeq

-- uncheckedRingAsPolygon' :: LinearRing GeoPositionWithoutCRS
--                         -> SimplePolygonF Seq GeoPositionWithoutCRS'
-- uncheckedRingAsPolygon' = uncheckedRingAsPolygon

--   MkSimplePolygon . ringToSeq

_RingAsSimplePolygon :: Iso' (LinearRing GeoPositionWithoutCRS)
                             (SimplePolygonF Seq1 GeoPositionWithoutCRS)
_RingAsSimplePolygon = _RingSeq1 . from _SimplePolygonContainer

_SimplePolygonContainer :: Iso (SimplePolygonF Seq1 point) (SimplePolygonF Seq1 point')
                               (Seq1 point) (Seq1 point')
_SimplePolygonContainer = coerced


-- instance HasDirectedTraversals Seq where

instance HasDirectedTraversals Seq1 where
  traverseRightFrom i = conjoined traverse' (itraverse' . indexed)
    where
      combine sb' sa' = Seq1 (sa' <> sb')

      traverse'             :: Apply f => (a -> f b) -> Seq1 a -> f (Seq1 b)
      traverse' f (Seq1 s)  = let (sa,sb) = Seq.splitAt i s
                              in combine <$>  traverse1Seq   f sb
                                         <.*> traverse1Maybe f sa
      itraverse'            :: Apply f => (Int -> a -> f b) -> Seq1 a -> f (Seq1 b)
      itraverse' f (Seq1 s) = let (sa,sb) = Seq.splitAt i s
                              in combine  <$>  itraverse1Seq  (\j x -> f (i+j) x) sb
                                          <.*> itraverseMaybe f                   sa

  traverseLeftFrom  i = conjoined traverse' (itraverse' . indexed)
    where
      combine sb' sa' = Seq1 (Seq.reverse sa' <> Seq.reverse sb')

      traverse'             :: Apply f => (a -> f b) -> Seq1 a -> f (Seq1 b)
      traverse' f (Seq1 s)  = let (sa,sb) = Seq.splitAt i s
                              in combine <$>  traverse1Seq   f (Seq.reverse sa)
                                         <.*> traverse1Maybe f (Seq.reverse sb)
      itraverse'            :: Apply f => (Int -> a -> f b) -> Seq1 a -> f (Seq1 b)
      itraverse' f (Seq1 s) = let (sa,sb) = Seq.splitAt (i+1) s
                              in combine <$>  itraverse1Seq  (\j x -> f (i+j) x) (Seq.reverse sa)
                                         <.*> itraverseMaybe f                   (Seq.reverse sb)



itraverse1Seq :: Apply f => (Int -> a -> f b) -> Seq a -> f (Seq b)
itraverse1Seq = itraverseOf traversed1Seq

itraverseMaybe   :: (Apply f, TraversableWithIndex i t)
                 => (i -> a -> f b) -> t a -> MaybeApply f (t b)
itraverseMaybe f = itraverse (\i -> MaybeApply . Left . f i)


--------------------------------------------------------------------------------

instance HasVertices' GeoPolygon where
  type VertexIx GeoPolygon = (Int,Int) -- first one is the ringId
  type Vertex   GeoPolygon = GeoPositionWithoutCRS'

  vertexAt (i,j) = _Wrapped .> iix i <.> _RingAsSimplePolygon .> vertexAt j
                 <. _GeoPositionWithoutCRS

  numVertices (GeoPolygon ss) = foldl' (\a s -> a + ringLength s - 1) 0 ss
  -- -- the ring incluces a copy of the first element, so it overestimates the length by 1

instance HasVertices GeoPolygon GeoPolygon where
  vertices = _Wrapped .> traversed1Seq <.> _RingAsSimplePolygon .> vertices
          <. singular _GeoPositionWithoutCRS
-- TODO: the internal ones should be reversed
{-
instance HasOuterBoundary GeoPolygon where
  outerBoundaryVertexAt v@(i,_)
    | i == 0    = vertexAt v
    | otherwise = \_ pg -> pure pg
  -- the first ring is outer boundary apparently
  ccwOuterBoundaryFrom v@(i,_)
    | i == 0    = _Wrapped .> traversed1Seq <.> _RingAsSimplePolygon .> ccwOuterBoundaryFrom v
                  <. singular _GeoPositionWithoutCRS
    | otherwise = \_ pg -> pure pg
  cwOuterBoundaryFrom v@(i,_)
    | i == 0    = _Wrapped .> traversed1Seq <.> _RingAsSimplePolygon .> cwOuterBoundaryFrom v
                  <. singular _GeoPositionWithoutCRS
    | otherwise = \_ pg -> pure pg

  outerBoundaryEdges = undefined
    -- _Wrapped .> traversed1Seq <.> _RingAsSimplePolygon .> outerBoundaryEdges

  outerBoundaryEdgeAt v@(i,_)
    | i == 0    = undefined -- _Wrapped .> traversed1Seq <.> _RingAsSimplePolygon . outerBoundaryEdgeAt v
    | otherwise = \_ pg -> pure pg



instance Polygon_ GeoPolygon GeoPositionWithoutCRS' Double where
  area = _
  ccwPredecessorOf ()= _
  ccwSuccessorOf   = _

instance SimplePolygon_ GeoPolygon GeoPositionWithoutCRS' Double where
  uncheckedFromCCWPoints = undefined
  fromPoints = undefined


-- featureCollection ::

-- just make instances

-}

-- parseGeoJSONFile      :: OsPath -> IO ()
-- parseGeoJSONFile path =
