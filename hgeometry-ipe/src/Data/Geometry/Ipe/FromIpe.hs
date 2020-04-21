{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Ipe.FromIpe
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Functions that help reading geometric values from ipe images.
--
--------------------------------------------------------------------------------
module Data.Geometry.Ipe.FromIpe(
  -- * Individual readers
    _asLineSegment
  , _asRectangle
  , _asTriangle

  , _asPolyLine
  , _asSomePolygon, _asSimplePolygon, _asMultiPolygon

  -- * Dealing with Attributes
  , _withAttrs

  -- * Default readers
  , HasDefaultFromIpe(..)

  -- * Reading all elements of a particular type
  , readAll, readAllFrom
  ) where

import           Control.Lens hiding (Simple)
import           Data.Ext
import           Data.Geometry.Ball
import           Data.Geometry.Box
import           Data.Geometry.Ellipse (Ellipse, _EllipseCircle)
import           Data.Geometry.Ipe.Path
import           Data.Geometry.Ipe.Reader
import           Data.Geometry.Ipe.Types
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import qualified Data.Geometry.PolyLine as PolyLine
import           Data.Geometry.Polygon
import           Data.Geometry.Properties
import           Data.Geometry.Triangle
import qualified Data.LSeq as LSeq
import           Data.List.NonEmpty (NonEmpty(..))

--------------------------------------------------------------------------------
-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Geometry.Ipe.Attributes
-- >>> import Data.Geometry.Ipe.Color(IpeColor(..))
-- >>> import Data.Geometry.Point
-- >>> :{
-- let testPath :: Path Int
--     testPath = Path . fromSingleton  . PolyLineSegment
--              . PolyLine.fromPoints . map ext
--              $ [ origin, Point2 10 10, Point2 200 100 ]
--     testPathAttrs :: IpeAttributes Path Int
--     testPathAttrs = attr SStroke (IpeColor "red")
--     testObject :: IpeObject Int
--     testObject = IpePath (testPath :+ testPathAttrs)
-- :}






-- | Try to convert a path into a line segment, fails if the path is not a line
-- segment or a polyline with more than two points.
--
--
_asLineSegment :: Prism' (Path r) (LineSegment 2 () r)
_asLineSegment = prism' seg2path path2seg
  where
    seg2path   = review _asPolyLine . PolyLine.fromLineSegment
    path2seg p = PolyLine.asLineSegment' =<< preview _asPolyLine p

-- | Convert to a polyline. Ignores all non-polyline parts
--
-- >>> testPath ^? _asPolyLine
-- Just (PolyLine {_points = LSeq (fromList [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [200,100] :+ ()])})
_asPolyLine :: Prism' (Path r) (PolyLine.PolyLine 2 () r)
_asPolyLine = prism' poly2path path2poly
  where
    poly2path = Path . fromSingleton  . PolyLineSegment
    path2poly = preview (pathSegments.traverse._PolyLineSegment)
    -- TODO: Check that the path actually is a polyline, rather
    -- than ignoring everything that does not fit

-- | Convert to a simple polygon
_asSimplePolygon :: Prism' (Path r) (Polygon Simple () r)
_asSimplePolygon = _asSomePolygon._Left


-- | Tries to convert a path into a rectangle.
_asRectangle :: forall r. (Num r, Ord r) => Prism' (Path r) (Rectangle () r)
_asRectangle = prism' rectToPath pathToRect
  where
    rectToPath (corners -> Corners a b c d) = review _asSimplePolygon . fromPoints $ [a,b,c,d]
    pathToRect p = p^?_asSimplePolygon >>= asRect

    asRect    :: SimplePolygon () r -> Maybe (Rectangle () r)
    asRect pg = case pg^..outerBoundary.traverse of
        [a,b,c,d] | isH a b && isV b c && isH c d && isV d a -> Just (boundingBoxList' [a,c])
        [a,b,c,d] | isV a b && isH b c && isV c d && isH d a -> Just (boundingBoxList' [a,c])
        _                                                    -> Nothing

    isH (p :+ _) (q :+ _) = p^.xCoord == q^.xCoord
    isV (p :+ _) (q :+ _) = p^.yCoord == q^.yCoord


-- | Convert to a triangle
_asTriangle :: Prism' (Path r) (Triangle 2 () r)
_asTriangle = prism' triToPath path2tri
  where
    triToPath (Triangle p q r) = polygonToPath . fromPoints . map (&extra .~ ()) $ [p,q,r]
    path2tri p = case p^..pathSegments.traverse._PolygonPath of
                    []   -> Nothing
                    [pg] -> case polygonVertices pg of
                              (a :| [b,c]) -> Just $ Triangle a b c
                              _            -> Nothing
                    _    -> Nothing


  -- an ellipse is an affine transformation of the unit disk


-- (Disk origin 1) (Vector2 1 1)

_asEllipse :: Prism' (Path r) (Ellipse r)
_asEllipse = prism' toPath toEllipse
  where
    toPath      = Path . fromSingleton  . EllipseSegment
    toEllipse p = case p^..pathSegments.traverse._EllipseSegment of
                    [e] -> Just e
                    _   -> Nothing

_asCircle :: (Floating r, Eq r) => Prism' (Path r) (Circle () r)
_asCircle = _asEllipse._EllipseCircle
-- FIXME: For reading we should not need the floating constraint!

_asDisk :: (Floating r, Eq r) => Prism' (Path r) (Disk () r)
_asDisk = _asCircle.from _DiskCircle


-- | Convert to a multipolygon
_asMultiPolygon :: Prism' (Path r) (MultiPolygon () r)
_asMultiPolygon = _asSomePolygon._Right

-- _asPolygon :: Prism' (Path r) (forall t. Polygon t () r)
-- _asPolygon = prism' polygonToPath (fmap (either id id) . pathToPolygon)

_asSomePolygon :: Prism' (Path r) (SomePolygon () r)
_asSomePolygon = prism' embed pathToPolygon
  where
    embed     = either polygonToPath polygonToPath


polygonToPath                      :: Polygon t () r -> Path r
polygonToPath pg@(SimplePolygon _) = Path . fromSingleton . PolygonPath $ pg
polygonToPath (MultiPolygon vs hs) = Path . LSeq.fromNonEmpty . fmap PolygonPath
                                   $ (SimplePolygon vs) :| hs


pathToPolygon   :: Path r -> Maybe (Either (SimplePolygon () r) (MultiPolygon () r))
pathToPolygon p = case p^..pathSegments.traverse._PolygonPath of
                    []                   -> Nothing
                    [pg]                 -> Just . Left  $ pg
                    SimplePolygon vs: hs -> Just . Right $ MultiPolygon vs hs





--------------------------------------------------------------------------------


-- | Use the first prism to select the ipe object to depicle with, and the second
-- how to select the geometry object from there on. Then we can select the geometry
-- object, directly with its attributes here.
--
-- >>> testObject ^? _withAttrs _IpePath _asPolyLine
-- Just (PolyLine {_points = LSeq (fromList [Point2 [0,0] :+ (),Point2 [10,10] :+ (),Point2 [200,100] :+ ()])} :+ Attrs {NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named "red"), NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr, NoAttr})
_withAttrs       :: Prism' (IpeObject r) (i r :+ IpeAttributes i r) -> Prism' (i r) g
                 -> Prism' (IpeObject r) (g :+ IpeAttributes i r)
_withAttrs po pg = prism' g2o o2g
  where
    g2o    = review po . over core (review pg)
    o2g o  = preview po o >>= \(i :+ ats) -> (:+ ats) <$> preview pg i





-- instance HasDefaultIpeObject Path where
--   defaultIpeObject' = _IpePath


-- class HasDefaultFromIpe g where
--   type DefaultFromIpe g :: * -> *
--   defaultIpeObject :: proxy g -> Prism' (IpeObject r) (DefaultFromIpe g r :+ IpeAttributes (DefaultFromIpe g) r)
--   defaultFromIpe   :: proxy g -> Prism' (DefaultFromIpe g (NumType g)) g


class HasDefaultFromIpe g where
  type DefaultFromIpe g :: * -> *
  defaultFromIpe :: (r ~ NumType g)
                 => Prism' (IpeObject r) (g :+ IpeAttributes (DefaultFromIpe g) r)

-- instance HasDefaultFromIpe (Point 2 r) where
--   type DefaultFromIpe (Point 2 r) = IpeSymbol
--   defaultFromIpe = _withAttrs _IpeUse symbolPoint


instance HasDefaultFromIpe (LineSegment 2 () r) where
  type DefaultFromIpe (LineSegment 2 () r) = Path
  defaultFromIpe = _withAttrs _IpePath _asLineSegment

instance HasDefaultFromIpe (Ellipse r) where
  type DefaultFromIpe (Ellipse r) = Path
  defaultFromIpe = _withAttrs _IpePath _asEllipse

instance (Floating r, Eq r) => HasDefaultFromIpe (Circle () r) where
  type DefaultFromIpe (Circle () r) = Path
  defaultFromIpe = _withAttrs _IpePath _asCircle

instance (Floating r, Eq r) => HasDefaultFromIpe (Disk () r) where
  type DefaultFromIpe (Disk () r) = Path
  defaultFromIpe = _withAttrs _IpePath _asDisk

instance HasDefaultFromIpe (PolyLine.PolyLine 2 () r) where
  type DefaultFromIpe (PolyLine.PolyLine 2 () r) = Path
  defaultFromIpe = _withAttrs _IpePath _asPolyLine


instance HasDefaultFromIpe (SimplePolygon () r) where
  type DefaultFromIpe (SimplePolygon () r) = Path
  defaultFromIpe = _withAttrs _IpePath _asSimplePolygon

instance HasDefaultFromIpe (MultiPolygon () r) where
  type DefaultFromIpe (MultiPolygon () r) = Path
  defaultFromIpe = _withAttrs _IpePath _asMultiPolygon


-- | Read all g's from some ipe page(s).
readAll :: (HasDefaultFromIpe g, r ~ NumType g, Foldable f)
        => f (IpePage r) -> [g :+ IpeAttributes (DefaultFromIpe g) r]
readAll = foldMap (^..content.traverse.defaultFromIpe)


-- | Convenience function from reading all g's from an ipe file. If there
-- is an error reading or parsing the file the error is "thrown away".
readAllFrom    :: (HasDefaultFromIpe g, r ~ NumType g, Coordinate r, Eq r)
               => FilePath -> IO [g :+ IpeAttributes (DefaultFromIpe g) r]
readAllFrom fp = readAll <$> readSinglePageFile fp

fromSingleton :: a -> LSeq.LSeq 1 a
fromSingleton = LSeq.fromNonEmpty . (:| [])
