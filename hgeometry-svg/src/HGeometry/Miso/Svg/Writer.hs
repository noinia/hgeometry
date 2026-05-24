{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE AllowAmbiguousTypes          #-}
{-# LANGUAGE QuantifiedConstraints          #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Miso.Svg.Writer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Render geometric objects to Svg files through miso
--
--------------------------------------------------------------------------------
module HGeometry.Miso.Svg.Writer
  ( withAts
  , Drawable(..)

  , dPoint
  , dLineSegment
  , dRectangle
  , dCircle
  , dDisk
  , dPolyLine
  , dSimplePolygon

  , SvgWriteAttributes(..)
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import           HGeometry.Ball
import           HGeometry.Box
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.LineSegment
import           HGeometry.Miso.OrphanInstances ()
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple
import           HGeometry.Vector
import qualified Ipe
import           Ipe.Attributes
import           Miso (Attribute, View, text)
import           Miso.String (MisoString, ToMisoString(..), ms)
import qualified Miso.String.Util as MisoString
import           Miso.Svg
import           Miso.Svg.Property
import           Miso.Html.Property (width_,height_) -- not sure if this is correct (namespace)!
import           Barbies

--------------------------------------------------------------------------------


-- | Helper function to construct drawing functions. I..e it allows
-- you do pre-specify a bunch of attributes that should be drawn
-- (ats1) yet allow more attributes to be added by the user later.
withAts             ::  ([Attribute action] -> View model action)
                    -> [Attribute action] -> [Attribute action] -> View model action
withAts f ats1 ats2 = f (ats1 <> ats2)

-- -- | Helper function to construct a View. See 'withAts' for its usage.
-- withAts'             :: ([Attribute action] -> View model action)
--                      -> [Attribute action]
--                      -> [Attribute action]
--                      -> View model action
-- withAts' f ats1 ats2 = f (ats1 <> ats2)

--------------------------------------------------------------------------------
-- * Default implementations for drawing geometric objects

-- | Default implementation for drawing geometric objects
class Drawable t where
  {-# MINIMAL draw | drawWith #-}
  -- | Draws the given object with the given attributes
  draw       :: t -> [Attribute action] -> View model action
  draw x ats = drawWith x ats []

  -- | draw the given object, as well as the given "children"
  drawWith          :: t -> [Attribute action] -> [View model action] -> View model action
  drawWith x ats _b = draw x ats

instance (Drawable l, Drawable r) => Drawable (Either l r) where
  draw = either draw draw

instance ToMisoString r => Drawable (Point 2 r) where
  draw = dPoint

instance (Point_ point 2 r, ToMisoString r, Num r) => Drawable (Rectangle point) where
  draw = dRectangle

instance ( Point_ point 2 r, EndPoint_ (endPoint point), IxValue (endPoint point) ~ point
         , ToMisoString r) => Drawable (LineSegment endPoint point) where
  draw = dLineSegment

instance ( Point_ point 2 r, ToMisoString r
         , Traversable1 f
         , Ixed (f point), IxValue (f point) ~ point, Index (f point) ~ Int
         , HasFromFoldable1 f
         , TraversableWithIndex Int f
         ) => Drawable (PolyLineF f point) where
  draw = dPolyLine

instance ( Point_ point 2 r, VertexContainer f point, HasFromFoldable1 f
         , ToMisoString r) => Drawable (SimplePolygonF f point) where
 draw = dSimplePolygon

instance ( Point_ point 2 r, VertexContainer f point, HasFromFoldable1 f
         , ToMisoString r) => Drawable (ConvexPolygonF f point) where
  draw = dSimplePolygon . toSimplePolygon

instance (Point_ point 2 r, ToMisoString r, Floating r) => Drawable (Circle point) where
  draw = dCircle

instance (Point_ point 2 r, ToMisoString r, Floating r) => Drawable (Disk point) where
  draw = dDisk

-- instance ToMisoString r => Drawable (Viewport r) where
--   draw = error
--   drawWith vp ats content = withAts' svg_ [ height_ $ ms h <> "px"
--                                           , viewbox_ outerVB
--                                           ] ats
--                                           [ g_ [] -- do the transorm here
--                                                [ svg_ [ width_ "100%"
--                                                       , height "100%"
--                                                       , viewbox innerVB
--                                                       ]
--                                                       content
--                                                ]
--                                           ]
--     where
--       toVB = MisoString.unwords . map ms
--       outerVB = toVB [0, (-1) * h, w, h]
--             -- the role of the outer viewBox is to flip the coordinate
--             -- system s.t. the origin is in the bottom left rather
--             -- than the top-left
--       innerVB = toVB [lx, ly, vw, vh]




--------------------------------------------------------------------------------
-- * Functions to draw geometric objects

-- | Draw a point
dPoint   :: (Point_ point 2 r, ToMisoString r) => point -> [Attribute action] -> View model action
dPoint p = withAts ellipse_ [ cx_ (ms $ p^.xCoord), cy_ (ms $ p^.yCoord)
                            , rx_ "5", ry_ "5"
                            ]

-- | Draw a rectangle
dRectangle   :: ( Rectangle_ rectangle point, Point_ point 2 r, ToMisoString r, Num r)
             => rectangle -> [Attribute action] -> View model action
dRectangle b = let Point2 x y  = over coordinates ms $ b^.minPoint.asPoint
                   Vector2 w h = ms <$> b^.to size
               in withAts rect_ [ x_ x, y_ y, width_ w, height_ h, fill_ "none"
                                 , strokeLinejoin_ "round"
                                ]

-- | Draw a simple polygon
dSimplePolygon    :: (SimplePolygon_ simplePolygon point r, ToMisoString r)
                  => simplePolygon -> [Attribute action] -> View model action
dSimplePolygon pg = withAts polygon_ [ points_ $ toPointsString $ pg^..vertices
                                     , strokeLinejoin_ "round"
                                     ]


  -- \case
  --   SimplePolygon vs   ->
  --   MultiPolygon vs hs -> withAts path_ [d_ s]
  --     where
  --       s = mconcat . map toSimplePolygonPathString $ vs : hs


-- toSimplePolygonPathString                    :: ToMisoString r => SimplePolygon p r -> MisoString
-- toSimplePolygonPathString (SimplePolygon vs) = mconcat [ "M", toOp p
--                                                        , mconcat $ map (\q -> "L" <> toOp q) ps
--                                                        , "Z"
--                                                        ]
  -- where
  --   p :| ps = F1.toNonEmpty vs
  --   toOp (Point2 x y :+ _) = ms x <> " " <> ms y <> " "


-- | Draw a polyline
dPolyLine    :: (PolyLine_ polyLine point, Point_ point 2 r, ToMisoString r)
             => polyLine -> [Attribute action] -> View model action
dPolyLine pl = withAts polyline_ [ points_ . toPointsString $ pl^..vertices
                                 , fill_ "none"
                                 , strokeLinejoin_ "round"
                                 ]

-- | Draw a line segment
dLineSegment   :: ( LineSegment_ lineSegment point, Point_ point 2 r, ToMisoString r)
               => lineSegment -> [Attribute action] -> View model action
dLineSegment s = withAts polyline_ [ points_ $ toPointsString [s^.start, s^.end] ]

-- | constructs a list of points to be used in the 'points' svg attribute.
toPointsString :: (Point_ point 2 r, ToMisoString r, Foldable f) => f point -> MisoString
toPointsString =
  MisoString.unwords . map (\(Point2_ x y) -> mconcat [ms x, ",", ms y]) . F.toList


-- | Draw a circle
dCircle              :: (Point_ point 2 r, ToMisoString r)
                     => Circle point -> [Attribute action] -> View model action
dCircle (Circle c r) = withAts ellipse_ [ rx_ . ms $ r
                                         , ry_ . ms $ r
                                         , cx_ . ms $ c^.xCoord
                                         , cy_ . ms $ c^.yCoord
                                         , fill_ "none"
                                         ]

-- | Draw a disk
dDisk             :: ( Disk_ disk point, ConstructableBall_ disk point
                     , Point_ point 2 r, ToMisoString r, Floating r)
                  => disk -> [Attribute action] -> View model action
dDisk (Disk_ c r) = dCircle (Circle c r)

-- instance (ToMisoString r, Drawable v, Drawable  => Drawable (PlanarSubdivision s v e f r)


-- dPlanarSubdivision        :: PlanarSubdivision s (Maybe (View model action))
--                                                  (Maybe (View model action))
--                                                  (Maybe (View model action)) r
--                           -> [Attribute action] -> View model action
-- dPlanarSubdivision = dPlanarSubdivisionWith (^._2.vData) (^._2.extra) (^._2.extra)


-- -- | Draws only the values for which we have a Just attribute
-- dPlanarSubdivision' :: (ToMisoString r)
--                     => PlanarSubdivision s (Maybe (Ipe.IpeAttributes Ipe.IpeSymbol r))
--                                            (Maybe (Ipe.IpeAttributes Ipe.Path r))
--                                            (Maybe (Ipe.IpeAttributes Ipe.Path r)) r
--                     -> [Attribute action]
--                     -> View model action
-- dPlanarSubdivision' = dPlanarSubdivisionWith fv fe ff
--   where
--     fv (_,v) = (\ats -> draw (v^.location) (svgWriteAttrs ats)) <$> v^.vData
--     fe (_,e) = (\ats -> draw (e^.core)     (svgWriteAttrs ats)) <$> e^.extra
--     ff (_,f) = (\ats -> draw (f^.core)     (svgWriteAttrs ats)) <$> f^.extra


-- type DrawF a action = a -> Maybe (View model action)

-- dPlanarSubdivisionWith                 :: DrawF (VertexId' s, VertexData r v)          action
--                                        -> DrawF (Dart s,      LineSegment 2 v r :+ e)  action
--                                        -> DrawF (FaceId' s,   SomePolygon v r :+ f)    action
--                                        -> PlanarSubdivision s v e f r
--                                        -> [Attribute action]
--                                        -> View model action
-- dPlanarSubdivisionWith fv fe ff ps ats = g_ ats (fs <> es <> vs)
--     -- draw faces at the bottom, then edges, and finally the vertices
--   where
--     vs = mapMaybe fv . F.toList . vertices        $ ps
--     es = mapMaybe fe . F.toList . edgeSegments    $ ps
--     fs = mapMaybe ff . F.toList . rawFacePolygons $ ps

--------------------------------------------------------------------------------
-- * Functions to draw ipe objects

instance ToMisoString r => Drawable (Ipe.IpeObject r) where
  draw = \case
    Ipe.IpeGroup g     -> draw g
    Ipe.IpeImage i     -> draw i
    Ipe.IpeTextLabel t -> draw t
    Ipe.IpeMiniPage m  -> draw m
    Ipe.IpeUse u       -> draw u
    Ipe.IpePath p      -> draw p

instance ( Drawable g
         , ToMisoString r
         , forall action. SvgWriteAttributes (ats r) action
         ) => Drawable (g :+ ats r Maybe) where
  draw (i :+ iAts) ats = draw i (svgWriteAttrs @(ats r) iAts <> ats)




instance ToMisoString r => Drawable (Ipe.Group r) where
  draw (Ipe.Group os) ats = g_ ats (map (flip draw []) os)

instance ToMisoString r => Drawable (Ipe.Image r) where
  draw _ ats = text_ ats [text "image"]
instance ToMisoString r => Drawable (Ipe.TextLabel r) where
  draw (Ipe.Label t p) ats = text_ ([ transform_ $ moveTo p ] <> ats) [text $ ms t ]
instance ToMisoString r => Drawable (Ipe.MiniPage r) where
  draw (Ipe.MiniPage t p w) ats = text_ ([ transform_ $ moveTo p
                                         , width_     $ ms w
                                         ] <> ats)
                                        [text $ ms t]

-- | renders a translation matrix
moveTo :: ToMisoString r => Point 2 r -> MisoString
moveTo (over coordinates ms -> Point2 x y) = "translate(" <> x <> " " <> y <> ")"

instance ToMisoString r => Drawable (Ipe.IpeSymbol r) where
  draw (Ipe.Symbol p _ ) = withAts ellipse_ [ rx_ r
                                            , ry_ r
                                            , cx_ . ms $ p^.xCoord
                                            , cy_ . ms $ p^.yCoord
                                            ]
    where
      r = "5"

instance ToMisoString r => Drawable (Ipe.Path r) where
  draw (Ipe.Path s) ats = g_ [] (map (flip draw (fill_ "none":ats)) . F.toList $ s)
  -- svg fills paths by default; don't do that unless specified otherwise

instance ToMisoString r => Drawable (Ipe.PathSegment r) where
  draw = \case
    Ipe.PolyLineSegment pl -> dPolyLine pl
    Ipe.PolygonPath _ pg   -> dSimplePolygon pg -- TODO: maybe don't ignore the orientation
    _                      -> error "toValue: not implemented yet"




--------------------------------------------------------------------------------
-- * Dealing with attributes



--------------------------------------------------------------------------------


newtype SvgF action val = SvgF (val -> [Attribute action])



  -- MisoString -> Attribute action

class SvgWriteAttributes ats action where
  svgAttrFunctions :: ats (SvgF action)
  -- | Write the attributes to file
  svgWriteAttrs :: ats Maybe -> [Attribute action]
  default svgWriteAttrs :: (ApplicativeB ats, TraversableB ats) => ats Maybe -> [Attribute action]
  svgWriteAttrs = bfoldMap getConst
                . bzipWith writeAttr svgAttrFunctions

singleton :: a -> [a]
singleton = (:[])

instance SvgWriteAttributes (CommonAttributes r) action where
  svgAttrFunctions = bpure (SvgF $ const [])
  svgWriteAttrs = bfoldMap getConst
                . bzipWith writeAttr svgAttrFunctions

writeAttr :: forall action. (forall a. SvgF action a -> Maybe a -> Const [Attribute action] a)
writeAttr (SvgF attr) m = Const $ maybe [] attr m


instance ToMisoString r => SvgWriteAttributes (SymbolAttributesF r) action where
  svgAttrFunctions = SymbolAttributes
    { _commonAttrs = svgAttrFunctions
    , _stroke      = SvgF (singleton . stroke_ . ms)
    , _fill        = SvgF (singleton . fill_   . ms)
    , _pen         = SvgF (const []           )
    , _symbolSize  = SvgF (const []           )
    }
  svgWriteAttrs = bfoldMap getConst
                . bzipWith writeAttr svgAttrFunctions

instance ToMisoString r => SvgWriteAttributes (PathAttributesF r) action where
  svgAttrFunctions = PathAttributes
    { _commonAttrs = svgAttrFunctions
    , _stroke        = SvgF (singleton . stroke_ . ms)
    , _fill          = SvgF (singleton . fill_ . ms)
    , _pen           = SvgF (singleton . strokeWidth_ . ms)
    , _dash          = SvgF (const []                           )
    , _lineCap       = SvgF (singleton . strokeLinecap_ . ms)
    , _lineJoin      = SvgF (singleton . strokeLinejoin_ . ms   )
    , _fillRule      = SvgF (const []                           )
    , _arrow         = SvgF (singleton . markerEnd_ . ms )
    , _rArrow        = SvgF (singleton . markerStart_ . ms)
    , _strokeOpacity = SvgF (singleton . strokeOpacity_ . ms)
    , _opacity       = SvgF (singleton . fillOpacity_ . ms)
    , _tiling        = SvgF (const []                           )
    , _gradient      = SvgF (const []                           )
    }

instance SvgWriteAttributes (GroupAttributesF r) action where
  svgAttrFunctions = GroupAttributes
    { _commonAttrs = svgAttrFunctions
    , _clip        = SvgF (const [])
    }

instance ToMisoString r => SvgWriteAttributes (TextAttributesF r) action where
  svgAttrFunctions = TextAttributes
    { _commonAttrs = svgAttrFunctions
    , _stroke      = SvgF (singleton . stroke_ . ms)
    , _textSize    = SvgF (const [])
    , _opacity     = SvgF (singleton . strokeOpacity_ . ms)
    , _textWidth   = SvgF (const [])
    , _textHeight  = SvgF (const [])
    , _depth       = SvgF (const [])
    , _hAlign      = SvgF (const [])
    , _vAlign      = SvgF (const [])
    , _style       = SvgF (const [])
    }
