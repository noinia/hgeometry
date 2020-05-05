{-# LANGUAGE ScopedTypeVariables #-}
module RenderCanvas where

import           Control.Lens
import           Data.Colour.SRGB(RGB(..), toSRGB24)
import           Data.Colour.Names(readColourName)
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Box
import           Data.Geometry.Ipe.Attributes
import           Data.Geometry.Ipe.Types hiding (ipeObject', width)
import qualified Data.Geometry.Ipe.Attributes as A
import           Data.Proxy
import           Data.Vinyl
import           Linear.V4 (V4(..))
import           Graphics.Rendering.Cairo.Canvas (Canvas)
import qualified Graphics.Rendering.Cairo.Canvas as Canvas
import qualified Data.Text as T


rectangle    :: (Real r, Ord r, Num r) => Rectangle p r -> Canvas ()
rectangle r' = let r                       = bimap id realToFrac r'
                   (Point2 x y :+ _,_,_,_) = corners r
               in Canvas.rect $ Canvas.D x y (width r) (height r)

polygon     :: Real r => SimplePolygon p r -> Canvas ()
polygon pg' = let pg = bimap id realToFrac pg'
              in Canvas.polygon $ pg^..outerBoundary.traverse.core.vector.to toV2

lineSegment    :: Real r => LineSegment 2 p r -> Canvas ()
lineSegment s' = let s = bimap id realToFrac s'
                 in Canvas.line (s^.start.core.vector.to toV2) (s^.end.core.vector.to toV2)

polyLine    :: Real r => PolyLine 2 p r -> Canvas ()
polyLine p' = let p = bimap id realToFrac p'
              in Canvas.shape Canvas.ShapeLines $ p^..points.traverse.core.vector.to toV2

-- | draw a point as a small disk
point   :: Real r => Point 2 r -> Canvas ()
point p = Canvas.circle' (realToFrac <$> (p^.vector.to toV2)) 5

-- | draw as a point
point'   :: Real r => Point 2 r -> Canvas ()
point' p = Canvas.point . fmap realToFrac $ p^.vector.to toV2

pathSegment                     :: Real r => PathSegment r -> Canvas ()
pathSegment (PolyLineSegment p) = polyLine p
pathSegment (PolygonPath p)     = polygon p
pathSegment _                   = error "pathSegment: Not implemented yet"


ipeUse              :: Real r => IpeSymbol r -> Canvas ()
ipeUse (Symbol p _) = Canvas.circle' (realToFrac <$> p^.vector.to toV2) 10

ipePath          :: Real r => Path r -> Canvas ()
ipePath (Path p) = mapM_ pathSegment p

ipeGroup :: RealFrac r => Group r -> Canvas ()
ipeGroup = mapM_ ipeObject . _groupItems

ipeObject'              :: forall g r. (RealFrac r, AllSatisfy ApplyAttr (AttributesOf g))
                        => (g r -> Canvas ())
                        -> g r :+ IpeAttributes g r
                        -> Canvas ()
ipeObject' f (i :+ ats) = do
                            Canvas.pushMatrix
                            applyAttributes (Proxy :: Proxy g) ats
                            f i
                            Canvas.popMatrix

ipeObject                  :: RealFrac r
                           => IpeObject r -> Canvas ()
ipeObject (IpeGroup g)     = ipeObject' ipeGroup g
ipeObject (IpeImage _)     = undefined
ipeObject (IpeTextLabel _) = undefined
ipeObject (IpeMiniPage _)  = undefined
ipeObject (IpeUse p)       = ipeObject' ipeUse  p
ipeObject (IpePath p)      = ipeObject' ipePath p


applyAttributes               :: (RealFrac r, AllSatisfy ApplyAttr (AttributesOf g))
                              => proxy g -> IpeAttributes g r -> Canvas ()
applyAttributes _ (Attrs ats) = applyAttributes' ats

applyAttributes'            :: (RealFrac r, AllSatisfy ApplyAttr rs)
                            => Rec (Attr (AttrMapSym1 r)) rs
                            -> Canvas ()
applyAttributes' RNil       = pure ()
applyAttributes' (a :& ats) = applyAttribute a >> applyAttributes' ats


newtype CanvasM = CanvasM { unCanvasM :: Canvas () }
instance Monoid CanvasM where
  mempty = CanvasM $ pure ()
  (CanvasM a) `mappend` (CanvasM b) = CanvasM $ a >> b


applyAttribute' :: (RealFrac r, ApplyAttr label)
                => Attr (AttrMapSym1 r) label -> CanvasM
applyAttribute' = CanvasM . applyAttribute


class ApplyAttr (label :: AttributeUniverse) where
  applyAttribute :: RealFrac r => Attr (AttrMapSym1 r) label -> Canvas ()


instance ApplyAttr Stroke where
  applyAttribute NoAttr   = pure ()
  applyAttribute (Attr c) = maybe (pure ()) Canvas.stroke $ toCanvasColor c

instance ApplyAttr Fill where
  applyAttribute NoAttr   = pure ()
  applyAttribute (Attr c) = maybe (pure ()) Canvas.fill $ toCanvasColor c

instance ApplyAttr Pen where
  applyAttribute NoAttr            = pure ()
  applyAttribute (Attr (IpePen p)) = case p of
      Named _  -> pure () -- TODO
      Valued v -> Canvas.strokeWeight (realToFrac v)

instance ApplyAttr Clip where
  applyAttribute _ = pure ()

instance ApplyAttr Size where
  applyAttribute _ = pure ()

instance ApplyAttr Dash where
  applyAttribute _ = pure ()


instance ApplyAttr Layer where
  applyAttribute _ = pure ()

instance ApplyAttr LineCap where
  applyAttribute _ = pure ()

instance ApplyAttr LineJoin where
  applyAttribute _ = pure ()

instance ApplyAttr A.Matrix where
  applyAttribute _ = pure ()

instance ApplyAttr Pin where
  applyAttribute _ = pure ()

instance ApplyAttr FillRule where
  applyAttribute _ = pure ()

instance ApplyAttr Arrow where
  applyAttribute _ = pure ()

instance ApplyAttr RArrow where
  applyAttribute _ = pure ()

instance ApplyAttr Opacity where
  applyAttribute _ = pure ()

instance ApplyAttr Tiling where
  applyAttribute _ = pure ()

instance ApplyAttr Gradient where
  applyAttribute _ = pure ()

instance ApplyAttr Transformations where
  applyAttribute _ = pure ()




-- | Looks up the colorname in the SVG colors if it is a name.
toCanvasColor :: RealFrac r => IpeColor r -> Maybe Canvas.Color
toCanvasColor (IpeColor c) = case c of
    Named t            -> h . toSRGB24 <$> readColourName (T.unpack $ T.toLower t)
    Valued v           -> Just $ f v
  where
    f (RGB r g b) = floor <$> V4 (255 *r) (255*g) (255*b) 255
    h (RGB r g b) = V4 r g b 255
