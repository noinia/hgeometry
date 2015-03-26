{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Ipe.Writer where

import           Control.Applicative hiding (Const(..))
import           Control.Lens((^.),(^..),(.~),(&))
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Ipe.Types
import qualified Data.Geometry.Ipe.Types as IT
import           Data.Geometry.Line
import qualified Data.Geometry.Transformation as GT
import           Data.Geometry.Point
import           Data.Geometry.Vector
import           Data.Maybe(catMaybes)
import           Data.Monoid
import           Data.Proxy
import qualified Data.Traversable as Tr
import           Data.Vinyl
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel

import           Data.Geometry.Ipe.Attributes
import           GHC.Exts

import qualified Data.ByteString as B
import           Data.Maybe(mapMaybe)
import           Data.List(nub)
import           Data.Monoid
import qualified Data.Sequence as S
import qualified Data.Seq2     as S2
import           Data.Text(Text)

import           Text.XML.Expat.Tree
import           Text.XML.Expat.Format(format')

import           System.IO(hPutStrLn,stderr)

import qualified Data.Text as T

--------------------------------------------------------------------------------

toIpeXML :: IpeWrite t => t -> Maybe B.ByteString
toIpeXML = fmap format' . ipeWrite

writeIpeFile    :: IpeWrite t => FilePath -> t -> IO ()
writeIpeFile fp = maybe err (B.writeFile fp) . toIpeXML
  where
    err = hPutStrLn stderr $
          "writeIpeFile: error converting to xml. File '" <> fp <> "'not written"

--------------------------------------------------------------------------------

-- | For types that can produce a text value
class IpeWriteText t where
  ipeWriteText :: t -> Maybe Text

-- | Types that correspond to an XML Element. All instances should produce an
-- Element. If the type should produce a Node with the Text constructor, use
-- the `IpeWriteText` typeclass instead.
class IpeWrite t where
  ipeWrite :: t -> Maybe (Node Text Text)


-- | Ipe write for Exts
ipeWriteExt             :: ( IpeWrite t
                           , RecAll a as IpeWriteText, AllSatisfy IpeAttrName as
                           ) => t :+ Rec a as -> Maybe (Node Text Text)
ipeWriteExt (x :+ arec) = ipeWrite x `mAddAtts` ipeWriteAttrs arec


-- | For the types representing attribute values we can get the name/key to use
-- when serializing to ipe.
class IpeAttrName a where
  attrName :: Proxy a -> Text


type Attr = (Text,Text)

-- | Functon to write all attributes in a Rec
ipeWriteAttrs :: ( RecAll f rs IpeWriteText
                 , AllSatisfy IpeAttrName rs
                 ) => Rec f rs -> [Attr]
ipeWriteAttrs rs = mapMaybe (\(x,my) -> (x,) <$> my) $ zip (writeAttrNames  rs)
                                                           (writeAttrValues rs)


-- | Writing the attribute values
writeAttrValues :: RecAll f rs IpeWriteText => Rec f rs -> [Maybe Text]
writeAttrValues = recordToList
                . rmap (\(Compose (Dict x)) -> Const $ ipeWriteText x)
                . reifyConstraint (Proxy :: Proxy IpeWriteText)

-- | Writing Attribute names
writeAttrNames           :: AllSatisfy IpeAttrName rs => Rec f rs -> [Text]
writeAttrNames RNil      = []
writeAttrNames (x :& xs) = write'' x : writeAttrNames xs
  where
    write''   :: forall f s. IpeAttrName s => f s -> Text
    write'' _ = attrName (Proxy :: Proxy s)

-- | Function that states that all elements in xs satisfy a given constraint c
type family AllSatisfy (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  AllSatisfy c '[] = ()
  AllSatisfy c (x ': xs) = (c x, AllSatisfy c xs)


instance IpeWriteText Text where
  ipeWriteText = Just

-- | Add attributes to a node
addAtts :: Node Text Text -> [(Text,Text)] -> Node Text Text
n `addAtts` ats = n { eAttributes = ats ++ eAttributes n }

-- | Same as `addAtts` but then for a Maybe node
mAddAtts  :: Maybe (Node Text Text) -> [(Text, Text)] -> Maybe (Node Text Text)
mn `mAddAtts` ats = fmap (`addAtts` ats) mn


--------------------------------------------------------------------------------

instance IpeWriteText Double where
  ipeWriteText = writeByShow

instance IpeWriteText Int where
  ipeWriteText = writeByShow


writeByShow :: Show t => t -> Maybe Text
writeByShow = ipeWriteText . T.pack . show



unwords' :: [Maybe Text] -> Maybe Text
unwords' = fmap T.unwords . sequence

unlines' :: [Maybe Text] -> Maybe Text
unlines' = fmap T.unlines . sequence


instance IpeWriteText r => IpeWriteText (Point 2 r) where
  ipeWriteText (Point2 x y) = unwords' [ipeWriteText x, ipeWriteText y]


--------------------------------------------------------------------------------

instance IpeWriteText v => IpeWriteText (IpeValue v) where
  ipeWriteText (Named t)  = ipeWriteText t
  ipeWriteText (Valued v) = ipeWriteText v

deriving instance IpeWriteText r => IpeWriteText (IpeSize  r)
deriving instance IpeWriteText r => IpeWriteText (IpePen   r)
deriving instance IpeWriteText IpeColor

--------------------------------------------------------------------------------
instance IpeWriteText r => IpeWrite (IpeSymbol r) where
  ipeWrite (Symbol p n) = f <$> ipeWriteText p
    where
      f ps = Element "use" [ ("pos", ps)
                           , ("name", n)
                           ] []

instance IpeWriteText (SymbolAttrElf rs r) => IpeWriteText (SymbolAttribute r rs) where
  ipeWriteText (SymbolAttribute x) = ipeWriteText x


-- CommonAttributeUnivers
instance IpeAttrName Layer           where attrName _ = "layer"
instance IpeAttrName Matrix          where attrName _ = "matrix"
instance IpeAttrName Pin             where attrName _ = "pin"
instance IpeAttrName Transformations where attrName _ = "transformations"

-- IpeSymbolAttributeUniversre
instance IpeAttrName SymbolStroke where attrName _ = "stroke"
instance IpeAttrName SymbolFill   where attrName _ = "fill"
instance IpeAttrName SymbolPen    where attrName _ = "pen"
instance IpeAttrName Size         where attrName _ = "size"

-- PathAttributeUniverse
instance IpeAttrName Stroke     where attrName _ = "stroke"
instance IpeAttrName Fill       where attrName _ = "fill"
instance IpeAttrName Dash       where attrName _ = "dash"
instance IpeAttrName Pen        where attrName _ = "pen"
instance IpeAttrName LineCap    where attrName _ = "cap"
instance IpeAttrName LineJoin   where attrName _ = "join"
instance IpeAttrName FillRule   where attrName _ = "fillrule"
instance IpeAttrName Arrow      where attrName _ = "arrow"
instance IpeAttrName RArrow     where attrName _ = "rarrow"
instance IpeAttrName Opacity    where attrName _ = "opacity"
instance IpeAttrName Tiling     where attrName _ = "tiling"
instance IpeAttrName Gradient   where attrName _ = "gradient"

-- GroupAttributeUniverse
instance IpeAttrName Clip     where attrName _ = "clip"

--------------------------------------------------------------------------------

instance IpeWriteText r => IpeWriteText (GT.Matrix 3 3 r) where
  ipeWriteText (GT.Matrix m) = unwords' [a,b,c,d,e,f]
    where
      (Vector3 r1 r2 _) = m

      (Vector3 a c e) = ipeWriteText <$> r1
      (Vector3 b d f) = ipeWriteText <$> r2
      -- TODO: The third row should be (0,0,1) I guess.


instance IpeWriteText r => IpeWriteText (Operation r) where
  ipeWriteText (MoveTo p)      = unwords' [ ipeWriteText p, Just "m"]
  ipeWriteText (LineTo p)      = unwords' [ ipeWriteText p, Just "l"]
  ipeWriteText (CurveTo p q r) = unwords' [ ipeWriteText p
                                          , ipeWriteText q
                                          , ipeWriteText r, Just "m"]
  ipeWriteText (Ellipse m)     = unwords' [ ipeWriteText m, Just "e"]
  -- TODO: The rest
  ipeWriteText ClosePath       = Just "h"


instance IpeWriteText r => IpeWriteText (PolyLine 2 () r) where
  ipeWriteText pl = case pl^..points.Tr.traverse.core of
    (p : rest) -> unlines' . map ipeWriteText $ MoveTo p : map LineTo rest
    -- the polyline type guarantees that there is at least one point


instance IpeWriteText r => IpeWriteText (PathSegment r) where
  ipeWriteText (PolyLineSegment p) = ipeWriteText p

instance IpeWriteText (PathAttrElf rs r) => IpeWriteText (PathAttribute r rs) where
  ipeWriteText (PathAttribute x) = ipeWriteText x

instance IpeWriteText r => IpeWrite (Path r) where
  ipeWrite (Path segs) = (\t -> Element "path" [] [Text t]) <$> mt
    where
      concat' = F.foldr1 (<>)
      mt      = fmap concat' . Tr.sequence . fmap ipeWriteText $ segs

--------------------------------------------------------------------------------


instance ( IpeObjectElF r fld  ~ (g :+ Rec f ats)
         , IpeWrite g
         , RecAll f ats IpeWriteText, AllSatisfy IpeAttrName ats
         ) => IpeWrite (IpeObject r fld) where
  ipeWrite (IpeObject (g :+ ats)) = ipeWrite g `mAddAtts` ipeWriteAttrs ats


ipeWriteRec :: RecAll f rs IpeWrite => Rec f rs -> [Node Text Text]
ipeWriteRec = catMaybes . recordToList
            . rmap (\(Compose (Dict x)) -> Const $ ipeWrite x)
            . reifyConstraint (Proxy :: Proxy IpeWrite)

instance RecAll (IpeObject r) gt IpeWrite => IpeWrite (Group gt r) where
  -- basically the same implementation as ipeWriteAttrs: convert the rec to a Rec Const
  -- then turn that into a list. If the list is non-empty we construct a new group element.
  ipeWrite = fmap (Element "group" [])
           . wrap . ipeWriteRec
    where
      wrap [] = Nothing
      wrap xs = Just xs

instance IpeWriteText (GroupAttrElf rs r) => IpeWriteText (GroupAttribute r rs) where
  ipeWriteText (GroupAttribute x) = ipeWriteText x


--------------------------------------------------------------------------------

instance IpeWrite IT.Layer where
  ipeWrite (IT.Layer l) = Just $ Element "layer" [("name", l)] []

instance IpeWrite View where
  ipeWrite (View lrs act) = Just $ Element "view" [ ("layers", ls)
                                                  , ("active", _layerName act)
                                                  ] []
    where
      ls = T.unwords .  map _layerName $ lrs

instance IpeWrite (Group gs r) => IpeWrite (IpePage gs r) where
  ipeWrite (IpePage lrs vs pgs) = Just . Element "page" [] . catMaybes . concat $
                                  [ map ipeWrite lrs
                                  , map ipeWrite vs
                                  , [ipeWrite pgs]
                                  ]

instance RecAll (Page r) gs IpeWrite => IpeWrite (IpeFile gs r) where
  ipeWrite (IpeFile p s pgs) = Just $ Element "ipe" ipeAtts chs
    where
    ipeAtts = [("version","70005"),("creator", "HGeometry")]
    -- TODO: Add preamble and styles
    chs = ipeWriteRec pgs


--------------------------------------------------------------------------------

type Atts = [(Text,Text)]

ipeWritePolyLines     :: IpeWriteText r
                      => [(PolyLine 2 () r, Atts)] -> Node Text Text
ipeWritePolyLines pls = Element "ipe" ipeAtts [Element "page" [] chs]
  where
    chs     = layers pls ++ mapMaybe f pls
    ipeAtts = [("version","70005"),("creator", "HGeometry 0.4.0.0")]

    f (pl,ats) = ipeWrite (mkPath pl) `mAddAtts` ats
    mkPath     = Path . S2.l1Singleton . PolyLineSegment
    layers     = map mkLayer . nub . mapMaybe (lookup "layer" . snd)
    mkLayer n  = Element "layer" [("name",n)] []


writePolyLineFile :: IpeWriteText r => FilePath -> [(PolyLine 2 () r, Atts)] -> IO ()
writePolyLineFile fp = B.writeFile fp . format' . ipeWritePolyLines


instance (IpeWriteText r, IpeWrite p) => IpeWrite (PolyLine 2 p r) where
  ipeWrite p = ipeWrite path
    where
      path = fromPolyLine $ p & points.Tr.traverse.extra .~ ()
      -- TODO: Do something with the p's

fromPolyLine = Path . S2.l1Singleton . PolyLineSegment


instance (IpeWriteText r) => IpeWrite (LineSegment 2 p r) where
  ipeWrite (LineSegment p q) = ipeWrite . fromPolyLine . fromPoints . map (^.core) $ [p,q]


instance IpeWrite () where
  ipeWrite = const Nothing

-- instance (IpeWrite a, IpeWrite b) => IpeWrite (a,b) where
--   ipeWrite (a,b) =

-- | The default symbol for a point
ipeWritePoint :: IpeWriteText r => Point 2 r -> Maybe (Node Text Text)
ipeWritePoint = ipeWrite . flip Symbol "mark/disk(sx)"


-- instance (IpeWriteText r) => IpeWrite (Circle r) where
--   ipeWrite



--------------------------------------------------------------------------------



testPoly :: PolyLine 2 () Double
testPoly = fromPoints [origin, point2 0 10, point2 10 10, point2 100 100]




testWriteUse :: Maybe (Node Text Text)
testWriteUse = ipeWriteExt sym
  where
    sym :: IpeSymbol Double :+ (Rec (SymbolAttribute Double) [Size, SymbolStroke])
    sym = Symbol origin "mark" :+ (  SymbolAttribute (IpeSize  $ Named "normal")
                                  :& SymbolAttribute (IpeColor $ Named "green")
                                  :& RNil
                                  )



foo = ipeWrite grrr
