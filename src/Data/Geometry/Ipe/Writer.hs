{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Ipe.Writer where

import           Control.Applicative hiding (Const(..))
import           Control.Lens((^.),(^..),(.~),(&), Prism', (#), to)
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Ipe.Types
import qualified Data.Geometry.Ipe.Types as IT
import           Data.Geometry.LineSegment
import           Data.Geometry.PolyLine
import           Data.Geometry.Polygon(SimplePolygon, outerBoundary)
import qualified Data.Geometry.Transformation as GT
import           Data.Geometry.Point
import           Data.Geometry.Box
import           Data.Geometry.Vector
import           Data.Maybe(catMaybes, mapMaybe, fromMaybe)
import           Data.Semigroup
import           Data.Proxy
import qualified Data.Traversable as Tr
import           Data.Vinyl
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel

import           Data.Singletons
import qualified Data.CircularList as CL
import qualified Data.Geometry.Ipe.Attributes as IA

import           Data.Geometry.Ipe.Attributes
import           GHC.Exts

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.List(nub)
import qualified Data.Seq2     as S2
import           Data.Text(Text)
import qualified Data.Text as Text

import           Text.XML.Expat.Tree
import           Text.XML.Expat.Format(format')

import           System.IO(hPutStrLn,stderr)

import qualified Data.Text as T

--------------------------------------------------------------------------------

-- | Given a prism to convert something of type g into an ipe file, a file path,
-- and a g. Convert the geometry and write it to file.

-- writeIpe        :: ( RecAll (Page r) gs IpeWrite
--                    , IpeWriteText r
--                    ) => Prism' (IpeFile gs r) g -> FilePath -> g -> IO ()
-- writeIpe p fp g = writeIpeFile (p # g) fp

-- | Write an IpeFiele to file.

-- writeIpeFile :: ( RecAll (Page r) gs IpeWrite
--                 , IpeWriteText r
--                 ) => IpeFile gs r -> FilePath -> IO ()
-- writeIpeFile = writeIpeFile'

-- | Convert the input to ipeXml, and prints it to standard out in such a way
-- that the copied text can be pasted into ipe as a geometry object.
printAsIpeSelection :: IpeWrite t => t -> IO ()
printAsIpeSelection = C.putStrLn . fromMaybe "" . toIpeSelectionXML

-- | Convert input into an ipe selection.
toIpeSelectionXML :: IpeWrite t => t -> Maybe B.ByteString
toIpeSelectionXML = fmap (format' . ipeSelection) . ipeWrite
  where
    ipeSelection x = Element "ipeselection" [] [x]


-- | Convert to Ipe xml
toIpeXML :: IpeWrite t => t -> Maybe B.ByteString
toIpeXML = fmap format' . ipeWrite


-- | Convert to ipe XML and write the output to a file.
writeIpeFile'      :: IpeWrite t => t -> FilePath -> IO ()
writeIpeFile' i fp = maybe err (B.writeFile fp) . toIpeXML $ i
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


instance IpeWriteText (Apply f at) => IpeWriteText (Attr f at) where
  ipeWriteText attr = _getAttr attr >>= ipeWriteText

-- | Functon to write all attributes in a Rec
ipeWriteAttrs           :: ( AllSatisfy IpeAttrName rs
                           , RecAll (Attr f) rs IpeWriteText
                           ) => IA.Attributes f rs -> [(Text,Text)]
ipeWriteAttrs (Attrs r) = catMaybes . recordToList $ zipRecsWith f (writeAttrNames  r)
                                                                   (writeAttrValues r)
  where
    f (Const n) (Const mv) = Const $ (n,) <$> mv

-- | Writing the attribute values
writeAttrValues :: RecAll f rs IpeWriteText => Rec f rs -> Rec (Const (Maybe Text)) rs
writeAttrValues = rmap (\(Compose (Dict x)) -> Const $ ipeWriteText x)
                . reifyConstraint (Proxy :: Proxy IpeWriteText)


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

instance IpeWriteText TransformationTypes where
  ipeWriteText Affine       = Just "affine"
  ipeWriteText Rigid        = Just "rigid"
  ipeWriteText Translations = Just "translations"

instance IpeWriteText PinType where
  ipeWriteText No         = Nothing
  ipeWriteText Yes        = Just "yes"
  ipeWriteText Horizontal = Just "h"
  ipeWriteText Vertical   = Just "v"

deriving instance IpeWriteText r => IpeWriteText (IpeSize  r)
deriving instance IpeWriteText r => IpeWriteText (IpePen   r)
deriving instance IpeWriteText IpeColor

instance IpeWriteText r => IpeWriteText (IpeDash r) where
  ipeWriteText (DashNamed t) = Just t
  ipeWriteText (DashPattern xs x) = (\ts t -> mconcat [ "["
                                                      , Text.intercalate " " ts
                                                      , "] ", t ])
                                    <$> Tr.mapM ipeWriteText xs
                                    <*> ipeWriteText x

instance IpeWriteText FillType where
  ipeWriteText Wind   = Just "wind"
  ipeWriteText EOFill = Just "eofill"

instance IpeWriteText r => IpeWriteText (IpeArrow r) where
  ipeWriteText (IpeArrow n s) = (\n s -> n <> "/" <> s) <$> ipeWriteText n
                                                        <*> ipeWriteText s

instance IpeWriteText r => IpeWriteText (Path r) where
  ipeWriteText = fmap concat' . Tr.sequence . fmap ipeWriteText . _pathSegments
    where
      concat' = F.foldr1 (\t t' -> t <> "\n" <> t')


--------------------------------------------------------------------------------
instance IpeWriteText r => IpeWrite (IpeSymbol r) where
  ipeWrite (Symbol p n) = f <$> ipeWriteText p
    where
      f ps = Element "use" [ ("pos", ps)
                           , ("name", n)
                           ] []

-- instance IpeWriteText (SymbolAttrElf rs r) => IpeWriteText (SymbolAttribute r rs) where
--   ipeWriteText (SymbolAttribute x) = ipeWriteText x



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

instance IpeWriteText r => IpeWriteText (SimplePolygon () r) where
  ipeWriteText pg = case pg^..outerBoundary.to CL.toList.Tr.traverse.core of
    (p : rest) -> unlines' . map ipeWriteText $ MoveTo p : map LineTo rest ++ [ClosePath]
    _          -> Nothing
    -- TODO: We are not really guaranteed that there is at least one point, it would
    -- be nice if the type could guarantee that.

instance IpeWriteText r => IpeWriteText (PathSegment r) where
  ipeWriteText (PolyLineSegment p) = ipeWriteText p
  ipeWriteText (PolygonPath     p) = ipeWriteText p
  ipeWriteText (EllipseSegment  m) = ipeWriteText $ Ellipse m

instance IpeWriteText r => IpeWrite (Path r) where
  ipeWrite p = (\t -> Element "path" [] [Text t]) <$> ipeWriteText p

--------------------------------------------------------------------------------


instance (IpeWriteText r) => IpeWrite (Group r) where
  ipeWrite (Group gs) = case mapMaybe ipeWrite gs of
                          [] -> Nothing
                          ns -> (Just $ Element "group" [] ns)


instance ( AllSatisfy IpeAttrName rs
         , RecAll (Attr f) rs IpeWriteText
         , IpeWrite g
         ) => IpeWrite (g :+ IA.Attributes f rs) where
  ipeWrite (g :+ ats) = ipeWrite g `mAddAtts` ipeWriteAttrs ats


instance IpeWriteText r => IpeWrite (MiniPage r) where
  ipeWrite (MiniPage t p w) = (\pt wt ->
                              Element "text" [ ("pos", pt)
                                             , ("type", "minipage")
                                             , ("width", wt)
                                             ] [Text t]
                              ) <$> ipeWriteText p
                                <*> ipeWriteText w

instance IpeWriteText r => IpeWrite (Image r) where
  ipeWrite (Image d (Box a b)) = (\dt p q ->
                                   Element "image" [("rect", p <> " " <> q)] [Text dt]
                                 )
                               <$> ipeWriteText d
                               <*> ipeWriteText (a^.core.(to getMin))
                               <*> ipeWriteText (b^.core.(to getMax))

-- TODO: Replace this one with s.t. that writes the actual image payload
instance IpeWriteText () where
  ipeWriteText () = Nothing

instance IpeWriteText r => IpeWrite (TextLabel r) where
  ipeWrite (Label t p) = (\pt ->
                         Element "text" [("pos", pt)
                                        ,("type", "label")
                                        ] [Text t]
                         ) <$> ipeWriteText p


instance (IpeWriteText r) => IpeWrite (IpeObject r) where
    ipeWrite (IpeGroup     g) = ipeWrite g
    ipeWrite (IpeImage     i) = ipeWrite i
    ipeWrite (IpeTextLabel l) = ipeWrite l
    ipeWrite (IpeMiniPage  m) = ipeWrite m
    ipeWrite (IpeUse       s) = ipeWrite s
    ipeWrite (IpePath      p) = ipeWrite p


ipeWriteRec :: RecAll f rs IpeWrite => Rec f rs -> [Node Text Text]
ipeWriteRec = catMaybes . recordToList
            . rmap (\(Compose (Dict x)) -> Const $ ipeWrite x)
            . reifyConstraint (Proxy :: Proxy IpeWrite)


-- instance IpeWriteText (GroupAttrElf rs r) => IpeWriteText (GroupAttribute r rs) where
--   ipeWriteText (GroupAttribute x) = ipeWriteText x


--------------------------------------------------------------------------------

deriving instance IpeWriteText LayerName

instance IpeWrite LayerName where
  ipeWrite (LayerName n) = Just $ Element "layer" [("name",n)] []

instance IpeWrite View where
  ipeWrite (View lrs act) = Just $ Element "view" [ ("layers", ls)
                                                  , ("active", _layerName act)
                                                  ] []
    where
      ls = T.unwords .  map _layerName $ lrs

instance (IpeWriteText r)  => IpeWrite (IpePage r) where
  ipeWrite (IpePage lrs vs objs) = Just .
                                  Element "page" [] . catMaybes . concat $
                                  [ map ipeWrite lrs
                                  , map ipeWrite vs
                                  , map ipeWrite objs
                                  ]


instance (IpeWriteText r) => IpeWrite (IpeFile r) where
  ipeWrite (IpeFile p s pgs) = Just $ Element "ipe" ipeAtts chs
    where
    ipeAtts = [("version","70005"),("creator", "HGeometry")]
    -- TODO: Add preamble and styles
    chs = mapMaybe ipeWrite . F.toList $ pgs


--------------------------------------------------------------------------------

type Atts = [(Text,Text)]

ipeWritePolyLines' :: IpeWriteText r
                  => [(PolyLine 2 () r, Atts)] -> Maybe (Node Text Text)
ipeWritePolyLines' = ipeWrite . singlePageFromContent . map f
  where
    f (pl,ats) = ipeObject' (mkPath pl) mempty -- TODO: We ignore the ats, as they are in the wrong format
    mkPath     = Path . S2.l1Singleton . PolyLineSegment



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
  ipeWrite (LineSegment' p q) = ipeWrite . fromPolyLine . fromPoints . map (extra .~ ()) $ [p,q]


instance IpeWrite () where
  ipeWrite = const Nothing

-- -- | slightly clever instance that produces a group if there is more than one
-- -- element and just an element if there is only one value produced
-- instance IpeWrite a => IpeWrite [a] where
--   ipeWrite = combine . mapMaybe ipeWrite


combine     :: [Node Text Text] -> Maybe (Node Text Text)
combine []  = Nothing
combine [n] = Just n
combine ns  = Just $ Element "group" [] ns

-- instance (IpeWrite a, IpeWrite b) => IpeWrite (a,b) where
--   ipeWrite (a,b) = combine . catMaybes $ [ipeWrite a, ipeWrite b]



-- -- | The default symbol for a point
-- ipeWritePoint :: IpeWriteText r => Point 2 r -> Maybe (Node Text Text)
-- ipeWritePoint = ipeWrite . flip Symbol "mark/disk(sx)"


-- instance (IpeWriteText r, Floating r) => IpeWrite (Circle r) where
--   ipeWrite = ipeWrite . Path . S2.l1Singleton . fromCircle



--------------------------------------------------------------------------------



-- testPoly :: PolyLine 2 () Double
-- testPoly = fromPoints' [origin, point2 0 10, point2 10 10, point2 100 100]




-- testWriteUse :: Maybe (Node Text Text)
-- testWriteUse = ipeWriteExt sym
--   where
--     sym :: IpeSymbol Double :+ (Rec (SymbolAttribute Double) [Size, SymbolStroke])
--     sym = Symbol origin "mark" :+ (  SymbolAttribute (IpeSize  $ Named "normal")
--                                   :& SymbolAttribute (IpeColor $ Named "green")
--                                   :& RNil
--                                   )
