{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.Writer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
-- Description :  Converting data types into IpeTypes
--
--------------------------------------------------------------------------------
module Ipe.Writer(
    writeIpeFile, writeIpeFile', writeIpePage
  , toIpeXML
  , printAsIpeSelection, toIpeSelectionXML

  , IpeWrite(..)
  , IpeWriteText(..)

  , ipeWriteAttrs, writeAttrValues
  ) where

import           Control.Lens (view, review, (^.), (^..), toNonEmptyOf, IxValue)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Colour.SRGB (RGB (..))
import           Data.Fixed
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import           Data.Ratio
import           Data.Semigroup.Foldable
import qualified Data.Sequence as Seq
import           Data.Singletons
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Vinyl hiding (Label)
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel
import           HGeometry.BezierSpline
import           HGeometry.Box
import           HGeometry.Ellipse (ellipseMatrix)
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.Interval.EndPoint
import           HGeometry.LineSegment
import qualified HGeometry.Matrix as Matrix
import           HGeometry.Number.Real.Rational
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Polygon.Class
import           HGeometry.Polygon.Simple
import           HGeometry.Vector
import           Ipe.Attributes
import qualified Ipe.Attributes as IA
import           Ipe.Color (IpeColor (..))
import           Ipe.Path
import           Ipe.Types
import           Ipe.Value
import qualified System.File.OsPath as File
import           System.IO (hPutStrLn, stderr)
import           System.OsPath
import           Text.XML.Expat.Format (format)
import           Text.XML.Expat.Tree

--------------------------------------------------------------------------------

-- | Write an IpeFiele to file.
writeIpeFile :: IpeWriteText r => OsPath -> IpeFile r -> IO ()
writeIpeFile = flip writeIpeFile'

-- | Creates a single page ipe file with the given page
writeIpePage    :: IpeWriteText r => OsPath -> IpePage r -> IO ()
writeIpePage fp = writeIpeFile fp . singlePageFile


-- | Convert the input to ipeXml, and prints it to standard out in such a way
-- that the copied text can be pasted into ipe as a geometry object.
printAsIpeSelection :: IpeWrite t => t -> IO ()
printAsIpeSelection = C.putStrLn . fromMaybe "" . toIpeSelectionXML

-- | Convert input into an ipe selection.
toIpeSelectionXML :: IpeWrite t => t -> Maybe B.ByteString
toIpeSelectionXML = fmap (format . ipeSelection) . ipeWrite
  where
    ipeSelection x = Element "ipeselection" [] [x]


-- | Convert to Ipe xml
toIpeXML :: IpeWrite t => t -> Maybe B.ByteString
toIpeXML = fmap format . ipeWrite


-- | Convert to ipe XML and write the output to a file.
writeIpeFile'      :: IpeWrite t => t -> OsPath -> IO ()
writeIpeFile' i fp = maybe err (File.writeFile fp) . toIpeXML $ i
  where
    err = hPutStrLn stderr $
          "writeIpeFile: error converting to xml. File '" <> show fp <> "'not written"

--------------------------------------------------------------------------------

-- | For types that can produce a text value
class IpeWriteText t where
  ipeWriteText :: t -> Maybe Text

-- | Types that correspond to an XML Element. All instances should produce an
-- Element. If the type should produce a Node with the Text constructor, use
-- the `IpeWriteText` typeclass instead.
class IpeWrite t where
  ipeWrite :: t -> Maybe (Node Text Text)

instance IpeWrite t => IpeWrite [t] where
  ipeWrite gs = case mapMaybe ipeWrite gs of
                  [] -> Nothing
                  ns -> (Just $ Element "group" [] ns)

instance IpeWrite t => IpeWrite (NonEmpty t) where
  ipeWrite = ipeWrite . F.toList

instance (IpeWrite l, IpeWrite r) => IpeWrite (Either l r) where
  ipeWrite = either ipeWrite ipeWrite

instance IpeWriteText (Apply f at) => IpeWriteText (Attr f at) where
  ipeWriteText att = _getAttr att >>= ipeWriteText

instance (IpeWriteText l, IpeWriteText r) => IpeWriteText (Either l r) where
  ipeWriteText = either ipeWriteText ipeWriteText


-- | Functon to write all attributes in a Rec
ipeWriteAttrs           :: ( RecordToList rs, RMap rs
                           , ReifyConstraint IpeWriteText (Attr f) rs
                           , AllConstrained IpeAttrName rs
                           , RecAll (Attr f) rs IpeWriteText
                           ) => IA.Attributes f rs -> [(Text,Text)]
ipeWriteAttrs (Attrs r) = catMaybes . recordToList $ zipRecsWith f (writeAttrNames  r)
                                                                   (writeAttrValues r)
  where
    f (Const n) (Const mv) = Const $ (n,) <$> mv

-- | Writing the attribute values
writeAttrValues :: ( RMap rs, ReifyConstraint IpeWriteText f rs
                   , RecAll f rs IpeWriteText)
                => Rec f rs -> Rec (Const (Maybe Text)) rs
writeAttrValues = rmap (\(Compose (Dict x)) -> Const $ ipeWriteText x)
                . reifyConstraint @IpeWriteText


instance IpeWriteText Text where
  ipeWriteText = Just

instance IpeWriteText String where
  ipeWriteText = ipeWriteText . Text.pack


-- | Add attributes to a node
addAtts :: Node Text Text -> [(Text,Text)] -> Node Text Text
n@(Element {}) `addAtts` ats = n { eAttributes = ats ++ eAttributes n }
_ `addAtts` _   = error "addAts, requires Element"

-- | Same as `addAtts` but then for a Maybe node
mAddAtts  :: Maybe (Node Text Text) -> [(Text, Text)] -> Maybe (Node Text Text)
mn `mAddAtts` ats = fmap (`addAtts` ats) mn


--------------------------------------------------------------------------------

instance IpeWriteText Double where
  ipeWriteText = writeByShow

instance IpeWriteText Float where
  ipeWriteText = writeByShow

instance IpeWriteText Int where
  ipeWriteText = writeByShow

instance IpeWriteText Integer where
  ipeWriteText = writeByShow

instance IpeWriteText (RealNumber p) where
  ipeWriteText = ipeWriteText . realToFrac @(RealNumber p) @Rational

instance HasResolution p => IpeWriteText (Fixed p) where
  ipeWriteText = writeByShow

-- | This instance converts the ratio to a Pico, and then displays that.
instance Integral a => IpeWriteText (Ratio a) where
  ipeWriteText = ipeWriteText . f . fromRational . toRational
    where
      f :: Pico -> Pico
      f = id

writeByShow :: Show t => t -> Maybe Text
writeByShow = ipeWriteText . Text.pack . show

unwords' :: [Maybe Text] -> Maybe Text
unwords' = fmap Text.unwords . sequence

unlines' :: [Maybe Text] -> Maybe Text
unlines' = fmap Text.unlines . sequence


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

instance IpeWriteText r => IpeWriteText (RGB r) where
  ipeWriteText (RGB r g b) = unwords' . map ipeWriteText $ [r,g,b]

deriving instance IpeWriteText r => IpeWriteText (IpeSize  r)
deriving instance IpeWriteText r => IpeWriteText (IpePen   r)
deriving instance IpeWriteText r => IpeWriteText (IpeColor r)

instance IpeWriteText r => IpeWriteText (IpeDash r) where
  ipeWriteText (DashNamed t) = Just t
  ipeWriteText (DashPattern xs x) = (\ts t -> mconcat [ "["
                                                      , Text.intercalate " " ts
                                                      , "] ", t ])
                                    <$> mapM ipeWriteText xs
                                    <*> ipeWriteText x

instance IpeWriteText FillType where
  ipeWriteText Wind   = Just "wind"
  ipeWriteText EOFill = Just "eofill"

instance IpeWriteText r => IpeWriteText (IpeArrow r) where
  ipeWriteText (IpeArrow n s) = (\n' s' -> n' <> "/" <> s') <$> ipeWriteText n
                                                            <*> ipeWriteText s

instance IpeWriteText r => IpeWriteText (Path r) where
  ipeWriteText = fmap concat' . sequence . fmap ipeWriteText . view pathSegments
    where
      concat' = F.foldr1 (\t t' -> t <> "\n" <> t')


instance IpeWriteText HorizontalAlignment where
  ipeWriteText = \case
    AlignLeft    -> Just "left"
    AlignHCenter -> Just "center"
    AlignRight   -> Just "right"

instance IpeWriteText VerticalAlignment where
  ipeWriteText = \case
    AlignTop      -> Just "top"
    AlignVCenter  -> Just "center"
    AlignBottom   -> Just "bottom"
    AlignBaseline -> Just "baseline"

--------------------------------------------------------------------------------
instance IpeWriteText r => IpeWrite (IpeSymbol r) where
  ipeWrite (Symbol p n) = f <$> ipeWriteText p
    where
      f ps = Element "use" [ ("pos", ps)
                           , ("name", n)
                           ] []

--------------------------------------------------------------------------------

instance IpeWriteText r => IpeWriteText (Matrix.Matrix 3 3 r) where
  ipeWriteText (Matrix.Matrix m) = unwords' [a,b,c,d,e,f]
    where
      (Vector3 r1 r2 _) = m

      (Vector3 a c e) = ipeWriteText <$> r1
      (Vector3 b d f) = ipeWriteText <$> r2
      -- TODO: The third row should be (0,0,1) I guess.


instance IpeWriteText r => IpeWriteText (Operation r) where
  ipeWriteText (MoveTo p)         = unwords' [ ipeWriteText p, Just "m"]
  ipeWriteText (LineTo p)         = unwords' [ ipeWriteText p, Just "l"]
  ipeWriteText (CurveTo p q r)    = unwords' [ ipeWriteText p
                                             , ipeWriteText q
                                             , ipeWriteText r, Just "c"]
  ipeWriteText (QCurveTo p q)     = unwords' [ ipeWriteText p
                                             , ipeWriteText q, Just "q"]
  ipeWriteText (Ellipse m)        = unwords' [ ipeWriteText m, Just "e"]
  ipeWriteText (ArcTo m p)        = unwords' [ ipeWriteText m
                                             , ipeWriteText p, Just "a"]
  ipeWriteText (Spline pts)       = unlines' $ map ipeWriteText pts <> [Just "s"]
  ipeWriteText (ClosedSpline pts) = unlines' $ map ipeWriteText pts <> [Just "u"]
  ipeWriteText ClosePath          = Just "h"


instance (IpeWriteText r, Point_ point 2 r) => IpeWriteText (PolyLine point) where
  ipeWriteText pl = case pl^..vertices.asPoint of
    (p : rest) -> unlines' . map ipeWriteText $ MoveTo p : map LineTo rest
    _          -> error "ipeWriteText. absurd. no vertices polyline"
    -- the polyline type guarantees that there is at least one point

instance (IpeWriteText r, Point_ point 2 r) => IpeWriteText (SimplePolygon point) where
  ipeWriteText pg = ipeWriteTextPolygonVertices $ toNonEmptyOf (outerBoundary.asPoint) pg

ipeWriteTextPolygonVertices :: IpeWriteText r => NonEmpty (Point 2 r) -> Maybe Text
ipeWriteTextPolygonVertices = \case 
    (p :| rest) -> unlines' . map ipeWriteText $ MoveTo p : map LineTo rest ++ [ClosePath]

instance (IpeWriteText r, Point_ point 2 r) => IpeWriteText (CubicBezier point) where
  ipeWriteText (fmap (^.asPoint) -> Bezier3 p q r s) =
    unlines' . map ipeWriteText $ [MoveTo p, CurveTo q r s]

instance IpeWriteText r => IpeWriteText (PathSegment r) where
  ipeWriteText (PolyLineSegment    p) = ipeWriteText p
  ipeWriteText (PolygonPath orient p) = case orient of
    AsIs     -> ipeWriteText p
    Reversed -> ipeWriteTextPolygonVertices . NonEmpty.reverse
              $ toNonEmptyOf (outerBoundary.asPoint) p
  ipeWriteText (EllipseSegment     e) = ipeWriteText $ Ellipse (e^.ellipseMatrix)
  ipeWriteText (CubicBezierSegment b) = ipeWriteText b 
  ipeWriteText _                      = error "ipeWriteText: PathSegment, not implemented yet."

instance IpeWriteText r => IpeWrite (Path r) where
  ipeWrite p = (\t -> Element "path" [] [Text t]) <$> ipeWriteText p

--------------------------------------------------------------------------------


instance (IpeWriteText r) => IpeWrite (Group r) where
  ipeWrite (Group gs) = ipeWrite gs


instance ( AllConstrained IpeAttrName rs
         , RecordToList rs, RMap rs
         , ReifyConstraint IpeWriteText (Attr f) rs
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
                               <*> ipeWriteText a
                               <*> ipeWriteText b

-- TODO: Replace this one with s.t. that writes the actual image payload
instance IpeWriteText () where
  ipeWriteText () = Nothing

instance IpeWriteText r => IpeWriteText (TextSizeUnit r) where
  ipeWriteText (TextSizeUnit x) = ipeWriteText x

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

--------------------------------------------------------------------------------

deriving instance IpeWriteText LayerName

instance IpeWrite LayerName where
  ipeWrite (LayerName n) = Just $ Element "layer" [("name",n)] []

instance IpeWrite View where
  ipeWrite (View lrs act) = Just $ Element "view" [ ("layers", ls)
                                                  , ("active", act^.layerName)
                                                  ] []
    where
      ls = Text.unwords .  map (^.layerName) $ lrs

instance (IpeWriteText r)  => IpeWrite (IpePage r) where
  ipeWrite (IpePage lrs vs objs) = Just .
                                  Element "page" [] . catMaybes . concat $
                                  [ map ipeWrite lrs
                                  , map ipeWrite vs
                                  , map ipeWrite objs
                                  ]


instance IpeWrite IpeStyle where
  ipeWrite (IpeStyle _ xml) = Just xml


instance IpeWrite IpePreamble where
  ipeWrite (IpePreamble _ latex) = Just $ Element "preamble" [] [Text latex]
  -- TODO: I probably want to do something with the encoding ....

instance (IpeWriteText r) => IpeWrite (IpeFile r) where
  ipeWrite (IpeFile mp ss pgs) = Just $ Element "ipe" ipeAtts chs
    where
      ipeAtts = [("version","70005"),("creator", "HGeometry")]
      chs = mconcat [ catMaybes [mp >>= ipeWrite]
                    , mapMaybe ipeWrite ss
                    , mapMaybe ipeWrite . F.toList $ pgs
                    ]




--------------------------------------------------------------------------------

instance (IpeWriteText r, Point_ point 2 r, Functor f, Foldable1 f
         ) => IpeWrite (PolyLineF f point) where
  ipeWrite = ipeWrite . fromPolyLine

fromPolyLine               :: (Point_ point 2 r, Functor f, Foldable1 f)
                           => PolyLineF f point -> Path r
fromPolyLine (PolyLine vs) =
  Path . Seq.singleton . PolyLineSegment . PolyLine . fromFoldable1 . fmap (view asPoint) $ vs


instance ( IpeWriteText r
         , EndPoint_ (endPoint point)
         , IxValue (endPoint point) ~ point
         , Vertex (LineSegment endPoint point) ~ point

         , Point_ point 2 r
         ) => IpeWrite (LineSegment endPoint point) where
  ipeWrite = ipeWrite @(PolyLineF NonEmpty point) . review _PolyLineLineSegment

instance IpeWrite () where
  ipeWrite = const Nothing
