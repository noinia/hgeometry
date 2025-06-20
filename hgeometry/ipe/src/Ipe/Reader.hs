{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ipe.Reader
  ( -- * Reading ipe Files
    readRawIpeFile
  , readIpeFile
  , readSinglePageFile
  , readSinglePageFileThrow
  , ConversionError
  -- * Readiing ipe style files
  , readIpeStylesheet
  , addStyleSheetFrom

    -- * Reading XML directly
  , fromIpeXML
  , readXML

    -- * Read classes
  , IpeReadText(..)
  , IpeRead(..)
  , IpeReadAttr(..)


    -- * Some low level implementation functions
  , ipeReadTextWith
  , ipeReadObject
  , ipeReadAttrs
  , ipeReadRec

  , Coordinate(..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Lens hiding (Const, rmap)
import           Control.Monad ((<=<))
import           Data.Bifunctor
import qualified Data.ByteString as B
import           Data.Colour.SRGB (RGB(..))
import           Data.Either (rights)
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Proxy
import qualified Data.Sequence as Seq
import           Data.Singletons
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Traversable as Tr
import           Data.Vinyl hiding (Label)
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel
import           HGeometry.BezierSpline
import           HGeometry.Box
import           HGeometry.Ellipse (ellipseMatrix)
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import qualified HGeometry.Matrix as Matrix
import           HGeometry.Point
import           HGeometry.PolyLine (polyLineFromPoints)
import qualified HGeometry.Polygon.Simple as Polygon
import           HGeometry.Vector
import           Ipe.Attributes
import           Ipe.Color (IpeColor(..))
import           Ipe.Matrix
import           Ipe.ParserPrimitives (pInteger, pWhiteSpace)
import           Ipe.Path
import           Ipe.PathParser
import           Ipe.Types
import           Ipe.Value
import qualified System.File.OsPath as File
import           System.OsPath
import           Text.XML.Expat.Tree

--------------------------------------------------------------------------------

type ConversionError = Text


-- | Given a file path, tries to read an ipe file
readRawIpeFile :: (Coordinate r, Eq r)
               => OsPath -> IO (Either ConversionError (IpeFile r))
readRawIpeFile = fmap fromIpeXML . File.readFile'


-- | Given a file path, tries to read an ipe file.
--
-- This function applies all matrices to objects.
readIpeFile :: (Coordinate r, Eq r)
            => OsPath -> IO (Either ConversionError (IpeFile r))
readIpeFile = fmap (second applyMatrices) . readRawIpeFile


-- | Since most Ipe file contain only one page, we provide a shortcut for that
-- as well.
--
-- This function applies all matrices, and it makes sure there is at
-- least one layer and view in the page.
--
readSinglePageFile :: (Coordinate r, Eq r)
                   => OsPath -> IO (Either ConversionError (IpePage r))
readSinglePageFile = fmap (fmap f) . readIpeFile
  where
    f   :: IpeFile r -> IpePage r
    f i = withDefaults . NonEmpty.head $ i^.pages

-- | Tries to read a single page file, throws an error when this
-- fails. See 'readSinglePageFile' for further details.
readSinglePageFileThrow    :: (Coordinate r, Eq r) => OsPath -> IO (IpePage r)
readSinglePageFileThrow fp = readSinglePageFile fp >>= \case
  Left err -> fail (show err)
  Right p  -> pure p

-- | Given a Bytestring, try to parse the bytestring into anything that is
-- IpeReadable, i.e. any of the Ipe elements.
fromIpeXML   :: IpeRead (t r) => B.ByteString -> Either ConversionError (t r)
fromIpeXML b = readXML b >>= ipeRead

-- | Reads the data from a Bytestring into a proper Node
readXML :: B.ByteString -> Either ConversionError (Node Text Text)
readXML = first (T.pack . show) . parse' defaultParseOptions

--------------------------------------------------------------------------------

-- | Reading an ipe elemtn from a Text value
class IpeReadText t where
  ipeReadText :: Text -> Either ConversionError t

-- | Reading an ipe lement from Xml
class IpeRead t where
  ipeRead  :: Node Text Text -> Either ConversionError t

--------------------------------------------------------------------------------
--  ReadText instances

instance IpeReadText Text where
  ipeReadText = Right

instance IpeReadText Int where
  ipeReadText = fmap fromInteger . runParser pInteger

instance Coordinate r => IpeReadText (Point 2 r) where
  ipeReadText = readPoint

instance Coordinate r => IpeReadText (Matrix.Matrix 3 3 r) where
  ipeReadText = readMatrix

instance IpeReadText LayerName where
  ipeReadText = Right . LayerName

instance IpeReadText PinType where
  ipeReadText "yes" = Right Yes
  ipeReadText "h"   = Right Horizontal
  ipeReadText "v"   = Right Vertical
  ipeReadText ""    = Right No
  ipeReadText _     = Left "invalid PinType"

instance IpeReadText TransformationTypes where
  ipeReadText "affine"       = Right Affine
  ipeReadText "rigid"        = Right Rigid
  ipeReadText "translations" = Right Translations
  ipeReadText _              = Left "invalid TransformationType"

instance IpeReadText FillType where
  ipeReadText "wind"   = Right Wind
  ipeReadText "eofill" = Right EOFill
  ipeReadText _        = Left "invalid FillType"

instance Coordinate r => IpeReadText (IpeArrow r) where
  ipeReadText t = case T.split (== '/') t of
                    [n,s] -> IpeArrow <$> pure n <*> ipeReadText s
                    _     -> Left "ipeArrow: name contains not exactly 1 / "

instance Coordinate r => IpeReadText (IpeDash r) where
  ipeReadText t = Right . DashNamed $ t
                  -- TODO: Implement proper parsing here


instance IpeReadText HorizontalAlignment where
  ipeReadText = \case
    "left"   -> Right AlignLeft
    "center" -> Right AlignHCenter
    "right"  -> Right AlignRight
    _        -> Left "invalid HorizontalAlignment"

instance IpeReadText VerticalAlignment where
  ipeReadText = \case
    "top"      -> Right AlignTop
    "center"   -> Right AlignVCenter
    "bottom"   -> Right AlignBottom
    "baseline" -> Right AlignBaseline
    _          -> Left "invalid VerticalAlignment"


instance Coordinate r => IpeReadText (TextSizeUnit r) where
  ipeReadText = fmap TextSizeUnit . readCoordinate


ipeReadTextWith     :: (Text -> Either t v) -> Text -> Either ConversionError (IpeValue v)
ipeReadTextWith f t = case f t of
                        Right v -> Right (Valued v)
                        Left _  -> Right (Named t)


instance Coordinate r => IpeReadText (Rectangle (Point 2 r)) where
  ipeReadText = readRectangle

instance Coordinate r => IpeReadText (RGB r) where
  ipeReadText = runParser (pRGB <|> pGrey)
    where
      pGrey = (\c -> RGB c c c) <$> pCoordinate
      pRGB  = RGB <$> pCoordinate <* pWhiteSpace
                  <*> pCoordinate <* pWhiteSpace
                  <*> pCoordinate

instance Coordinate r => IpeReadText (IpeColor r) where
  ipeReadText = fmap IpeColor . ipeReadTextWith ipeReadText

instance Coordinate r => IpeReadText (IpePen r) where
  ipeReadText = fmap IpePen . ipeReadTextWith readCoordinate

instance Coordinate r => IpeReadText (IpeSize r) where
  ipeReadText = fmap IpeSize . ipeReadTextWith readCoordinate


instance Coordinate r => IpeReadText [Operation r] where
  ipeReadText = readPathOperations

instance (Coordinate r, Fractional r, Eq r) => IpeReadText (NonEmpty.NonEmpty (PathSegment r)) where
  ipeReadText t = ipeReadText t >>= fromOpsN
    where
      fromOpsN xs = case fromOps xs of
                      Left l       -> Left l
                      Right []     -> Left "No path segments produced"
                      Right (p:ps) -> Right $ p NonEmpty.:| ps

      fromOps []            = Right []
      fromOps [Ellipse m]   = Right [EllipseSegment . view (from ellipseMatrix) $ m]
      fromOps (MoveTo p:xs) = fromOps' p xs
      fromOps _             = Left "Path should start with a move to"

      fromOps' _ []             = Left "Found only a MoveTo operation"
      fromOps' s (LineTo q:ops) = let (ls,xs) = span' _LineTo ops
                                      pts  = s NonEmpty.:| q:mapMaybe (^?_LineTo) ls
                                      mPoly = Polygon.fromPoints pts
                                      pl    = polyLineFromPoints pts
                                      or'   = if isCounterClockwise pts
                                              then Ipe.Path.AsIs
                                              else Ipe.Path.Reversed
                                  in case xs of
                                       (ClosePath : xs') -> case mPoly of
                                         Nothing         -> Left "simple polygon failed"
                                         Just poly       -> PolygonPath or' poly <<| xs'
                                       _                 -> PolyLineSegment pl <<| xs

      fromOps' s [Spline [a, b]]  = Right [QuadraticBezierSegment $ Bezier2 s a b]
      fromOps' s [Spline [a, b, c]]  = Right [CubicBezierSegment $ Bezier3 s a b c]
      fromOps' s [Spline ps] = Right $ map CubicBezierSegment $ splineToCubicBeziers $ s : ps
      -- these will not occur anymore with recent ipe files
      fromOps' s [QCurveTo a b]  = Right [QuadraticBezierSegment $ Bezier2 s a b]
      fromOps' s [CurveTo a b c] = Right [CubicBezierSegment $ Bezier3 s a b c]
      fromOps' _ _ = Left "fromOpts': rest not implemented yet."

      span' pr = L.span (not . isn't pr)

      x <<| xs = (x:) <$> fromOps xs


-- | test if the sequence of points, forming a simple polygon, is in CCW order
isCounterClockwise :: (Eq r, Num r) => NonEmpty.NonEmpty (Point 2 r) -> Bool
isCounterClockwise = (\x -> x == abs x) . signedArea2X'
  where
    signedArea2X' poly = sum [ p^.xCoord * q^.yCoord - q^.xCoord * p^.yCoord
                             | (p,q) <- edges poly
                             ]
    edges poly = let poly' = F.toList poly in zip poly' (NonEmpty.tail poly <> poly')


-- | Read a list of control points of a uniform cubic B-spline and conver it
--   to cubic Bezier pieces
splineToCubicBeziers :: Fractional r => [Point 2 r] -> [CubicBezier (Point 2 r)]
splineToCubicBeziers [a, b, c, d] = [Bezier3 a b c d]
splineToCubicBeziers (a : b : c : d : rest) =
  let p = b .+^ ((c .-. b) ^/ 2)
      q = c .+^ ((d .-. c) ^/ 3)
      r = p .+^ ((q .-. p) ^/ 2)
  in (Bezier3 a b p r) : splineToCubicBeziers (r : q : d : rest)
splineToCubicBeziers _ = error "splineToCubicBeziers needs at least four points"

instance (Coordinate r, Fractional r, Eq r) => IpeReadText (Path r) where
  ipeReadText = fmap (Path . fromNonEmpty') . ipeReadText
    where
      fromNonEmpty' :: NonEmpty.NonEmpty a -> Seq.Seq a
      fromNonEmpty' = fromFoldable

--------------------------------------------------------------------------------
-- Reading attributes

-- | Basically IpeReadText for attributes. This class is not really meant to be
-- implemented directly. Just define an IpeReadText instance for the type
-- (Apply f at), then the generic instance below takes care of looking up the
-- name of the attribute, and calling the right ipeReadText value. This class
-- is just so that reifyConstraint in `ipeReadRec` can select the right
-- typeclass when building the rec.
class IpeReadAttr t where
  ipeReadAttr  :: Text -> Node Text Text -> Either ConversionError t

instance IpeReadText (Apply f at) => IpeReadAttr (Attr f at) where
  ipeReadAttr n (Element _ ats _) = GAttr <$> Tr.mapM ipeReadText (lookup n ats)
  ipeReadAttr _ _                 = Left "IpeReadAttr: Element expected, Text found"

-- | Combination of zipRecWith and traverse
zipTraverseWith                       :: forall f g h i (rs :: [AttributeUniverse]). Applicative h
                                      => (forall (x :: AttributeUniverse). f x -> g x -> h (i x))
                                      -> Rec f rs -> Rec g rs -> h (Rec i rs)
zipTraverseWith _ RNil      RNil      = pure RNil
zipTraverseWith f (x :& xs) (y :& ys) = (:&) <$> f x y <*> zipTraverseWith f xs ys

-- | Reading the Attributes into a Rec (Attr f), all based on the types of f
-- (the type family mapping labels to types), and a list of labels (ats).
ipeReadRec       :: forall f ats.
                 ( RecApplicative ats
                 , ReifyConstraint IpeReadAttr (Attr f) ats
                 , RecAll (Attr f) ats IpeReadAttr
                 , AllConstrained IpeAttrName ats
                 )
                 => Proxy f -> Proxy ats
                 -> Node Text Text
                 -> Either ConversionError (Rec (Attr  f) ats)
ipeReadRec _ _ x = zipTraverseWith f (writeAttrNames r) r'
  where
    r  = rpure (GAttr Nothing)
    r' = reifyConstraint @IpeReadAttr r


    f                              :: forall at.
                                      Const Text at
                                   -> (Dict IpeReadAttr :. Attr f) at
                                   -> Either ConversionError (Attr f at)
    f (Const n) (Compose (Dict _)) = ipeReadAttr n x


-- | Reader for records. Given a proxy of some ipe type i, and a proxy of an
-- coordinate type r, read the IpeAttributes for i from the xml node.
ipeReadAttrs     :: forall proxy proxy' i r f ats.
                 ( f ~ AttrMapSym1 r, ats ~ AttributesOf i
                 , ReifyConstraint IpeReadAttr (Attr f) ats
                 , RecApplicative ats
                 , RecAll (Attr f) ats IpeReadAttr
                 , AllConstrained IpeAttrName ats
                 )
                 => proxy i -> proxy' r
                 -> Node Text Text
                 -> Either ConversionError (IpeAttributes i r)
ipeReadAttrs _ _ = fmap Attrs . ipeReadRec (Proxy :: Proxy f) (Proxy :: Proxy ats)


-- testSym :: B.ByteString
-- testSym = "<use name=\"mark/disk(sx)\" pos=\"320 736\" size=\"normal\" stroke=\"black\"/>"




-- readAttrsFromXML :: B.ByteString -> Either

-- readSymAttrs :: Either ConversionError (IpeAttributes IpeSymbol Double)
-- readSymAttrs = readXML testSym
--                >>= ipeReadAttrs (Proxy :: Proxy IpeSymbol) (Proxy :: Proxy Double)





-- | If we can ipeRead an ipe element, and we can ipeReadAttrs its attributes
-- we can properly read an ipe object using ipeReadObject
ipeReadObject           :: ( IpeRead (i r)
                           , f ~ AttrMapSym1 r, ats ~ AttributesOf i
                           , RecApplicative ats
                           , ReifyConstraint IpeReadAttr (Attr f) ats
                           , RecAll (Attr f) ats IpeReadAttr
                           , AllConstrained IpeAttrName ats
                           )
                        => Proxy i -> proxy r -> Node Text Text
                        -> Either ConversionError (i r :+ IpeAttributes i r)
ipeReadObject prI prR xml = (:+) <$> ipeRead xml <*> ipeReadAttrs prI prR xml


--------------------------------------------------------------------------------
-- | Ipe read instances

instance Coordinate r => IpeRead (IpeSymbol r) where
  ipeRead (Element "use" ats _) = case lookup "pos" ats of
      Nothing -> Left "symbol without position"
      Just ps -> flip Symbol name <$> ipeReadText ps
    where
      name = fromMaybe "mark/disk(sx)" $ lookup "name" ats
  ipeRead _ = Left "symbol element expected, text found"

-- | Given a list of Nodes, try to parse all of them as a big text. If we
-- encounter anything else then text, the parsing fails.
allText :: [Node Text Text] -> Either ConversionError Text
allText = fmap T.unlines . mapM unT
  where
    unT (Text t) = Right t
    unT _        = Left "allText: Expected Text, found an Element"

instance (Coordinate r, Fractional r, Eq r) => IpeRead (Path r) where
  ipeRead (Element "path" _ chs) = allText chs >>= ipeReadText
  ipeRead _                      = Left "path: expected element, found text"


lookup'   :: Text -> [(Text,a)] -> Either ConversionError a
lookup' k = maybe (Left $ "lookup' " <> k <> " not found") Right . lookup k

instance Coordinate r => IpeRead (TextLabel r) where
  ipeRead (Element "text" ats chs)
    | lookup "type" ats == Just "label" = Label
                                       <$> allText chs
                                       <*> (lookup' "pos" ats >>= ipeReadText)
    | otherwise                         = Left "Not a Text label"
  ipeRead _                             = Left "textlabel: Expected element, found text"



instance Coordinate r => IpeRead (MiniPage r) where
  ipeRead (Element "text" ats chs)
    | lookup "type" ats == Just "minipage" = MiniPage
                                          <$> allText chs
                                          <*> (lookup' "pos"   ats >>= ipeReadText)
                                          <*> (lookup' "width" ats >>= readCoordinate)
    | otherwise                            = Left "Not a MiniPage"
  ipeRead _                                = Left "MiniPage: Expected element, found text"


instance Coordinate r => IpeRead (Image r) where
  ipeRead (Element "image" ats _) = Image () <$> (lookup' "rect" ats >>= ipeReadText)
  ipeRead _                       = Left "Image: Element expected, text found"

instance (Coordinate r, Fractional r, Eq r) => IpeRead (IpeObject r) where
  ipeRead x = firstRight [ IpeUse       <$> ipeReadObject (Proxy :: Proxy IpeSymbol) r x
                         , IpePath      <$> ipeReadObject (Proxy :: Proxy Path)      r x
                         , IpeGroup     <$> ipeReadObject (Proxy :: Proxy Group)     r x
                         , IpeTextLabel <$> ipeReadObject (Proxy :: Proxy TextLabel) r x
                         , IpeMiniPage  <$> ipeReadObject (Proxy :: Proxy MiniPage)  r x
                         , IpeImage     <$> ipeReadObject (Proxy :: Proxy Image)     r x
                         ]
    where
      r = Proxy :: Proxy r

firstRight :: [Either ConversionError a] -> Either ConversionError a
firstRight = maybe (Left "No matching object") Right . firstOf (traverse._Right)


instance (Coordinate r, Eq r) => IpeRead (Group r) where
  ipeRead (Element "group" _ chs) = Right . Group . rights . map ipeRead $ chs
  ipeRead _                       = Left "ipeRead Group: expected Element, found Text"


instance IpeRead LayerName where
  ipeRead (Element "layer" ats _) = LayerName <$> lookup' "name" ats
  ipeRead _                       = Left "layer: Expected element, found text"

instance IpeRead View where
  ipeRead (Element "view" ats _) = (\lrs a -> View (map LayerName $ T.words lrs) a)
                                <$> lookup' "layers" ats
                                <*> (lookup' "active" ats >>= ipeReadText)
  ipeRead _                      = Left "View Expected element, found text"


-- TODO: this instance throws away all of our error collecting (and is pretty
-- slow/stupid since it tries parsing all children with all parsers)
instance (Coordinate r, Eq r) => IpeRead (IpePage r) where
  ipeRead (Element "page" _ chs) = Right $ IpePage (readAll chs) (readAll chs) (readAll chs)
  ipeRead _                      = Left "page: Element expected, text found"
      -- withDef   :: b -> Either a b -> Either c b
      -- withDef d = either (const $ Right d) Right

      -- readLayers  = withDef ["alpha"] . readAll
      -- readViews   = withDef []        . readAll
      -- readObjects = withDef []        . readAll

-- | try reading everything as an a. Throw away whatever fails.
readAll   :: IpeRead a => [Node Text Text] -> [a]
readAll   = rights . map ipeRead


instance (Coordinate r, Eq r) => IpeRead (IpeFile r) where
  ipeRead (Element "ipe" _ chs) = case readAll chs of
                                    []  -> Left "Ipe: no pages found"
                                    pgs -> Right $ IpeFile Nothing [] (NonEmpty.fromList pgs)
  ipeRead _                     = Left "Ipe: Element expected, text found"


instance IpeRead IpeStyle where
  ipeRead = \case
    xml@(Element "ipestyle" ats _) -> Right $ IpeStyle (lookup "name" ats) xml
    _                              -> Left "ipeStyle exptected. Something else found"


-- | Reads an Ipe stylesheet from Disk.
readIpeStylesheet :: OsPath -> IO (Either ConversionError IpeStyle)
readIpeStylesheet = fmap (ipeRead <=< readXML) . File.readFile'

-- | Given a path to a stylesheet, add it to the ipe file with the
-- highest priority. Throws an error when this fails.
addStyleSheetFrom      :: OsPath -> IpeFile r -> IO (IpeFile r)
addStyleSheetFrom fp f = readIpeStylesheet fp >>= \case
  Left err -> fail (show err)
  Right s  -> pure $ addStyleSheet s f

--------------------------------------------------------------------------------
