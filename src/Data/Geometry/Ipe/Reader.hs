{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module Data.Geometry.Ipe.Reader where

import           Data.Proxy
import           Data.Either(rights)
import           Control.Applicative hiding (Const)
import           Control.Lens hiding (only, Const, rmap)

import           Data.Ext
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr
import           Data.Maybe(fromMaybe, isJust, mapMaybe)
import qualified Data.List as L
import           Data.Vinyl
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel

import qualified Data.List.NonEmpty as NE
-- import           Data.Validation
import qualified Data.Seq2     as S2

import           Data.Geometry.Point
import           Data.Geometry.Box
import qualified Data.Geometry.Polygon as Polygon
import           Data.Geometry.PolyLine
import qualified Data.Geometry.Transformation as Trans
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Ipe.Attributes
import qualified Data.Geometry.Ipe.Attributes as IA
import           Data.Geometry.Ipe.PathParser
import           Data.Geometry.Ipe.ParserPrimitives(pInteger)

import qualified Data.ByteString as B
import           Data.Monoid
import           Data.Singletons
import           Data.Text(Text)


import           Text.XML.Expat.Tree

import qualified Data.Text as T
-- import qualified Data.Map as M

--------------------------------------------------------------------------------

type ConversionError = Text


-- | Given a file path, tries to read an ipe file
readRawIpeFile :: Coordinate r => FilePath -> IO (Either ConversionError (IpeFile r))
readRawIpeFile = fmap fromIpeXML . B.readFile


-- | Given a file path, tries to read an ipe file. This function applies all
-- matrices to objects.
readIpeFile :: Coordinate r => FilePath -> IO (Either ConversionError (IpeFile r))
readIpeFile = fmap (bimap id applyMatrices) . readRawIpeFile


-- | Since most Ipe file contain only one page, we provide a shortcut for that
-- as well. This function applies all matrices.
readSinglePageFile :: Coordinate r => FilePath -> IO (Either ConversionError (IpePage r))
readSinglePageFile = fmap f . readIpeFile
  where
    f (Left e)  = Left e
    f (Right i) = maybe (Left "No Ipe pages found") Right . firstOf (pages.traverse) $ i

-- | Given a Bytestring, try to parse the bytestring into anything that is
-- IpeReadable, i.e. any of the Ipe elements.
fromIpeXML   :: (Coordinate r, IpeRead (t r))
             => B.ByteString -> Either ConversionError (t r)
fromIpeXML b = readXML b >>= ipeRead

-- | Reads the data from a Bytestring into a proper Node
readXML :: B.ByteString -> Either ConversionError (Node Text Text)
readXML = bimap (T.pack . show) id . parse' defaultParseOptions

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

instance Coordinate r => IpeReadText (Trans.Matrix 3 3 r) where
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


ipeReadTextWith     :: (Text -> Either t v) -> Text -> Either ConversionError (IpeValue v)
ipeReadTextWith f t = case f t of
                        Right v -> Right (Valued v)
                        Left _  -> Right (Named t)


instance Coordinate r => IpeReadText (Rectangle () r) where
  ipeReadText = readRectangle

instance IpeReadText IpeColor where
  ipeReadText = fmap IpeColor . ipeReadTextWith Right

instance Coordinate r => IpeReadText (IpePen r) where
  ipeReadText = fmap IpePen . ipeReadTextWith readCoordinate

instance Coordinate r => IpeReadText (IpeSize r) where
  ipeReadText = fmap IpeSize . ipeReadTextWith readCoordinate


instance Coordinate r => IpeReadText [Operation r] where
  ipeReadText = readPathOperations

instance Coordinate r => IpeReadText (NE.NonEmpty (PathSegment r)) where
  ipeReadText t = ipeReadText t >>= fromOpsN
    where
      fromOpsN xs = case fromOps xs of
                      Left l       -> Left l
                      Right []     -> Left "No path segments produced"
                      Right (p:ps) -> Right $ p NE.:| ps

      fromOps []            = Right []
      fromOps (MoveTo p:xs) = fromOps' p xs
      fromOps _             = Left "Path should start with a move to"

      fromOps' _ []             = Left "Found only a MoveTo operation"
      fromOps' s (LineTo q:ops) = let (ls,xs) = span' _LineTo ops
                                      pts  = map only $ s:q:mapMaybe (^?_LineTo) ls
                                      poly = Polygon.fromPoints pts
                                      pl   = fromPoints pts
                                  in case xs of
                                       (ClosePath : xs') -> PolygonPath poly   <<| xs'
                                       _                 -> PolyLineSegment pl <<| xs
      fromOps' _ _ = Left "fromOpts': rest not implemented yet."

      span' pr = L.span (not . isn't pr)

      x <<| xs = (x:) <$> fromOps xs

instance Coordinate r => IpeReadText (Path r) where
  ipeReadText = fmap (Path . S2.viewL1FromNonEmpty) . ipeReadText

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
zipTraverseWith                       :: forall f g h i (rs :: [u]). Applicative h
                                      => (forall (x :: u). f x -> g x -> h (i x))
                                      -> Rec f rs -> Rec g rs -> h (Rec i rs)
zipTraverseWith _ RNil      RNil      = pure RNil
zipTraverseWith f (x :& xs) (y :& ys) = (:&) <$> f x y <*> zipTraverseWith f xs ys

-- | Reading the Attributes into a Rec (Attr f), all based on the types of f
-- (the type family mapping labels to types), and a list of labels (ats).
ipeReadRec       :: forall f ats.
                 ( RecApplicative ats
                 , RecAll (Attr f) ats IpeReadAttr
                 , AllSatisfy IpeAttrName ats
                 )
                 => Proxy f -> Proxy ats
                 -> Node Text Text
                 -> Either ConversionError (Rec (Attr  f) ats)
ipeReadRec _ _ x = zipTraverseWith f (writeAttrNames r) r'
  where
    r  = rpure (GAttr Nothing)
    r' = reifyConstraint (Proxy :: Proxy IpeReadAttr) r


    f                              :: forall at.
                                      Const Text at
                                   -> (Dict IpeReadAttr :. Attr f) at
                                   -> Either ConversionError (Attr f at)
    f (Const n) (Compose (Dict _)) = ipeReadAttr n x


-- | Reader for records. Given a proxy of some ipe type i, and a proxy of an
-- coordinate type r, read the IpeAttributes for i from the xml node.
ipeReadAttrs     :: forall proxy proxy' i r f ats.
                 ( f ~ AttrMapSym1 r, ats ~ IpeObjectAttrF i
                 , RecApplicative ats
                 , RecAll (Attr f) ats IpeReadAttr
                 , AllSatisfy IpeAttrName ats
                 )
                 => proxy i -> proxy' r
                 -> Node Text Text
                 -> Either ConversionError (IpeAttributes i r)
ipeReadAttrs _ _ = fmap Attrs . ipeReadRec (Proxy :: Proxy f) (Proxy :: Proxy ats)


testSym :: B.ByteString
testSym = "<use name=\"mark/disk(sx)\" pos=\"320 736\" size=\"normal\" stroke=\"black\"/>"




-- readAttrsFromXML :: B.ByteString -> Either

readSymAttrs :: Either ConversionError (IpeAttributes IpeSymbol Double)
readSymAttrs = readXML testSym
               >>= ipeReadAttrs (Proxy :: Proxy IpeSymbol) (Proxy :: Proxy Double)





-- | If we can ipeRead an ipe element, and we can ipeReadAttrs its attributes
-- we can properly read an ipe object using ipeReadObject
ipeReadObject           :: ( IpeRead (i r)
                           , f ~ AttrMapSym1 r, ats ~ IpeObjectAttrF i
                           ,  RecApplicative ats
                           , RecAll (Attr f) ats IpeReadAttr
                           , AllSatisfy IpeAttrName ats
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

instance Coordinate r => IpeRead (Path r) where
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

instance Coordinate r => IpeRead (IpeObject r) where
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


instance Coordinate r => IpeRead (Group r) where
  ipeRead (Element "group" _ chs) = Group <$> mapM ipeRead chs
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
instance Coordinate r => IpeRead (IpePage r) where
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


instance Coordinate r => IpeRead (IpeFile r) where
  ipeRead (Element "ipe" _ chs) = case readAll chs of
                                    []  -> Left "Ipe: no pages found"
                                    pgs -> Right $ IpeFile Nothing [] (NE.fromList pgs)
  ipeRead _                     = Left "Ipe: Element expected, text found"



testz :: Either ConversionError (IpeObject Double)
testz = (bimap (T.pack . show) id $ parse' defaultParseOptions testSym)
               >>= ipeRead -- Object (Proxy :: Proxy IpeSymbol) (Proxy :: Proxy Double)














  -- ipeReadAttrs (Element _ ats _) =

instance Coordinate r => IpeReadText (PolyLine 2 () r) where
  ipeReadText t = readPathOperations t >>= fromOps
    where
      fromOps (MoveTo p:LineTo q:ops) = (\ps -> fromPoints' $ [p,q] ++ ps)
                                     <$> validateAll "Expected LineTo p" _LineTo ops
      fromOps _                       = Left "Expected MoveTo p:LineTo q:... "

validateAll         :: ConversionError -> Prism' (Operation r) (Point 2 r) -> [Operation r]
                    -> Either ConversionError [Point 2 r]
validateAll err fld = bimap T.unlines id . validateAll' err fld


validateAll' :: err -> Prism' (Operation r) (Point 2 r) -> [Operation r]
               -> Either [err] [Point 2 r]
validateAll' err field = toEither . foldr (\op res -> f op <> res) (Right' [])
  where
    f op = maybe (Left' [err]) (\p -> Right' [p]) $ op ^? field
    toEither = either' Left Right

-- This is a bit of a hack
instance Coordinate r => IpeRead (PolyLine 2 () r) where
  ipeRead (Element "path" ats ts) = ipeReadText . T.unlines . map unText $ ts
                                    -- apparently hexpat already splits the text into lines
  ipeRead _                       = Left "iperead: no polyline."

unText          :: Node t t1 -> t1
unText (Text t) = t
unText _        = error "unText: element found, text expected"

instance Coordinate r => IpeRead (PathSegment r) where
  ipeRead = fmap PolyLineSegment . ipeRead

testP :: B.ByteString
testP = "<path stroke=\"black\">\n128 656 m\n224 768 l\n304 624 l\n432 752 l\n</path>"


-- testPoly :: Either Text (Path Double)
-- testPoly = fromIpeXML testP

-- ipeRead' :: [Element Text Text]
-- ipeRead' = map ipeRead

-- instance IpeRead (IpePage gs) where
--   ipeRead (Element "page" ats chs) = Right . IpePage [] [] . fromList' . rights $ map ipeRead chs
--     where
--       fromList' = Group' . foldr (\x r -> (IpeObject x :& RNil) :& r) RNil
--   ipeRead _                        = Left "ipeRead: Not a page"

readPolyLines :: Coordinate r => Node Text Text -> [PolyLine 2 () r]
readPolyLines (Element "ipe" _ chs) = concatMap readPolyLines' chs


readPolyLines' :: Coordinate r => Node Text Text -> [PolyLine 2 () r]
readPolyLines' (Element "page" _ chs) = rights $ map ipeRead chs
readPolyLines' _                      = []

polylinesFromIpeFile :: (Coordinate r) => FilePath -> IO [PolyLine 2 () r]
polylinesFromIpeFile = fmap readPolies . B.readFile
  where
    readPolies = either (const []) readPolyLines . parse' defaultParseOptions

--------------------------------------------------------------------------------
