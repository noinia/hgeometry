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
import qualified Data.Geometry.Polygon as Polygon
import           Data.Geometry.PolyLine
import qualified Data.Geometry.Transformation as Trans
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Ipe.Attributes
import qualified Data.Geometry.Ipe.Attributes as IA
import           Data.Geometry.Ipe.PathParser

import qualified Data.ByteString as B
import           Data.Monoid
import           Data.Singletons
import           Data.Text(Text)


import           Text.XML.Expat.Tree

import qualified Data.Text as T
-- import qualified Data.Map as M

--------------------------------------------------------------------------------

type ConversionError = Text

--------------------------------------------------------------------------------

class IpeReadText t where
  ipeReadText :: Text -> Either ConversionError t

-- instance IpeReadText Text where
--   ipeReadText = Right

-- instance Coordinate r => IpeReadText r where
--   ipeReadText = readCoordinate

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


-- instance Coordinate r => IpeReadText (IpeDash r) where

ipeReadTextWith     :: (Text -> Either t v) -> Text -> Either ConversionError (IpeValue v)
ipeReadTextWith f t = case f t of
                        Right v -> Right (Valued v)
                        Left _  -> Right (Named t)


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


-- fromIpeFile :: (Coordinate r, IpeRead t) => FilePath -> IO [PolyLine 2 () r]
-- fromIpeFile


fromIpeXML   :: (Coordinate r, IpeRead (t r))
             => B.ByteString -> Either ConversionError (t r)
fromIpeXML b = (bimap (T.pack . show) id $ parse' defaultParseOptions b) >>= ipeRead

class IpeRead t where
  ipeRead  :: Node Text Text -> Either ConversionError t

class IpeReadAttr t where
  ipeReadAttr  :: Text -> Node Text Text -> Either ConversionError t


instance IpeReadText (Apply f at) => IpeReadAttr (Attr f at) where
  ipeReadAttr n (Element _ ats _) = GAttr <$> Tr.mapM ipeReadText (lookup n ats)
  ipeReadAttr _ _                 = Left "IpeReadAttr: Element expected, Text found"





-- ipeReadRec :: forall t r f (ats :: [AttributeUniverse]).
--                            ( RecApplicative ats
--                            , RecAll (Attr f) ats IpeReadAttr
--                            )
--                            => Proxy f -> Proxy ats
--                            -> Node Text Text
--                            -> Either ConversionError (Rec (Attr  f) ats)
-- ipeReadRec prf _ x = rtraverse f
--                    . reifyConstraint (Proxy :: Proxy IpeReadAttr)
--                    $ rpure (GAttr Nothing)
--   where
--     f                    :: forall at. (Dict IpeReadAttr :. Attr f) at
--                          -> Either ConversionError (Attr f at)
--     f (Compose (Dict _)) = ipeReadAttr "foo" x


zipTraverseWith                       :: forall f g h i (rs :: [u]). Applicative h
                                      => (forall (x :: u). f x -> g x -> h (i x))
                                      -> Rec f rs -> Rec g rs -> h (Rec i rs)
zipTraverseWith _ RNil      RNil      = pure RNil
zipTraverseWith f (x :& xs) (y :& ys) = (:&) <$> f x y <*> zipTraverseWith f xs ys

ipeReadRec :: forall r f (ats :: [AttributeUniverse]).
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


    f                    :: forall at.
                            Const Text at -> (Dict IpeReadAttr :. Attr f) at
                         -> Either ConversionError (Attr f at)
    f (Const n) (Compose (Dict _)) = ipeReadAttr n x

ipeReadAttrs             :: forall proxy i r f (ats :: [AttributeUniverse]).
                         ( f ~ AttrMapSym1 r, ats ~ IpeObjectAttrF i
                         ,  RecApplicative ats
                         , RecAll (Attr f) ats IpeReadAttr
                         , AllSatisfy IpeAttrName ats
                         )
                         => Proxy i -> proxy r
                         -> Node Text Text
                         -> Either ConversionError (IpeAttributes i r)
ipeReadAttrs prI prR = fmap Attrs . ipeReadRec (Proxy :: Proxy f) (Proxy :: Proxy ats)

testSym :: B.ByteString
testSym = "<use name=\"mark/disk(sx)\" pos=\"320 736\" size=\"normal\" stroke=\"black\"/>"

readSymAttrs :: Either ConversionError (IpeAttributes IpeSymbol Double)
readSymAttrs = (bimap (T.pack . show) id $ parse' defaultParseOptions testSym)
               >>= ipeReadAttrs (Proxy :: Proxy IpeSymbol) (Proxy :: Proxy Double)

ipeReadObject           :: ( IpeRead (i r)
                           , f ~ AttrMapSym1 r, ats ~ IpeObjectAttrF i
                           ,  RecApplicative ats
                           , RecAll (Attr f) ats IpeReadAttr
                           , AllSatisfy IpeAttrName ats
                           )
                        => Proxy i -> proxy r -> Node Text Text
                        -> Either ConversionError (i r :+ IpeAttributes i r)
ipeReadObject prI prR xml = (:+) <$> ipeRead xml <*> ipeReadAttrs prI prR xml


instance Coordinate r => IpeRead (IpeSymbol r) where
  ipeRead (Element "use" ats _) = case lookup "pos" ats of
      Nothing -> Left "symbol without position"
      Just ps -> flip Symbol name <$> ipeReadText ps
    where
      name = fromMaybe "mark/disk(sx)" $ lookup "name" ats
  ipeRead _ = Left "symbol element expected, text found"

-- instance Coordinate r => IpeRead (IpeAttributes IpeSymbol r) where
--   ipeRead = ipeReadAttrs (Proxy :: Proxy IpeSymbol) (Proxy :: Proxy r)

testz = (bimap (T.pack . show) id $ parse' defaultParseOptions testSym)
               >>= ipeReadObject (Proxy :: Proxy IpeSymbol) (Proxy :: Proxy Double)


-- instance Coordinate r => IpeRead (IpeObject r) where
--   ipeRead xml = firstRight [ IpeGroup <$> ipeReadObject (Proxy :: Proxy Group) (Proxy :: Proxy r) xml
--                            ]
--     where
--       firstRight =







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

testO :: Text
testO = "\n128 656 m\n224 768 l\n304 624 l\n432 752 l\n"

testPoly :: Either Text (PolyLine 2 () Double)
testPoly = fromIpeXML testP

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
