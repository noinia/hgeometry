{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Ipe.Writer where


import           Control.Applicative hiding (Const(..))
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Maybe(catMaybes)
import           Data.Monoid
import           Data.Proxy
import qualified Data.Traversable as Tr
import           Data.Vinyl
import           Data.Vinyl.Functor
import           Data.Vinyl.TypeLevel
import           Data.Geometry.Point
import           Data.Geometry.PolyLine


import           Data.Geometry.Ipe.Types
import           GHC.Exts

import qualified Data.ByteString as B
import           Data.Maybe(mapMaybe)
import           Data.List(nub)
import           Data.Monoid
import qualified Data.Sequence as S
import           Data.Text(Text)

import           Text.XML.Expat.Tree
import           Text.XML.Expat.Format(format')

import           System.IO(hPutStrLn,stderr)

import qualified Data.Text as T

--------------------------------------------------------------------------------

toIpeXML :: (IpeWriteText r, IpeWrite t) => t r -> Maybe B.ByteString
toIpeXML = fmap format' . ipeWrite

writeIpeFile    :: (IpeWriteText r, IpeWrite t) => FilePath -> t r -> IO ()
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
  ipeWrite :: IpeWriteText r => t r -> Maybe (Node Text Text)

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


mAddAtts  :: Maybe (Node Text Text) -> [(Text, Text)] -> Maybe (Node Text Text)
mn `mAddAtts` ats = fmap (`addAtts` ats) mn

addAtts :: Node Text Text -> [(Text,Text)] -> Node Text Text
n `addAtts` ats = n { eAttributes = ats ++ eAttributes n }

--------------------------------------------------------------------------------

instance IpeWriteText Double where
  ipeWriteText = ipeWriteText . T.pack . show

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
instance IpeWrite IpeSymbol where
  ipeWrite (Symbol p n) = f <$> ipeWriteText p
    where
      f ps = Element "use" [ ("pos", ps)
                           , ("name", n)
                           ] []

instance IpeWriteText (SymbolAttrElf rs r) => IpeWriteText (SymbolAttrs' r rs) where
  ipeWriteText (SymbolAttrs' x) = ipeWriteText x

instance IpeAttrName SymbolName where attrName _ = "name"
instance IpeAttrName Pos        where attrName _ = "pos"
instance IpeAttrName Stroke     where attrName _ = "stroke"
instance IpeAttrName Fill       where attrName _ = "fill"
instance IpeAttrName Pen        where attrName _ = "pen"
instance IpeAttrName Size       where attrName _ = "size"


-- instance IpeWriteAttributes (SymbolAttrs' r) where
--   ipeWriteAtts _ = [] -- TODO

-- fff :: Rec (SymbolAttrs' r) rs -> [Attr]

-- fff = ipeWriteAttrs testR

-- testR :: Rec (SymbolAttrs' Double) '[SymbolName, Pen]
-- testR =  (SymbolAttrs' "nameX") :& (SymbolAttrs' . IpePen $ Named "Foo") :& RNil

--------------------------------------------------------------------------------


instance IpeWriteText r => IpeWriteText (Operation r) where
  ipeWriteText (MoveTo p) = unwords' [ipeWriteText p, Just "m"]
  ipeWriteText (LineTo p) = unwords' [ipeWriteText p, Just "l"]
  ipeWriteText (CurveTo p q r) = unwords' [ ipeWriteText p
                                          , ipeWriteText q
                                          , ipeWriteText r, Just "m"]
  -- TODO: The rest
  ipeWriteText ClosePath = Just "h"


instance IpeWriteText r => IpeWriteText (PolyLine 2 () r) where
  ipeWriteText pl = case points pl of
    []       -> Nothing
    (p:rest) -> unlines' . map ipeWriteText $ MoveTo p : map LineTo rest


instance IpeWriteText r => IpeWriteText (PathSegment r) where
  ipeWriteText (PolyLineSegment p) = ipeWriteText p

-- instance IpeWriteAttributes PathAttrs where
--   ipeWriteAtts _ = [] -- TODO!

instance IpeWrite Path where
  ipeWrite (Path segs) = (\t -> Element "path" [] [Text t]) <$> mt
    where
      concat' = F.foldr1 (<>)
      mt      = fmap concat' . Tr.sequence . fmap ipeWriteText $ segs

--------------------------------------------------------------------------------

-- instance IpeWrite (IpeObject gt gs is ts mps ss ps) where
--   -- ipeWrite (IpeGroup     (g :+ ats)) = ipeWrite g `addAtts` ipeWriteAtts ats
--   -- ipeWrite (IpeImage     (i :+ ats)) = ipeWrite i `addAtts` ipeWriteAtts ats
--   -- ipeWrite (IpeTextLabel (l :+ ats)) = ipeWrite l `addAtts` ipeWriteAtts ats
--   -- ipeWrite (IpeMiniPage  (m :+ ats)) = ipeWrite m `addAtts` ipeWriteAtts ats
--   ipeWrite (IpeUse       (s :+ ats)) = ipeWrite s `addAtts` ipeWriteAtts ats
--   ipeWrite (IpePath      (p :+ ats)) = ipeWrite p `addAtts` ipeWriteAtts ats


-- instance IpeWrite (Group gt) where
--   ipeWrite GNil = mempty

type Atts = [(Text,Text)]

ipeWritePolyLines     :: IpeWriteText r
                      => [(PolyLine 2 () r, Atts)] -> Node Text Text
ipeWritePolyLines pls = Element "ipe" ipeAtts [Element "page" [] chs]
  where
    chs     = layers pls ++ mapMaybe f pls
    ipeAtts = [("version","70005"),("creator", "HGeometry 0.4.0.0")]

    f (pl,ats) = ipeWrite (mkPath pl) `mAddAtts` ats
    mkPath     = Path . S.singleton . PolyLineSegment
    layers     = map mkLayer . nub . mapMaybe (lookup "layer" . snd)
    mkLayer n  = Element "layer" [("name",n)] []


writePolyLineFile :: IpeWriteText r => FilePath -> [(PolyLine 2 () r, Atts)] -> IO ()
writePolyLineFile fp = B.writeFile fp . format' . ipeWritePolyLines


testPoly :: PolyLine 2 () Double
testPoly = fromPoints [origin, point2 0 10, point2 10 10, point2 100 100]
