{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Geometry.Ipe.Writer where

import           Control.Applicative
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Monoid
import qualified Data.Traversable as Tr
import           Data.Vinyl

import           Data.Geometry.Point
import           Data.Geometry.PolyLine


import           Data.Geometry.Ipe.Types

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

class IpeWriteText t where
  ipeWriteText :: t -> Maybe Text

class IpeWrite t where
  ipeWrite :: IpeWriteText r => t r -> Maybe (Node Text Text)

class IpeWriteAttributes ats where
  ipeWriteAtts :: Rec ats rs -> [(Text,Text)]


mAddAtts  :: Maybe (Node Text Text) -> [(Text, Text)] -> Maybe (Node Text Text)
mn `mAddAtts` ats = fmap (`addAtts` ats) mn

addAtts :: Node Text Text -> [(Text,Text)] -> Node Text Text
n `addAtts` ats = n { eAttributes = ats ++ eAttributes n }


--------------------------------------------------------------------------------

instance IpeWriteText Double where
  ipeWriteText = Just . T.pack . show


unwords' :: [Maybe Text] -> Maybe Text
unwords' = fmap T.unwords . sequence

unlines' :: [Maybe Text] -> Maybe Text
unlines' = fmap T.unlines . sequence


instance IpeWriteText r => IpeWriteText (Point 2 r) where
  ipeWriteText (Point2 x y) = unwords' [ipeWriteText x, ipeWriteText y]


--------------------------------------------------------------------------------
instance IpeWrite IpeSymbol where
  ipeWrite (Symbol p n) = f <$> ipeWriteText p
    where
      f ps = Element "use" [ ("pos", ps)
                           , ("name", n)
                           ] []

instance IpeWriteAttributes SymbolAttrs where
  ipeWriteAtts _ = [] -- TODO



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

instance IpeWriteAttributes PathAttrs where
  ipeWriteAtts _ = [] -- TODO!

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
