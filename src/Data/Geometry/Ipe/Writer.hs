{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Geometry.Ipe.Writer where

import           Data.Ext
import qualified Data.Foldable as F
import           Data.Vinyl

import           Data.Geometry.Point
import           Data.Geometry.PolyLine


import           Data.Geometry.Ipe.Types

import qualified Data.ByteString as B
import           Data.Monoid
import           Data.Text(Text)

import           Text.XML.Expat.Tree
import           Text.XML.Expat.Format(format')

import qualified Data.Text as T

--------------------------------------------------------------------------------

toIpeXML :: (IpeWriteText r, IpeWrite t) => t r -> B.ByteString
toIpeXML = format' . ipeWrite

writeIpeFile    :: (IpeWriteText r, IpeWrite t) => FilePath -> t r -> IO ()
writeIpeFile fp = B.writeFile fp . toIpeXML

--------------------------------------------------------------------------------

class IpeWriteText t where
  ipeWriteText :: t -> Text

class IpeWrite t where
  ipeWrite :: IpeWriteText r => t r -> Node Text Text

class IpeWriteAttributes ats where
  ipeWriteAtts :: Rec ats rs -> [(Text,Text)]

addAtts :: Node Text Text -> [(Text,Text)] -> Node Text Text
n `addAtts` ats = n { eAttributes = ats ++ eAttributes n }

--------------------------------------------------------------------------------

instance IpeWriteText Double where
  ipeWriteText = T.pack . show

instance IpeWriteText r => IpeWriteText (Point 2 r) where
  ipeWriteText (Point2 x y) = T.unwords [ipeWriteText x, ipeWriteText y]


--------------------------------------------------------------------------------
instance IpeWrite IpeSymbol where
  ipeWrite (Symbol p n) = Element "use" [ ("pos",  ipeWriteText p)
                                        , ("name", n)
                                        ] []

instance IpeWriteAttributes SymbolAttrs where
  ipeWriteAtts _ = [] -- TODO



--------------------------------------------------------------------------------


instance IpeWriteText r => IpeWriteText (Operation r) where
  ipeWriteText (MoveTo p) = T.unwords [ipeWriteText p, "m"]
  ipeWriteText (LineTo p) = T.unwords [ipeWriteText p, "l"]
  ipeWriteText (CurveTo p q r) = T.unwords [ ipeWriteText p
                                           , ipeWriteText q
                                           , ipeWriteText r, "m"]
  -- TODO: The rest
  ipeWriteText ClosePath = "h"


instance IpeWriteText r => IpeWriteText (PolyLine 2 () r) where
  ipeWriteText pl = let (p:rest) = points pl
                        ops      = MoveTo p : map LineTo rest
                    in T.unlines . map ipeWriteText $ ops


instance IpeWriteText r => IpeWriteText (PathSegment r) where
  ipeWriteText (PolyLineSegment p) = ipeWriteText p

instance IpeWriteAttributes PathAttrs where
  ipeWriteAtts _ = [] -- TODO!

instance IpeWrite Path where
  ipeWrite (Path segs) = Element "path" []
                         [Text . F.foldr1 (<>) . fmap ipeWriteText $ segs]

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
