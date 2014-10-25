{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Geometry.Ipe.Writer where


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

--------------------------------------------------------------------------------

instance IpeWriteText Double where
  ipeWriteText = T.pack . show

--------------------------------------------------------------------------------

instance IpeWriteText r => IpeWriteText (Point 2 r) where
  ipeWriteText (Point2 x y) = T.unwords [ipeWriteText x, ipeWriteText y]


--------------------------------------------------------------------------------
instance IpeWrite IpeSymbol where
  ipeWrite (Symbol p n) = Element "use" [ ("pos",  ipeWriteText p)
                                        , ("name", n)
                                        ] []

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

instance IpeWrite PathSegment where
  ipeWrite path = Element "path" [] [Text $ ipeWriteText path]
  -- TODO: The rest
