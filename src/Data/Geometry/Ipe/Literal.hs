{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.Ipe.Literal where


import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Text.XML.Expat.Tree

literally :: String -> Q Exp
literally = return . LitE . StringL

lit :: QuasiQuoter
lit = QuasiQuoter { quoteExp = literally
                  , quotePat = undefined
                  , quoteType = undefined
                  , quoteDec  = undefined
                  }

litFile :: QuasiQuoter
litFile = quoteFile lit


xmlLiteral :: String -> Node T.Text T.Text
xmlLiteral = either (error "xmlLiteral. error parsing xml: " . show) id
           .  parse' defaultParseOptions . C.pack
