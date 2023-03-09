{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.Literal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Including xml literals
--
--------------------------------------------------------------------------------
module Ipe.Literal where


import Language.Haskell.TH
import Language.Haskell.TH.Quote
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import Text.XML.Expat.Tree

-- | Include a literal expression
literally :: String -> Q Exp
literally = return . LitE . StringL

-- | Literal quoter.
lit :: QuasiQuoter
lit = QuasiQuoter { quoteExp = literally
                  , quotePat = undefined
                  , quoteType = undefined
                  , quoteDec  = undefined
                  }

-- | Include a file as a literal.
litFile :: QuasiQuoter
litFile = quoteFile lit

-- | Parse a string into a Node.
xmlLiteral :: String -> Node T.Text T.Text
xmlLiteral = either (error "xmlLiteral. error parsing xml: " . show) id
           .  parse' defaultParseOptions . C.pack
