{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Miso.Svg
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Render geometric objects to Svg files through miso
--
--------------------------------------------------------------------------------
module HGeometry.Miso.Svg
  ( renderSvgToFile
  , renderAsSvgText, renderAsSvgByteString

  , withAts, withAts'
  , Drawable(..)

  , dPoint
  , dLineSegment
  , dRectangle
  , dCircle
  , dDisk
  , dPolyLine
  , dSimplePolygon
  ) where

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import           HGeometry.Miso.Svg.Writer
import qualified Miso
import qualified System.File.OsPath as File
import           System.OsPath

--------------------------------------------------------------------------------

-- | Given an file path, and a view whose root is an svg element,
-- render the output to the given file.
renderSvgToFile    :: OsPath -> Miso.View action -> IO ()
renderSvgToFile fp = File.writeFile fp . renderAsSvgByteString

-- | Add the doctype
withDocType         :: ByteString.ByteString -> ByteString.ByteString
withDocType content = mconcat
  [ "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
  , "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">"
  , content
  , "</svg>"
  ]


  -- "<svg>"

  --                    "</svg>"
  -- do
  --                       Svg.doctype_
  --                       Svg.with content
  --                            [ Lucid.makeAttribute "xmlns" "http://www.w3.org/2000/svg"
  --                            , Lucid.makeAttribute "xmlns:xlink" "http://www.w3.org/1999/xlink"
  --                            , Svg.version_ "1.1"
  --                            ]

-- | Given an View whose root is an svg element, renders the view to a
-- lazy Text
--
renderAsSvgText :: Miso.View action -> Text.Text
renderAsSvgText = Text.decodeUtf8With Text.strictDecode . renderAsSvgByteString

-- | Given an View whose root is an svg element, renders the view to a
-- lazy ByteString.
--
renderAsSvgByteString :: Miso.View action -> ByteString.ByteString
renderAsSvgByteString = withDocType . Miso.toHtml
