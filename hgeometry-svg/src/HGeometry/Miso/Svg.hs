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
import qualified Data.Text.Lazy as Text
import           HGeometry.Miso.Svg.Writer
import qualified Lucid
import qualified Miso
import qualified System.File.OsPath as File
import           System.OsPath

--------------------------------------------------------------------------------

-- | Render the output to the given file.
renderSvgToFile    :: OsPath -> Miso.View action -> IO ()
renderSvgToFile fp = File.writeFile fp . renderAsSvgByteString

-- | Render the output to a lazy text
renderAsSvgText :: Miso.View action -> Text.Text
renderAsSvgText = Lucid.renderText . Lucid.toHtml

-- | Render the otuput of a view to a lazy bytestring
renderAsSvgByteString :: Miso.View action -> ByteString.ByteString
renderAsSvgByteString = Lucid.renderBS . Lucid.toHtml
