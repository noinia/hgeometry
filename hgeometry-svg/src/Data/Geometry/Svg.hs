module Data.Geometry.Svg( module Data.Geometry.Svg.Writer
                        , svgViaIpe
                        ) where

import           Data.Geometry.Ipe.IpeOut
import           Data.Geometry.Svg.Writer
import qualified Text.Blaze.Svg as Svg
import           Data.Geometry.Ipe.Types(ToObject(..))

-- $setup
-- >>> import Data.Geometry.Point


-- | Convert an input into svg via ipe
--
-- >>> printSvgXMLElem $ svgViaIpe defIO (origin :: Point 2 Rational)
-- <circle cx="0.000000000000" cy="0.000000000000" r="5.000000000000" />
svgViaIpe      :: (ToObject i, Real r) => IpeOut g i r -> g -> Svg.Svg
svgViaIpe mkIO = svgO . iO . mkIO
