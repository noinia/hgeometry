module Geometry.Svg( module Geometry.Svg.Writer
                        , svgViaIpe
                        ) where

import           Ipe.IpeOut
import           Geometry.Svg.Writer
import qualified Text.Blaze.Svg as Svg
import           Ipe.Types(ToObject(..))

-- $setup
-- >>> import Geometry.Point


-- | Convert an input into svg via ipe
--
-- >>> printSvgXMLElem $ svgViaIpe defIO (origin :: Point 2 Rational)
-- <circle cx="0.000000000000" cy="0.000000000000" r="5.000000000000" />
svgViaIpe      :: (ToObject i, Real r) => IpeOut g i r -> g -> Svg.Svg
svgViaIpe mkIO = svgO . iO . mkIO
