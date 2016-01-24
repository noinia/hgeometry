module Util where

import Data.Vinyl
import Data.Geometry.Ipe
import Data.Proxy
import Data.Ext
import Data.Function(on)
import qualified Data.List as L

byStrokeColour :: (Stroke ∈ AttributesOf g) => [IpeObject' g r] -> [[IpeObject' g r]]
byStrokeColour = map (map fst) . L.groupBy ((==) `on` snd) . L.sortOn snd
               . map (\x -> (x,lookup' x))
  where
    lookup' (_ :+ ats) = lookupAttr (Proxy :: Proxy Stroke) ats
