module Algorithms.Geometry.SoS.Point
  ( toSymbolic
  , toSoSRational
  ) where

import Algorithms.Geometry.SoS.Index
import Algorithms.Geometry.SoS.Symbolic
import Control.Lens
import Geometry.Point.Class
import Geometry.Point.Internal
import Geometry.Vector

--------------------------------------------------------------------------------


-- | Given an input point, transform its number type to include
-- symbolic $\varepsilon$ expressions so that we can use SoS.
toSymbolic    :: (Arity d, ToAPoint point d r, HasSoSIndex point)
              => point -> Point d (Symbolic SoSI r)
toSymbolic p' = let p = p'^.toPoint
                    i = sosIndex p'
                in p&vector %~ imap (\j x -> symbolic x $ MkSoS i j)

-- | Constructs an point whose numeric type uses SoSRational, so that
-- we can use SoS.
toSoSRational :: (Arity d, ToAPoint point d r, HasSoSIndex point, Eq r, Num r)
              => point -> Point d (SoSRational SoSI r)
toSoSRational = fmap (\x -> sosRational x 1) . toSymbolic

-- instance (ToAPoint point d r, Arity d, HasSoSIndex point)
--          => ToAPoint (WithSoS point) d (Symbolic SoSI r) where
--   toPoint = to undefined -- toSymbolic


-- FIXME: I guess the indices should be strictly positive, otherwise some term eps(0) would add 1?
