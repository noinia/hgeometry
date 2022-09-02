{-# LANGUAGE UndecidableInstances #-}
module Algorithms.Geometry.SoS.Internal where

import Algorithms.Geometry.SoS.Symbolic
import Control.Lens
import Geometry.Point.Class
import Geometry.Point.Internal
import Geometry.Vector
import Data.Ord
import Data.Proxy
import GHC.TypeLits

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------


instance (ToAPoint point d r, Arity d)
        => ToAPoint (WithSoS point) d (Symbolic SoSIndex r) where
  toPoint = to (\(WithSoS i p) -> let Point v = p^.toPoint
                                      d       = fromIntegral $ natVal (Proxy @d)
                                  in Point $ imap (\j -> WithSoS (i*d+j)) v
               )
  {-# INLINE toPoint #-}




--------------------------------------------------------------------------------

-- | Given an input point, transform its number type to include
-- symbolic $\varepsilon$ expressions so that we can use SoS.
toSymbolic          :: (Ord i, Arity d)
                    => Point d r :+ i -> Point d (Symbolic (i,Int) r)
toSymbolic (p :+ i) = p&vector %~ imap (\j x -> symbolic x (i,j))
