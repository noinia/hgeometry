module Algorithms.Geometry.SoS.Homogenious where




-- | Determines the sign of the Determinant, assuming the input is
-- given in homogeneous coordinates.
--
-- pre: the rows in the input vector are given in increasing index
-- order
signDetHom :: (Num r, Eq r, ToTermsHom d) => Matrix d d r -> Sign
signDetHom = signFromTerms . toTermsHom

class ToTermsHom d where
  toTermsHom :: Matrix d d r -> [r]




-- class ToTermsHom 2 where
--   toTerms m@(Matrix (Vector2 i1 i2)
--                     (Vector2 j1 j2)
--             )                     = [ det22 m
--                                     , -i1
--                                     , j2
--                                     ,
--                                     ]


-- toTerms   :: Matrix d d r -> [r]
-- toTerms m = undefined



-------------------------------------------------------------------------------
