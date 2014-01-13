{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Data.Geometry.Curve where

import Data.Geometry.Point
import Data.Vinyl
import Data.Vinyl.Unicode

import GHC.TypeLits



startPoint :: "start" ::: Point d r p
startPoint = Field :: "start" ::: Point d r p

endPoint :: "end" ::: Point d r p
endPoint = Field :: "end" ::: Point d r p


class (HasDimension c, HasNumType c) => HasParametrization c where
  atTime :: (d ~ Dimension c, r ~ NumType c) =>
            c -> r -> Point d r (Fields d r)


data LineSegment (d :: Nat) (r :: *) (fields :: [*]) where
  LineSegment :: ( ("start" ::: Point d r p) ∈ fields
                 , ("stop" ::: Point d r p) ∈ fields
                 ) => PlainRec fields -> LineSegment d r fields
