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


startPoint :: "start" ::: Point p r
startPoint = Field :: "start" ::: Point p r

endPoint :: "end" ::: Point p r
endPoint = Field :: "end" ::: Point p r


class (HasDimension c, HasNumType c) => HasParametrization c where
  atTime :: c ->  NumType c -> Point (Dimension c)
