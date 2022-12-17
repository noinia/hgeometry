--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Transformation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Transformation
  ( Transformation(Transformation)
  , transformationMatrix
  , (|.|), identity, inverseOf

  , IsTransformable(..)
  , OptMatrix_
  , ConstructableMatrix_

  , translation, scaling, uniformScaling

  , translateBy, scaleBy, scaleUniformlyBy

  , rotateTo

  , skewX, rotation, reflection, reflectionV, reflectionH

  -- , fitToBox
  -- , fitToBoxTransform
  ) where

import           Control.Lens
import           Data.Ext
-- import           HGeometry.Box.Internal (Rectangle, IsBoxable)
import           HGeometry.Box.Class
import           HGeometry.Box.Boxable
import           HGeometry.Interval
import qualified HGeometry.Box.Class as Box
import           HGeometry.Properties
import           HGeometry.Point
import           HGeometry.Transformation.Internal
import           HGeometry.Vector
import           Data.Semigroup

--------------------------------------------------------------------------------

{-

-- | Given a rectangle r and a geometry g with its boundingbox,
-- transform the g to fit r.
fitToBox     :: forall g r rectangle.
                ( IsTransformable g
                , IsBoxable g
                , NumType g ~ r, Dimension g ~ 2
                , Ord r, Fractional r
                , Rectangle_ rectangle r
                ) => rectangle -> g -> g
fitToBox r g = transformBy (fitToBoxTransform r g) g

-}

-- | Given a rectangle r and a geometry g with its boundingbox,
-- compute a transformation can fit g to r.
fitToBoxTransform     :: forall g r rectangle point.
                         ( IsTransformable g
                         , IsBoxable g
                         , NumType g ~ r, Dimension g ~ 2
                         , Ord r, Fractional r
                         , Rectangle_ rectangle point
                         , Point_ point 2 r
                         , TransformationConstraints g
                         , OptCVector_ 2 r
                         , OptCVector_ 2 (ClosedInterval r)
                         , OptVector_ 3 r
                         -- , OptAdditive_ 3 r
                         , OptMetric_ 2 r
                         , HasComponents (Vector 2 (ClosedInterval r)) (Vector 2 r)
                         -- , HasComponents (Vector)
                         ) => rectangle -> g -> Transformation 2 r
fitToBoxTransform r g = translation v2 |.| uniformScaling lam |.| translation v1
  where
    b = boundingBox g
    v1  :: Vector 2 r
    v1  = negated $ b^.minPoint.vector
    v2  = r^.minPoint.vector
    lam = minimum1Of components' $ liftI2 (/) (Box.size r) (Box.size b)



      -- (/) <$> Box.size r <*> Box.size b
