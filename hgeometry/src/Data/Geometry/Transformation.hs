{-# LANGUAGE Unsafe #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Transformation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module Data.Geometry.Transformation where

import           Control.Lens (iso,set,Iso,imap)
import           Data.Geometry.Matrix
import           Data.Geometry.Matrix.Internal (mkRow)
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import qualified Data.Geometry.Vector as V
import           Data.Proxy
import           GHC.TypeLits

{- $setup
>>> import Data.Geometry.LineSegment
>>> import Data.Ext
-}

--------------------------------------------------------------------------------
-- * Transformations

-- | A type representing a Transformation for d dimensional objects
newtype Transformation d r = Transformation { _transformationMatrix :: Matrix (d + 1) (d + 1) r }

-- | Transformations and Matrices are isomorphic.
transformationMatrix :: Iso (Transformation d r)       (Transformation d       s)
                            (Matrix (d + 1) (d + 1) r) (Matrix (d + 1) (d + 1) s)
transformationMatrix = iso _transformationMatrix Transformation

deriving instance (Show r, Arity (d + 1)) => Show (Transformation d r)
deriving instance (Eq r, Arity (d + 1))   => Eq (Transformation d r)
deriving instance (Ord r, Arity (d + 1))  => Ord (Transformation d r)
deriving instance Arity (d + 1)           => Functor (Transformation d)
deriving instance Arity (d + 1)           => Foldable (Transformation d)
deriving instance Arity (d + 1)           => Traversable (Transformation d)

type instance NumType (Transformation d r) = r

-- | Compose transformations (right to left)
(|.|) :: (Num r, Arity (d + 1)) => Transformation d r -> Transformation d r -> Transformation d r
(Transformation f) |.| (Transformation g) = Transformation $ f `multM` g


-- if it exists?

-- | Compute the inverse transformation
--
-- >>> inverseOf $ translation (Vector2 (10.0) (5.0))
-- Transformation {_transformationMatrix = Matrix Vector3 [Vector3 [1.0,0.0,-10.0],Vector3 [0.0,1.0,-5.0],Vector3 [0.0,0.0,1.0]]}
inverseOf :: (Fractional r, Invertible (d + 1) r)
          => Transformation d r -> Transformation d r
inverseOf = Transformation . inverse' . _transformationMatrix

--------------------------------------------------------------------------------
-- * Transformable geometry objects

-- | A class representing types that can be transformed using a transformation
class IsTransformable g where
  transformBy :: Transformation (Dimension g) (NumType g) -> g -> g

-- | Apply a transformation to a collection of objects.
--
-- >>> transformAllBy (uniformScaling 2) [Point1 1, Point1 2, Point1 3]
-- [Point1 [2.0],Point1 [4.0],Point1 [6.0]]
transformAllBy :: (Functor c, IsTransformable g)
               => Transformation (Dimension g) (NumType g) -> c g -> c g
transformAllBy t = fmap (transformBy t)

-- | Apply transformation to a PointFunctor, ie something that contains
--   points. Polygons, triangles, line segments, etc, are all PointFunctors.
--
-- >>> transformPointFunctor (uniformScaling 2) $ OpenLineSegment (Point1 1 :+ ()) (Point1 2 :+ ())
-- LineSegment (Open (Point1 [2.0] :+ ())) (Open (Point1 [4.0] :+ ()))
transformPointFunctor   :: ( PointFunctor g, Fractional r, d ~ Dimension (g r)
                           , Arity d, Arity (d + 1)
                           ) => Transformation d r -> g r -> g r
transformPointFunctor t = pmap (transformBy t)

instance (Fractional r, Arity d, Arity (d + 1))
         => IsTransformable (Point d r) where
  transformBy t = Point . transformBy t . toVec

instance (Fractional r, Arity d, Arity (d + 1))
         => IsTransformable (Vector d r) where
  transformBy (Transformation m) v = f $ m `mult` snoc v 1
    where
      f u   = (/ V.last u) <$> V.init u


--------------------------------------------------------------------------------
-- * Common transformations

-- | Create translation transformation from a vector.
--
-- >>> transformBy (translation $ Vector2 1 2) $ Point2 2 3
-- Point2 [3.0,5.0]
translation   :: (Num r, Arity d, Arity (d + 1))
              => Vector d r -> Transformation d r
translation v = Transformation . Matrix $ imap transRow (snoc v 1)

-- | Create scaling transformation from a vector.
--
-- >>> transformBy (scaling $ Vector2 2 (-1)) $ Point2 2 3
-- Point2 [4.0,-3.0]
scaling   :: (Num r, Arity d, Arity (d + 1))
          => Vector d r -> Transformation d r
scaling v = Transformation . Matrix $ imap mkRow (snoc v 1)

-- | Create scaling transformation from a scalar that is applied
--   to all dimensions.
--
-- >>> transformBy (uniformScaling 5) $ Point2 2 3
-- Point2 [10.0,15.0]
-- >>> uniformScaling 5 == scaling (Vector2 5 5)
-- True
-- >>> uniformScaling 5 == scaling (Vector3 5 5 5)
-- True
uniformScaling :: (Num r, Arity d, Arity (d + 1)) => r -> Transformation d r
uniformScaling = scaling . pure


--------------------------------------------------------------------------------
-- * Functions that execute transformations

-- | Translate a given point.
--
-- >>> translateBy (Vector2 1 2) $ Point2 2 3
-- Point2 [3.0,5.0]
translateBy :: ( IsTransformable g, Num (NumType g)
               , Arity (Dimension g), Arity (Dimension g + 1)
               ) => Vector (Dimension g) (NumType g) -> g -> g
translateBy = transformBy . translation

-- | Scale a given point.
--
-- >>> scaleBy (Vector2 2 (-1)) $ Point2 2 3
-- Point2 [4.0,-3.0]
scaleBy :: ( IsTransformable g, Num (NumType g)
           , Arity (Dimension g), Arity (Dimension g + 1)
           ) => Vector (Dimension g) (NumType g) -> g -> g
scaleBy = transformBy . scaling


-- | Scale a given point uniformly in all dimensions.
--
-- >>> scaleUniformlyBy 5 $ Point2 2 3
-- Point2 [10.0,15.0]
scaleUniformlyBy :: ( IsTransformable g, Num (NumType g)
                    , Arity (Dimension g), Arity (Dimension g + 1)
                    ) => NumType g -> g -> g
scaleUniformlyBy = transformBy  . uniformScaling


-- | Row in a translation matrix
-- transRow     :: forall n r. ( Arity n, Arity (n- 1), ((n - 1) + 1) ~ n
--                             , Num r) => Int -> r -> Vector n r
-- transRow i x = set (V.element (Proxy :: Proxy (n-1))) x $ mkRow i 1

transRow     :: forall n r. (Arity n, Arity (n + 1), Num r)
             => Int -> r -> Vector (n + 1) r
transRow i x = set (V.element (Proxy :: Proxy n)) x $ mkRow i 1

--------------------------------------------------------------------------------
-- * 3D Rotations

-- | Given three new unit-length basis vectors (u,v,w) that map to (x,y,z),
-- construct the appropriate rotation that does this.
--
--
rotateTo                 :: Num r => Vector 3 (Vector 3 r) -> Transformation 3 r
rotateTo (Vector3 u v w) = Transformation . Matrix $ Vector4 (snoc u        0)
                                                             (snoc v        0)
                                                             (snoc w        0)
                                                             (Vector4 0 0 0 1)

--------------------------------------------------------------------------------
-- * 2D Transformations

-- | Skew transformation that keeps the y-coordinates fixed and shifts
-- the x coordinates.
skewX        :: Num r => r -> Transformation 2 r
skewX lambda = Transformation . Matrix $ Vector3 (Vector3 1 lambda 0)
                                                 (Vector3 0 1      0)
                                                 (Vector3 0 0      1)
