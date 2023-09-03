{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
-- --------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Transformation.Internal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Transformation.Internal where

import           Control.Lens (iso,set,Iso,over,iover)
import           GHC.TypeLits
import           HGeometry.Matrix
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.Point
-- >>> import HGeometry.Vector

--------------------------------------------------------------------------------
-- * Transformations

-- | A type representing a Transformation for d dimensional objects
newtype Transformation d r = Transformation { _transformationMatrix :: Matrix (d + 1) (d + 1) r }


-- | Transformations and Matrices are isomorphic.
transformationMatrix :: Iso (Transformation d r)       (Transformation d       s)
                            (Matrix (d + 1) (d + 1) r) (Matrix (d + 1) (d + 1) s)
transformationMatrix = iso _transformationMatrix Transformation

deriving stock   instance (Show (Matrix (d+1) (d+1) r)) => Show (Transformation d r)
deriving newtype instance (Eq (Matrix (d+1) (d+1) r))   => Eq (Transformation d r)
deriving newtype instance (Ord (Matrix (d+1) (d+1) r))  => Ord (Transformation d r)



type instance NumType (Transformation d r) = r

-- | Compose transformations (right to left)
(|.|)                                     :: (Num r, OptMatrix_ (d+1) r)
                                          => Transformation d r -> Transformation d r
                                          -> Transformation d r
(Transformation f) |.| (Transformation g) = Transformation $ f !*! g
{-# INLINE (|.|) #-}

-- | Identity transformation; i.e. the transformation which does not change anything.
identity :: (Num r, OptMatrix_ (d+1) r) => Transformation d r
identity = Transformation identityMatrix


instance (Num r, OptMatrix_ (d+1) r) => Semigroup (Transformation d r) where
  (<>) = (|.|)
  {-# INLINE (<>) #-}
instance (Num r, OptMatrix_ (d+1) r) => Monoid (Transformation d r) where
  mempty = identity

-- if it exists?

-- | Compute the inverse transformation
--
-- >>> inverseOf $ translation (Vector2 (10.0) (5.0))
-- Transformation {_transformationMatrix = Matrix (Vector3 (Vector3 1.0 0.0 (-10.0)) (Vector3 0.0 1.0 (-5.0)) (Vector3 0.0 0.0 1.0))}
inverseOf :: (Fractional r, OptMatrix_ (d+1) r, Invertible (d + 1))
          => Transformation d r -> Transformation d r
inverseOf = Transformation . inverseMatrix . _transformationMatrix
{-# INLINE inverseOf #-}

--------------------------------------------------------------------------------
-- * Transformable geometry objects

-- | Some constraints that we will pretty much need to transform a d
-- dimensional object whose numtype is r
type TransformationConstraints d r =
  ( KnownNat d
  , Has_ Vector_ d r
  , OptMatrix_ (d+1) r
  , HasComponents (Vector (d+1) r)
                  (Vector (d+1) (Vector (d+1) r))
  , Has_ Additive_ (d+1) r
  )

-- | Bunch of constraints we need for the default implementation of transformBy
type DefaultTransformByConstraints g d r =
  ( d ~ Dimension g, r ~ NumType g
  , HasPoints g g (Point d r) (Point d r)
  , OptMatrix_ (d+1) r
  , Fractional r
  , Has_ Additive_ d r
  , HasComponents (Vector (1 + d) (Vector (1 + d) r)) (Vector (1 + d) r)
  )

-- | A class representing types that can be transformed using a transformation
class IsTransformable g where
  transformBy :: Transformation (Dimension g) (NumType g) -> g -> g

  default transformBy :: forall d r. ( DefaultTransformByConstraints g d r )
                      => Transformation (Dimension g) (NumType g) -> g -> g
  transformBy t = over (allPoints @g @g @(Point d r) @(Point d r))
                       (transformBy t)
  {-# INLINE transformBy #-}

instance (Fractional r, Has_ Vector_ d r, OptMatrix_ (d+1) r
         , HasComponents (Vector (1 + d) (Vector (1 + d) r)) (Vector (1 + d) r)
         ) => IsTransformable (Point d r) where
  transformBy t = Point . transformBy t . toVec
  {-# INLINE transformBy #-}

instance ( Fractional r
         , Has_ Vector_ d r
         , OptMatrix_ (d+1) r
         , HasComponents (Vector (1 + d) (Vector (1 + d) r)) (Vector (1 + d) r)
         -- , d < d+1
         ) => IsTransformable (Vector d r) where
  transformBy (Transformation m) v = f $ m !* (snoc v 1 :: Vector (d+1) r)
    where
      f   :: Vector (d+1) r -> Vector d r
      f u = let (u',x) = unsnoc u
            in u' ^/ x
  {-# INLINE transformBy #-}

--------------------------------------------------------------------------------
-- * Common transformations

-- | Create translation transformation from a vector.
--
-- >>> transformBy (translation $ Vector2 1 2) $ Point2 2 3
-- Point2 3.0 5.0
translation   :: forall d r vector. ( Num r
                                    , Vector_ vector d r
                                    , TransformationConstraints d r
                                    )
              => vector -> Transformation d r
translation v = Transformation . Matrix
             $ iover components transRow (snoc v 1 :: Vector (d+1) r)
{-# INLINE translation #-}

-- class CFunctorWithIndex


-- type TransformationConstraints d r =
--   ( KnownNat d
--   , OptMatrix_ d r
--   , HasComponents (Vector (d+1) r)
--                   (Vector (d+1) (Vector (d+1) r))
--   , OptAdditive_ d r
--   , d < d + 1 -- TODO: get rid of this constraint somehow
--   )


-- | Create scaling transformation from a vector.
--
-- >>> transformBy (scaling $ Vector2 2 (-1)) $ Point2 2 3
-- Point2 4.0 (-3.0)
scaling   :: forall d r vector. ( Num r
             , Vector_ vector d r
             , TransformationConstraints d r
             )
          => vector -> Transformation d r
scaling v = Transformation . Matrix
          $ iover components mkRow (snoc v 1 :: Vector (d+1) r)
{-# INLINE scaling #-}

-- | Create scaling transformation from a scalar that is applied
--   to all dimensions.
--
-- >>> transformBy (uniformScaling 5) $ Point2 2 3
-- Point2 10.0 15.0
-- >>> uniformScaling 5 == scaling (Vector2 5 5)
-- True
-- >>> uniformScaling 5 == scaling (Vector3 5 5 5)
-- True
uniformScaling   :: forall d r. ( Num r, TransformationConstraints d r
                                ) => r -> Transformation d r
uniformScaling x = scaling $ generate @(Vector d r) (const x)
{-# INLINE uniformScaling #-}


-- test :: Point 2 Double
-- test = transformBy (uniformScaling 5) $ Point2 2 3

-- test2 :: Point 2 Double
-- test2 = transformBy (scaling $ Vector2 2 5) $ Point2 2 3





--------------------------------------------------------------------------------
-- * Functions that execute transformations

-- | Translate a given point.
--
-- >>> translateBy (Vector2 1 2) $ Point2 2 3
-- Point2 3.0 5.0
translateBy :: ( IsTransformable g
               , Num (NumType g)
               , Vector_ vector (Dimension g) (NumType g)
               , TransformationConstraints (Dimension g) (NumType g)
               ) => vector -> g -> g
translateBy = transformBy . translation
{-# INLINE translateBy #-}

-- | Scale a given point.
--
-- >>> scaleBy (Vector2 2 (-1)) $ Point2 2 3
-- Point2 4.0 (-3.0)
scaleBy :: ( IsTransformable g, Num (NumType g)
           , Vector_ vector (Dimension g) (NumType g)
           , TransformationConstraints (Dimension g) (NumType g)
           ) => vector -> g -> g
scaleBy = transformBy . scaling
{-# INLINE scaleBy #-}


-- | Scale a given point uniformly in all dimensions.
--
-- >>> scaleUniformlyBy 5 $ Point2 2 3
-- Point2 10.0 15.0
scaleUniformlyBy :: ( IsTransformable g, Num (NumType g)
                    , TransformationConstraints (Dimension g) (NumType g)
                    ) => NumType g -> g -> g
scaleUniformlyBy = transformBy  . uniformScaling
{-# INLINE scaleUniformlyBy #-}


-- | Row in a translation matrix
transRow     :: forall n r . ( Num r
                             , Has_ Additive_ (n+1) r
                             )
             => Int -> r -> Vector (n + 1) r
transRow i x = set (component @n) x $ mkRow i 1

-- | Creates a row with zeroes everywhere, except at position i, where the
-- value is the supplied value.
mkRow     :: forall d r. (Num r, Has_ Vector_ d r) => Int -> r -> Vector d r
mkRow i x = generate $ \j -> if i == j then x else 0

--------------------------------------------------------------------------------
-- * 3D Rotations

-- | Given three new unit-length basis vectors (u,v,w) that map to (x,y,z),
-- construct the appropriate rotation that does this.
--
--
rotateTo                 :: ( Num r
                            ) => Vector 3 (Vector 3 r) -> Transformation 3 r
rotateTo (Vector3 u v w) = Transformation . Matrix $ Vector4 (snoc u        0)
                                                             (snoc v        0)
                                                             (snoc w        0)
                                                             (Vector4 0 0 0 1)
{-# INLINE rotateTo #-}

--------------------------------------------------------------------------------
-- * 2D Transformations

-- | Skew transformation that keeps the y-coordinates fixed and shifts
-- the x coordinates.
skewX        :: ( Num r
                ) => r -> Transformation 2 r
skewX lambda = Transformation . Matrix $ Vector3 (Vector3 1 lambda 0)
                                                 (Vector3 0 1      0)
                                                 (Vector3 0 0      1)


-- | Create a matrix that corresponds to a rotation by 'a' radians counter-clockwise
--   around the origin.
rotation   :: (Floating r) => r -> Transformation 2 r
rotation a = Transformation . Matrix $ Vector3 (Vector3 (cos a) (- sin a) 0)
                                               (Vector3 (sin a) (  cos a) 0)
                                               (Vector3 0       0         1)

-- | Create a matrix that corresponds to a reflection in a line through the origin
--   which makes an angle of 'a' radians with the positive 'x'-asis, in counter-clockwise
--   orientation.
reflection   :: ( Floating r
                ) => r -> Transformation 2 r
reflection a = rotation a |.| reflectionV |.| rotation (-a)

-- | Vertical reflection
reflectionV :: (Num r) => Transformation 2 r
reflectionV = Transformation . Matrix $ Vector3 (Vector3 1   0  0)
                                                (Vector3 0 (-1) 0)
                                                (Vector3 0   0  1)

-- | Horizontal reflection
reflectionH :: (Num r) => Transformation 2 r
reflectionH = Transformation . Matrix $ Vector3 (Vector3 (-1) 0  0)
                                                (Vector3   0  1  0)
                                                (Vector3   0  0  1)
