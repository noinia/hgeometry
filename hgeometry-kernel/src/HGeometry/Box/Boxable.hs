{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Box.Boxable
  ( IsBoxable(..)
  ) where

import Control.Lens
import Data.Ext
import Data.Functor.Contravariant (phantom)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Semigroup.Foldable
import HGeometry.Box.Class
import HGeometry.Box.Optimal
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector
--------------------------------------------------------------------------------

-- foo'     :: ( IxValue vector ~ Bool
--             , Additive_ vector
--             , Eq (IxValue vector)
--             ) => vector -> vector -> Bool
-- foo' u v = andOf components $ liftU2 (==) u v

-- | Types for which we can compute an axis parallel boundingbox
class IsBoxable g where
  -- | Compute the axis-parallel boundingbox of the given geometry.
  boundingBox :: ( d ~ Dimension g, r ~ NumType g
                 , Ord r
                 ) => g -> Box (Point d r)
  default boundingBox  :: ( d ~ Dimension g, r ~ NumType g
                          , Ord r
                          , HasPoints' g point
                          , Point_ point d r
                          , OptVector_ d r, OptMetric_ d r
                          , Ord (VectorFamily' d r)
                          )
                       => g -> Box (Point d r)
  boundingBox = defaultBBox

-- | default implementation of boundingBox
defaultBBox  :: forall g point d r. ( d ~ Dimension g, r ~ NumType g
                , HasPoints' g point
                , Point_ point d r
                , OptVector_ d r
                , OptMetric_ d r
                , Ord (VectorFamily' d r)
                )
              => g -> Box (Point d r)
defaultBBox g = Box bl tr
    where
      thePoints :: Fold1 g (Point d r)
      thePoints = folding1 (fmap pointFromPoint . toNonEmptyOf allPoints)
      bl = minimum1Of thePoints g
      tr = maximum1Of thePoints g

-- | construct a Fold1 from a function that produces a Foldable1
folding1         :: Foldable1 f => (s -> f a) -> Fold1 s a
folding1 sfa agb = phantom . traverse1_ agb . sfa
{-# INLINE folding1 #-}

  -- $ \g -> g^..allPoints

--------------------------------------------------------------------------------
-- Instances

instance ( Box_ (Box point) point
         , Point_ point d r
         , OptVector_ d r
         , OptCVector_ 2 point
         , OptCVector_ 2 (Point d r)
         , OptMetric_ d r
         ) => IsBoxable (Box point) where
  boundingBox (Box p q) = Box (pointFromPoint p) (pointFromPoint q)

instance IsBoxable g => IsBoxable (g :+ extra) where
  boundingBox = boundingBox . view core

----------------------------------------
newtype Union point = Union { unUnion :: Box point }

instance (OptCVector_ 2 point, Point_ point d r, Ord r) => Semigroup (Union point) where
  (Union (Box p q)) <> (Union (Box p' q')) = Union $ Box (f min p p') (f max q q')
    where
      f combine a b = b&vector %~ liftI2 combine (a^.vector)


instance (IsBoxable g, OptVector_ d r, OptMetric_ d r
         , d ~ Dimension g, r ~ NumType g
         ) => IsBoxable (NonEmpty g) where
  boundingBox = unUnion . foldMap1 (Union . boundingBox)


type instance Dimension (NonEmpty g) = Dimension g
type instance NumType   (NonEmpty g) = NumType g
-- TODO: move these to hgeometry-combinatorial

----------------------------------------
