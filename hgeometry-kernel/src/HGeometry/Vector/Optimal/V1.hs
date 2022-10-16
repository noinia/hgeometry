module HGeometry.Vector.Optimal.V1
  ( V1(Vector1)
  , HasV1(..)
  ) where

import Control.Lens
import Data.Coerce
import GHC.Generics(Generic)
import HGeometry.Properties
import HGeometry.Vector.Class
import System.Random (Random (..))
import System.Random.Stateful (UniformRange(..), Uniform(..))

--------------------------------------------------------------------------------

newtype V1 r = Vector1 r
  deriving (Show,Eq,Ord,Generic,Functor,Foldable,Traversable
           -- ,Uniform,UniformRange
           )

instance Field1 (V1 r) (V1 s) r s where
  _1 = lens coerce (\_ x -> coerce x)

class Field1 (V1 r) (V1 r) r r => HasV1 r where
  mkV1 :: r -> V1 r
  mkV1 = Vector1

--------------------------------------------------------------------------------

type instance Dimension (V1 r) = 1
type instance NumType (V1 r)   = r
type instance IxValue (V1 r)   = r
type instance Index   (V1 r)   = Int

instance TraversableWithIndex Int V1
instance FoldableWithIndex Int V1
instance FunctorWithIndex Int V1


instance Ixed (V1 r) where
  ix i f v@(Vector1 x) = case i of
                           0 -> Vector1 <$> f x
                           _ -> pure v

instance HasComponents (V1 r) (V1 s) where
  components = itraversed

instance Vector_ (V1 r) 1 r where
  vectorFromList = \case
    [x] -> Just $ coerce x
    _   -> Nothing
  {-# INLINE vectorFromList #-}
