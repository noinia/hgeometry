{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Box.Internal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Axis alligned boxes in d-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.Box.Internal
  ( Box(Box,Rectangle)
  , Rectangle
  ) where

import Control.Lens
import Data.Zip
import GHC.Generics (Generic)
import HGeometry.Box.Class
import HGeometry.Interval
import HGeometry.Point
import HGeometry.Properties (NumType,Dimension)
import HGeometry.Vector
import Prelude hiding (zipWith)
import Text.Read

--------------------------------------------------------------------------------
-- | D-dimensional boxes.
newtype Box point = MkBox (Vector 2 point)
  deriving stock (Generic)
  deriving newtype (Eq,Ord)

-- | Construct a box
pattern Box           :: point -> point -> Box point
pattern Box minP maxP = MkBox (Vector2 minP maxP)
{-# COMPLETE Box #-}

-- | Defines a rectangle
type Rectangle = Box
-- TODO this type is slightly misleading

-- | Construct a Rectangle
--
--
pattern Rectangle           :: Dimension point ~ 2
                            => point -> point -> Box point
pattern Rectangle minP maxP = Box minP maxP
{-# COMPLETE Rectangle #-}
{-# INLINE Rectangle #-}

-- type instance PointFor  (Box point) = point
type instance Dimension (Box point) = Dimension point
type instance NumType   (Box point) = NumType point

-- instance Constrained Box where
--   type Dom Box point = OptCVector_ 2 point
instance Functor Box where
  fmap f (Box p q) = Box (f p) (f q)
--   -- cmap f (MkBox v) = MkBox $ v&components %~ f
instance Foldable Box where
  foldMap f (Box p q) = f p <> f q
instance Traversable Box where
  traverse f (Box p q) = Box <$> f p <*> f q

instance HasMinPoint (Box point) point where
  minPoint = lens (\(Box p _) -> p) (\(Box _ q) p -> Box p q)

instance HasMaxPoint (Box point) point where
  maxPoint = lens (\(Box _ q) -> q) (\(Box p _) q -> Box p q)

instance HasPoints (Box point) (Box point') point point' where
  allPoints f (MkBox v) = MkBox <$> components f v

instance ( Point_ point d r
         , Zip (Vector d)
         ) => Box_ (Box point) point where
  extent (Box p q) = zipWith ClosedInterval (p^.vector) (q^.vector)


instance (Show point) => Show (Box point) where
  showsPrec k (Box p q) = showParen (k > appPrec) $
                              showString "Box "
                            . showsPrec (appPrec+1) p
                            . showChar ' '
                            . showsPrec (appPrec+1) q

appPrec :: Int
appPrec = 10

instance (Read point) => Read (Box point) where
  readPrec = parens (prec appPrec $ do
                          Ident "Box" <- lexP
                          p <- step readPrec
                          q <- step readPrec
                          return (Box p q))

--------------------------------------------------------------------------------
