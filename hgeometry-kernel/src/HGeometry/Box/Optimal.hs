{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Box.Optimal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Axis alligned boxes in d-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.Box.Optimal
  ( Box(Box,Rectangle)
  , Rectangle
  ) where

import Control.Lens
import GHC.Generics hiding (prec)
import HGeometry.Box.Class
import HGeometry.Intersection
import HGeometry.Interval
import HGeometry.Line.LineEQ
-- import HGeometry.LineSegment
import HGeometry.Point
import HGeometry.Properties (NumType,Dimension)
import HGeometry.Vector
import Text.Read

import Data.Coerce
-- import Data.Functor.Apply

--------------------------------------------------------------------------------
-- | D-dimensional boxes.
newtype Box point = MkBox (Vector 2 point) deriving (Generic)

-- | Construct a box
pattern Box           :: OptCVector_ 2 point => point -> point -> Box point
pattern Box minP maxP = MkBox (Vector2 minP maxP)
{-# COMPLETE Box #-}

-- | Defines a rectangle
type Rectangle = Box
-- TODO this type is slightly misleading

-- | Construct a Rectangle
pattern Rectangle           :: (OptCVector_ 2 point, Dimension point ~ 2)
                            => point -> point -> Box point
pattern Rectangle minP maxP = Box minP maxP
{-# COMPLETE Rectangle #-}


-- type instance PointFor  (Box point) = point
type instance Dimension (Box point) = Dimension point
type instance NumType   (Box point) = NumType point

instance OptCVector_ 2 point => HasMinPoint (Box point) point where
  minPoint = lens (\(Box p _) -> p) (\(Box _ q) p -> Box p q)

instance OptCVector_ 2 point => HasMaxPoint (Box point) point where
  maxPoint = lens (\(Box _ q) -> q) (\(Box p _) q -> Box p q)

instance ( Affine_ point
         , Point_ point (Dimension point) (NumType point)
         , OptCVector_ 2 (NumType point)
         , OptCVector_ 2 point
         ) => Box_ (Box point) point where
  extent (Box p q) = vZipWith ClosedInterval (p^.vector) (q^.vector)


instance ( Show point, OptCVector_ 2 point) => Show (Box point) where
  showsPrec k (Box p q) = showParen (k > appPrec) $
                              showString "Box "
                            . showsPrec (appPrec+1) p
                            . showChar ' '
                            . showsPrec (appPrec+1) q

appPrec :: Int
appPrec = 10

instance (Read point, OptCVector_ 2 point) => Read (Box point) where
  readPrec = parens (prec appPrec $ do
                          Ident "Box" <- lexP
                          p <- step readPrec
                          q <- step readPrec
                          return (Box p q))

--------------------------------------------------------------------------------
{-
data LineBoxIntersection d r = Line_x_Box_Point (Point d r)
                             | Line_x_Box_Segment (ClosedLineSegment (Point d r))
                             deriving (Show,Eq)

type instance Intersection (LineEQ r) (Rectangle r) = Maybe (LineBoxIntersection 2 r)

-- instance HasIntersection (LineEQ r) (Rectangle r) where
--   (LineEQ a b) `intersects` (Rectangle p q) =
-}

--------------------------------------------------------------------------------

myRect :: Rectangle (Point 2 Double)
myRect = Rectangle origin (Point2 10 20.0)

test = size myRect
