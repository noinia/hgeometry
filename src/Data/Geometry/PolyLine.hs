{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Data.Geometry.PolyLine where

import           Control.Applicative
import           Control.Lens
import qualified Data.Foldable as F

import           Data.Monoid

import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Transformation
import           Data.Geometry.Box
import           Data.Geometry.Properties
import           Data.Geometry.Vector(Arity, AlwaysTrueDestruct)

-- import           Data.Sequence((<|))
import qualified Data.Sequence as S

import           GHC.TypeLits
--------------------------------------------------------------------------------

newtype PolyLine d p r = PolyLine { _unPolyLine :: S.Seq (Point d r :+ p) }
makeLenses ''PolyLine

deriving instance (Show r, Show p, Arity d) => Show    (PolyLine d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq      (PolyLine d p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord     (PolyLine d p r)
deriving instance Arity d                   => Functor (PolyLine d p)
type instance Dimension (PolyLine d p r) = d
type instance NumType   (PolyLine d p r) = r

instance HasPoints (PolyLine d p r) where
  points = F.toList . fmap _core . _unPolyLine

instance Arity d => IsBoxable (PolyLine d p r) where
  boundingBox = boundingBoxList . points

instance (Num r, AlwaysTruePFT d) => IsTransformable (PolyLine d p r) where
  transformBy = transformPointFunctor

instance PointFunctor (PolyLine d p) where
  pmap f = over unPolyLine (fmap (fmap f))

fromPoints :: (Monoid p, F.Foldable f) => f (Point 2 r) -> PolyLine 2 p r
fromPoints = PolyLine . F.foldr (\p s -> (p :+ mempty) <| s) S.empty

--------------------------------------------------------------------------------

data LineSegment d p r = LineSegment { _start :: Point d r :+ p
                                     , _end   :: Point d r :+ p
                                     }
makeLenses ''LineSegment

deriving instance (Show r, Show p, Arity d) => Show (LineSegment d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq (LineSegment d p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord (LineSegment d p r)
deriving instance Arity d                   => Functor (LineSegment d p)
type instance Dimension (LineSegment d p r) = d
type instance NumType   (LineSegment d p r) = r

instance HasPoints (LineSegment d p r) where
  points l = [l^.start.core, l^.end.core]

instance PointFunctor (LineSegment d p) where
  pmap f (LineSegment s e) = LineSegment (f <$> s) (f <$> e)

instance Arity d => IsBoxable (LineSegment d p r) where
  boundingBox = boundingBoxList . points

instance (Num r, AlwaysTrueTFP d) => IsTransformable (LineSegment d p r) where
  transformBy = transformPointFunctor
