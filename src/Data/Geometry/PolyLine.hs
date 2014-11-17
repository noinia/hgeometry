{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Data.Geometry.PolyLine where

import           Control.Lens
import qualified Data.Foldable as F

import           Data.Monoid

import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Box
import           Data.Geometry.Properties
import           Data.Geometry.Vector(Arity)

-- import           Data.Sequence((<|))
import qualified Data.Sequence as S

--------------------------------------------------------------------------------

newtype PolyLine d pe r = PolyLine { _unPolyLine :: S.Seq (Point d r :+ pe) }
makeLenses ''PolyLine

deriving instance (Show r, Show pe, Arity d) => Show    (PolyLine d pe r)
deriving instance (Eq r, Eq pe, Arity d)     => Eq      (PolyLine d pe r)
deriving instance (Ord r, Ord pe, Arity d)   => Ord     (PolyLine d pe r)
deriving instance Arity d                    => Functor (PolyLine d pe)
type instance Dimension (PolyLine d pe r) = d
type instance NumType   (PolyLine d pe r) = r

instance HasPoints (PolyLine d pe r) where
  points = F.toList . fmap _core . _unPolyLine

instance Arity d => IsBoxable (PolyLine d pe r) where
  boundingBox = boundingBoxList . points

--------------------------------------------------------------------------------

data LineSegment d pe r = LineSegment { _start :: Point d r :+ pe
                                      , _end   :: Point d r :+ pe
                                      }
makeLenses ''LineSegment

deriving instance (Show r, Show pe, Arity d) => Show (LineSegment d pe r)
deriving instance (Eq r, Eq pe, Arity d)     => Eq (LineSegment d pe r)
deriving instance (Ord r, Ord pe, Arity d)   => Ord (LineSegment d pe r)
deriving instance Arity d                    => Functor (LineSegment d pe)
type instance Dimension (LineSegment d pe r) = d
type instance NumType   (LineSegment d pe r) = r

instance HasPoints (LineSegment d pe r) where
  points l = [l^.start.core, l^.end.core]

fromPoints :: (Monoid pe, F.Foldable f) => f (Point 2 r) -> PolyLine 2 pe r
fromPoints = PolyLine . F.foldr (\p s -> (p :+ mempty) <| s) S.empty

instance Arity d => IsBoxable (LineSegment d pe r) where
  boundingBox = boundingBoxList . points

-- lineSegments :: [LineSegment d r pe :+ le]
