{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE TemplateHaskell  #-}
module Data.Geometry.PolyLine where

import           Control.Lens
import qualified Data.Foldable as F

import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Vector(Arity)

import qualified Data.Sequence as S

--------------------------------------------------------------------------------

newtype PolyLine d pe r = PolyLine { _unPolyLine :: S.Seq (Point d r :+ pe) }
makeLenses ''PolyLine

deriving instance (Show r, Show pe, Arity d) => Show (PolyLine d pe r)
deriving instance (Eq r, Eq pe, Arity d)     => Eq (PolyLine d pe r)
deriving instance (Ord r, Ord pe, Arity d)   => Ord (PolyLine d pe r)
deriving instance Arity d                    => Functor (PolyLine d pe)
type instance Dimension (PolyLine d pe r) = d
type instance NumType   (PolyLine d pe r) = r

instance HasPoints (PolyLine d pe r) where
  points = F.toList . fmap _core . _unPolyLine

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



-- lineSegments :: [LineSegment d r pe :+ le]
