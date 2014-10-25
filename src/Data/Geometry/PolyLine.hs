{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.PolyLine where

import Data.Geometry.Vector(Arity)
import Data.Geometry.Point
import Data.Ext

import qualified Data.Sequence as S

newtype PolyLine d pe r = PolyLine { unPolyLine :: S.Seq (Point d r :+ pe) }

deriving instance (Show r, Show pe, Arity d) => Show (PolyLine d pe r)
deriving instance (Eq r, Eq pe, Arity d)     => Eq (PolyLine d pe r)
deriving instance (Ord r, Ord pe, Arity d)   => Ord (PolyLine d pe r)
deriving instance Arity d                    => Functor (PolyLine d pe)

data LineSegment d pe r = LineSegment { _start :: Point d r :+ pe
                                      , _end   :: Point d r :+ pe
                                      }

deriving instance (Show r, Show pe, Arity d) => Show (LineSegment d pe r)
deriving instance (Eq r, Eq pe, Arity d)     => Eq (LineSegment d pe r)
deriving instance (Ord r, Ord pe, Arity d)   => Ord (LineSegment d pe r)
deriving instance Arity d                    => Functor (LineSegment d pe)



-- lineSegments :: [LineSegment d r pe :+ le]
