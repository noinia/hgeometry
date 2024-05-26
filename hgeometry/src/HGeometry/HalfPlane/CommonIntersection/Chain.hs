{-# LANGUAGE UndecidableInstances #-}
module HGeometry.HalfPlane.CommonIntersection.Chain
  ( Chain(..)
  , _ChainAlternating
  , bimap'
  , evalChainAt
  , leftMost
  , clipLeft
  , clipRight
  ) where

import           Control.Lens hiding (Empty)
import           Data.Bifunctor (first)
import           Data.Default.Class
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Ord (comparing)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.These
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Line.LowerEnvelope
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Sequence.Alternating
import           HGeometry.Vector

--------------------------------------------------------------------------------


-- | A polygonal chain bounding an unbounded convex region, in CCW order.
newtype Chain f halfPlane r = Chain (Alternating f (Point 2 r) halfPlane)

-- | Iso to convert to just an Alternating
_ChainAlternating :: Iso (Chain f halfPlane r)
                         (Chain f' halfPlane' r')
                         (Alternating f  (Point 2 r)  halfPlane)
                         (Alternating f' (Point 2 r') halfPlane')
_ChainAlternating = coerced

deriving instance ( Show halfPlane, Show (f (Point 2 r, halfPlane))
                  ) => Show (Chain f halfPlane r)

deriving instance ( Eq halfPlane, Eq (f (Point 2 r, halfPlane))
                  ) => Eq (Chain f halfPlane r)

deriving instance ( Ord halfPlane, Ord (f (Point 2 r, halfPlane))
                  ) => Ord (Chain f halfPlane r)

instance Functor f => Functor (Chain f r) where
  fmap f = bimap id f
instance Functor f => Bifunctor (Chain f) where
  bimap f g = bimap' f (over coordinates g)

-- | slightly more general version of bimap so we can easily flip the plane.
bimap'                 :: Functor f
                       => (halfPlane -> halfPlane')
                       -> (Point 2 r -> Point 2 s)
                       -> Chain f halfPlane r -> Chain f halfPlane' s
bimap' f g (Chain alt) = Chain $ bimap g f alt


-- | Evaluates the chain at the given x-coordinate. Returns the value (y-coordinate) y at
-- the given x-coordinate x, and the halfplane containing the point (x,y)
evalChainAt               :: (Num r, Ord r)
                          => r -> Chain Seq (LineEQ r :+ halfPlane) r -> (r :+ halfPlane)
evalChainAt x chain = let chain'   = clipLeft x chain
                          (l :+ h) = chain'^._ChainAlternating.head1
                      in evalAt' x l :+ h

-- | Get the leftmost line in the chain
leftMost :: Chain Seq (line :+ halfPlane) r -> line
leftMost = view (_ChainAlternating.head1.core)


--------------------------------------------------------------------------------

-- | Given a value x, Clip the chain to the interval \((-\infty,x]\)
clipRight      :: Ord r
               => r -> Chain Seq halfPlane r -> Chain Seq halfPlane r
clipRight maxX = clipRightWhen $ \(v, _) -> v^.xCoord >= maxX

-- | Given a value x, Clip the chain to the interval \([x,\infty)\)
clipLeft      :: Ord r
               => r -> Chain Seq halfPlane r -> Chain Seq halfPlane r
clipLeft minX = clipLeftWhen $ \(v, _) -> v^.xCoord <= minX

-- | Clip on the right by a line
clipRightLine       :: (Ord r, Num r)
                    => LineEQ r -> Chain Seq halfPlane r -> Chain Seq halfPlane r
clipRightLine right = clipRightWhen (above right)

-- | Clip the left by a line
clipLeftLine      :: (Ord r, Num r)
                  => LineEQ r -> Chain Seq halfPlane r -> Chain Seq halfPlane r
clipLeftLine left = clipLeftWhen (above left)

-- | Test if the given (Point 2 r) lies above the line
above            :: (Ord r, Num r) => LineEQ r -> (Point 2 r,a) -> Bool
above line (v,_) = (v^.yCoord) >= evalAt' (v^.xCoord) line

-- | Clip the chain on the right
clipRightWhen   :: ((Point 2 r, halfPlane) -> Bool)
                -> Chain Seq halfPlane r -> Chain Seq halfPlane r
clipRightWhen p = over _ChainAlternating $
                 \(Alternating h0 hs) -> Alternating h0 $ Seq.dropWhileR p hs

-- | Clip the chain on the left
clipLeftWhen   :: ((Point 2 r, halfPlane) -> Bool)
               -> Chain Seq halfPlane r -> Chain Seq halfPlane r
clipLeftWhen p = over _ChainAlternating $
                 \alt@(Alternating _ hs) -> case Seq.spanl p hs of
                   (Empty,         _)    -> alt
                   (_ :|> (_,h0'), kept) -> Alternating h0' kept

--------------------------------------------------------------------------------
