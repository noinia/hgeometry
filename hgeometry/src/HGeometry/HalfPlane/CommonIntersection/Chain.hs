{-# LANGUAGE UndecidableInstances #-}
module HGeometry.HalfPlane.CommonIntersection.Chain
  ( Chain(..)
  , _ChainAlternating
  , bimap'
  , evalChainAt
  , leftMost, rightMost
  , clipLeft
  , clipRight
  , glueChains
  , clipLeftLine, clipRightLine
  ) where

import Data.Foldable1
import Data.Coerce (coerce)
import Control.Lens hiding (Empty)
import Data.Sequence (Seq(..))
import Data.Sequence qualified as Seq
import Data.Bifoldable
import HGeometry.Ext
import HGeometry.Line
import HGeometry.Intersection
import HGeometry.Point
import HGeometry.Sequence.Alternating
import HGeometry.Properties
import HGeometry.HalfSpace.Class
import HGeometry.HalfLine
import HGeometry.Vector

--------------------------------------------------------------------------------


-- | A polygonal chain bounding an unbounded convex region, in CCW order.
newtype Chain f r halfPlane = Chain (Alternating f (Point 2 r) halfPlane)
  deriving newtype (Functor,Foldable,Foldable1)

type instance NumType (Chain f r halfPlane) = r

-- | Iso to convert to just an Alternating
_ChainAlternating :: Iso (Chain f  r  halfPlane)
                         (Chain f' r' halfPlane')
                         (Alternating f  (Point 2 r)  halfPlane)
                         (Alternating f' (Point 2 r') halfPlane')
_ChainAlternating = coerced

deriving instance ( Show halfPlane, Show (f (Point 2 r, halfPlane))
                  ) => Show (Chain f r halfPlane)

deriving instance ( Eq halfPlane, Eq (f (Point 2 r, halfPlane))
                  ) => Eq (Chain f r halfPlane)

deriving instance ( Ord halfPlane, Ord (f (Point 2 r, halfPlane))
                  ) => Ord (Chain f r halfPlane)

instance Traversable f => Traversable (Chain f r) where
  traverse f (Chain alt) = Chain <$> traverse f alt

instance Traversable f => Traversable1 (Chain f r) where
  traverse1 f (Chain alt) = Chain <$> traverse1 f alt

instance Functor f => Bifunctor (Chain f) where
  bimap f g = bimap' (over coordinates f) g

instance Foldable f => Bifoldable (Chain f) where
  bifoldMap f g (Chain alt) = bifoldMap (foldMapOf coordinates f) g alt

-- instance Traversable f => Bitraversable (Chain f) where
--   bitraverse f g (Chain alt) = Chain <$> bitraverse (over coordinates f) g alt

-- | slightly more general version of bimap so we can easily flip the plane.
bimap'                 :: Functor f
                       => (Point 2 r -> Point 2 s)
                       -> (halfPlane -> halfPlane')
                       -> Chain f r halfPlane -> Chain f s halfPlane'
bimap' f g (Chain alt) = Chain $ bimap f g alt


-- | Evaluates the chain at the given x-coordinate. Returns the value (y-coordinate) y at
-- the given x-coordinate x, and the halfplane containing the point (x,y)
evalChainAt               :: (Num r, Ord r)
                          => r -> Chain Seq r (LineEQ r :+ halfPlane) -> (r :+ halfPlane)
evalChainAt x chain = let chain'   = clipLeft x chain
                          (l :+ h) = chain'^._ChainAlternating.head1
                      in evalAt' x l :+ h

-- | Get the leftmost line in the chain (i.e. the line bounding the first halfplane)
leftMost :: Lens' (Chain Seq r halfPlane) halfPlane
leftMost = _ChainAlternating.head1

-- | Get the rightmost line in the chain (i.e. the line bounding the last halfplane)
rightMost :: Lens' (Chain Seq r halfPlane) halfPlane
rightMost = _ChainAlternating.last1


unboundedEdges   :: ( HalfPlane_ halfPlane r
                    , Cons (f (Point 2 r, halfPlane)) (f (Point 2 r, halfPlane))
                           (Point 2 r, halfPlane)     (Point 2 r, halfPlane)
                    , Snoc (f (Point 2 r, halfPlane)) (f (Point 2 r, halfPlane))
                           (Point 2 r, halfPlane) (Point 2 r, halfPlane)
                    , HasDirection (BoundingHyperPlane halfPlane 2 r)
                    )
                 => Chain f r halfPlane
                 -> Either (BoundingHyperPlane halfPlane 2 r :+ halfPlane)
                           (Vector 2 (HalfLine (Point 2 r) :+ halfPlane))
unboundedEdges c = case unconsAlt $ c^._ChainAlternating of
  Left h             -> Left $ (h^.boundingHyperPlane) :+ h
  Right ((h,v),rest) -> Right $ case unsnocAlt rest of
      Left h'          -> Vector2 (toHalfLine v h :+ h) (toHalfLine v h' :+ h')
      Right (_,(w,h')) -> Vector2 (toHalfLine v h :+ h) (toHalfLine w h' :+ h')
    where
      toHalfLine p halfPlane = HalfLine p (halfPlane^.boundingHyperPlane.direction)
      -- TODO: somehow report the right vector

leftBoundingLine   :: halfPlane -> LinePV 2 r
leftBoundingLine h = undefined -- h^.boundingLine

--------------------------------------------------------------------------------

-- | Given a value x, Clip the chain to the interval \((-\infty,x]\)
clipRight      :: Ord r
               => r -> Chain Seq r halfPlane -> Chain Seq r halfPlane
clipRight maxX = clipRightWhen $ \(v, _) -> v^.xCoord >= maxX

-- | Given a value x, Clip the chain to the interval \([x,\infty)\)
clipLeft      :: Ord r
               => r -> Chain Seq r halfPlane -> Chain Seq r halfPlane
clipLeft minX = clipLeftWhen $ \(v, _) -> v^.xCoord <= minX

-- | Clip on the right by a line
clipRightLine   :: (Ord r, Num r, HasIntersectionWith (Point 2 r) halfPlane')
                => halfPlane' -> Chain Seq r halfPlane -> Chain Seq r halfPlane
clipRightLine h = clipRightWhen (\(v,_) -> not $ v `intersects` h)

-- | Clip the chain from the left; removing when a vertex is not in the halfplane
clipLeftLine   :: (Ord r, Num r, HasIntersectionWith (Point 2 r) halfPlane')
               => halfPlane' -> Chain Seq r halfPlane -> Chain Seq r halfPlane
clipLeftLine h = clipLeftWhen (\(v,_) -> not $ v `intersects` h)

-- | Clip the chain on the right
clipRightWhen   :: ((Point 2 r, halfPlane) -> Bool)
                -> Chain Seq r halfPlane -> Chain Seq r halfPlane
clipRightWhen p = over _ChainAlternating $
                 \(Alternating h0 hs) -> Alternating h0 $ Seq.dropWhileR p hs

-- | Clip the chain on the left
clipLeftWhen   :: ((Point 2 r, halfPlane) -> Bool)
               -> Chain Seq r halfPlane -> Chain Seq r halfPlane
clipLeftWhen p = over _ChainAlternating $
                 \alt@(Alternating _ hs) -> case Seq.spanl p hs of
                   (Empty,         _)    -> alt
                   (_ :|> (_,h0'), kept) -> Alternating h0' kept

--------------------------------------------------------------------------------
-- | Given two chains; glue them together by introducing the new
-- appropriate intersection point.
--
-- pre: such an intersection point exists (i.e. the rightmost bounding line of the prefix)
-- and the leftmost bounding line of the suffix intersect in a point.
glueChains          :: (Ord r, Fractional r)
                    => Chain Seq r (LineEQ r :+ halfPlane)
                    -> Chain Seq r (LineEQ r :+ halfPlane)
                    -> Chain Seq r (LineEQ r :+ halfPlane)
glueChains pref suf = Chain $ concatAlternatingsWith f (coerce pref) (coerce suf)
  where
    f (r :+ _) (l :+ _) = case r `intersect` l of
      Just (Line_x_Line_Point p)  -> p
      _                           -> error "glueChains: precondition failed!"
