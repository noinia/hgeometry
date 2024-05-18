{-# LANGUAGE UndecidableInstances #-}
module HGeometry.HalfPlane.CommonIntersection
  ( CommonIntersection(..)
  , Chain(..)
  , commonIntersection
  , lowerBoundary
  -- , LowerBoundary(..)
  ) where

import           Control.Lens
import           Data.Default.Class
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import           Data.Ord (comparing)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           Data.These
import           HGeometry.Ext
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane.Class
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Line.LowerEnvelope
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Sequence.Alternating
import           HGeometry.Vector

--------------------------------------------------------------------------------

-- | Common intersection of a bunch of halfplanes
data CommonIntersection halfPlane r =
    EmptyIntersection
  | Bounded (ConvexPolygon (Point 2 r :+ halfPlane))
  | Slab halfPlane () -- TODO needs a boundingLLine
    -- ^ each vertex stores the interior halfplane of the CCW-edge it is incident to.
  | Unbounded (Chain Seq halfPlane r)
  deriving (Show,Eq)

-- | A polygonal chain bounding an unbounded convex region, in CCW order.
newtype Chain f halfPlane r = Chain (Alternating f (Point 2 r) halfPlane)

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

--------------------------------------------------------------------------------

-- | Computes the common intersection of a \(n\) halfplanes.
--
-- running time: \(O(n\log n)\)
commonIntersection     :: ( Foldable1 f, Functor f
                          , HalfPlane_ halfPlane r
                          , Fractional r, Ord r

                          , Default (LineEQ r :+ halfPlane), Default halfPlane -- FIXME
                          )
                       => f halfPlane -> CommonIntersection halfPlane r
commonIntersection hs0 = case partitionEithersNE . fmap classifyHalfPlane $ toNonEmpty hs0 of
    This onlyBelows     -> Unbounded $ upperBoundary onlyBelows
    That onlyAboves     -> Unbounded $ lowerBoundary onlyAboves
    These belows aboves -> let bb = lowerBoundary aboves
                               ub = upperBoundary belows
                           in undefined -- somehow combine them
  where
    classifyHalfPlane h = case h^.halfSpaceSign of
                            Negative -> Left  h
                            Positive -> Right h

--------------------------------------------------------------------------------

-- | Given the bounding lines of a bunch of halfplanes that are all bounded from below,
-- i.e. all halfplanes have positive sign, computes their common intersection.
--
-- running time: O(n\log n)
lowerBoundary :: forall f halfPlane r.
                 ( HalfPlane_ halfPlane r
                 , Foldable1 f, Functor f, Fractional r, Ord r
                 , Default (LineEQ r :+ halfPlane), Default halfPlane -- FIXME
                 )
              => f halfPlane -> Chain Seq halfPlane r
lowerBoundary = undefined

  -- bimap' flipY (over yCoord negate) . lowerBoundary . fmap flipY
  --               -- by flipping the plane, and using the existing lowerBoundary machinery

  --               -- TODO: should we also mirror the plane in x?
  -- where
  --   flipY :: halfPlane -> halfPlane
  --   flipY = over (boundingHyperPlane.hyperPlaneCoefficients.traverse) negate




-- -- | Given the bounding lines of a bunch of halfplanes that are all bounded from above,
-- -- computes their common intersection.
-- --
-- -- running time: O(n\log n)
-- upperBoundary :: ( NonVerticalHyperPlane_ boundingLine 2 r
--                  , Fractional r, Ord r, Foldable f, Functor f
--                  )
--               => f boundingLine -> LowerBoundary (LowerChain boundingLine r)
-- upperBoundary =



--------------------------------------------------------------------------------

-- | Given the bounding lines of a bunch of halfplanes that are all bounded from above,
-- i.e. all halfplanes have negative sign, computes their common intersection.
--
-- running time: O(n\log n)
upperBoundary     :: ( HalfPlane_ halfPlane r
                     , Foldable1 f, Fractional r, Ord r
                     , Default (LineEQ r :+ halfPlane), Default halfPlane -- FIXME
                     )
                  => f halfPlane -> Chain Seq halfPlane r
upperBoundary hs0 = Chain $ case partitionEithersNE . fmap classifyHalfPlane $ toNonEmpty hs0 of
    This onlyVerticals           -> let (_ :+ h) = leftMostPlane onlyVerticals
                                    in Alternating h mempty
    That onlyNonVerticals        -> let env = view extra <$> lowerEnvelope' onlyNonVerticals
                                    in env^._Alternating
    These verticals nonVerticals -> let (maxX :+ h) = leftMostPlane verticals
                                        env         = clipRight maxX $ lowerEnvelope' nonVerticals
                                        -- we clip the env at the leftmost vertical plane, throwing
                                        -- away any vertices whose x-coord is at most maxX
                                        alt         = snocElemWith (intersectVertical maxX)
                                                                   (env^._Alternating)
                                                                   (undefined :+ h)
                                        -- we snoc the new element onto the alternating list.
                                        -- we use undefined to create a dummy non-vertical
                                        -- line (that we will next immediately) throw away
                                        -- anyway
                                    in view extra <$> alt
  where
    classifyHalfPlane h = case h^.boundingHyperPlane.to asGeneralLine of
      VerticalLineThrough x -> Left  (x :+ h)
      NonVertical l         -> Right (l :+ h)

    asGeneralLine = hyperPlaneFromEquation . hyperPlaneEquation

    -- finds the leftMost vertical halfplane
    leftMostPlane = minimumBy (comparing (^.core))

    lowerEnvelope' = over _Alternating (mapF Seq.fromList) . lowerEnvelope

    -- to construct the separator we simply evaluate the bounding line at the rightmost
    -- position.
    intersectVertical x (l' :+ _) _ = Point2 x (evalAt' x l')

clipRight      :: (Ord r, Point_ vertex 2 r)
               => r -> LowerEnvelopeF Seq vertex line -> LowerEnvelopeF Seq vertex line
clipRight maxX = over _Alternating $
                 \(Alternating h0 hs) -> Alternating h0 $ Seq.dropWhileR clip hs
  where
    clip (v, _) = v^.xCoord >= maxX


--------------------------------------------------------------------------------
