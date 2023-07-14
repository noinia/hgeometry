{-# LANGUAGE UndecidableInstances #-}
module HGeometry.LowerEnvelope.Naive
  ( lowerEnvelopeVertexForm
  -- , lowerEnvelope
  -- , triangulatedLowerEnvelope

  , asVertex
  , belowAll
  , intersectionPoint
  ) where

--------------------------------------------------------------------------------

import           Control.Lens
import           Control.Monad (guard)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           HGeometry.Combinatorial.Util
import           HGeometry.Foldable.Sort
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.Line.LineEQ
import           HGeometry.LowerEnvelope.VertexForm
import           HGeometry.Point
import           HGeometry.Properties
import           Hiraffe.Graph

--------------------------------------------------------------------------------
-- * Computing a lower envelope in vertex form

-- | Brute force implementation that computes the vertex form of the
-- lower envelope, by explicitly considering every triple of planes.
--
-- running time: \(O(n^4 )\)
lowerEnvelopeVertexForm    :: ( Plane_ plane r
                              , Ord r, Fractional r, Foldable f, Ord plane
                              ) => f plane -> VertexForm plane
lowerEnvelopeVertexForm hs = foldMap (\t -> case asVertex hs t of
                                              Nothing -> mempty
                                              Just v  -> singleton v
                                     ) $ uniqueTriplets hs

-- | Given all planes, and a triple, computes if the triple defines a
-- vertex of the lower envelope, and if so returns it.
asVertex                       :: (Plane_ plane r, Foldable f, Ord plane, Ord r, Fractional r)
                               => f plane -> Three plane -> Maybe (LEVertex plane)
asVertex hs t@(Three h1 h2 h3) = do v <- intersectionPoint t
                                    guard (v `belowAll` hs)
                                    pure $ LEVertex v (Set.fromList [h1,h2,h3])


-- | test if v lies below (or on) all the planes in hs
belowAll      :: (Plane_ plane r, Ord r, Num r, Foldable f) => Point 3 r -> f plane -> Bool
belowAll v hs = all (\h -> onSideTest v h /= GT) hs
{-# INLINE belowAll #-}

-- | Computes there the three planes intersect
intersectionPoint :: ( Plane_ plane r, Ord r, Fractional r) => Three plane -> Maybe (Point 3 r)
intersectionPoint (Three h1@(Plane_ a1 b1 c1)
                         (Plane_ a2 b2 c2)
                         (Plane_ a3 b3 c3))
  | a1 == a2  = Nothing -- FIXME: this can't be right
  | otherwise = do y <- my
                   x <- xf y
                   pure $ Point3 x y (z x y)
  where
    xf y' = (b2-b1)*y' + c2 - c1
            ///
            a1 - a2

    my = c3 - c1 - (a1-a3)*(c2-c1)/(a1-a2)
         ///
         (a1-a3)*(b2-b1)/(a1-a2) + b1 - b3

    z x' y' = evalAt (Point2 x' y') h1

infixl 0 ///
-- | safe division testing for zero
(///)         :: (Eq a, Fractional a) => a -> a -> Maybe a
num /// denom = if denom /= 0 then Just (num / denom) else Nothing






  -- asVertex
  -- catMaybes [ (\v -> Vertex v (definers h1 h2 h))) <$> intersectionPoint h1 h2 h3
  --           | Three h1 h2 h3 <- uniqueTriplets hs
  --           ]






{-
-- | simple implementation of the lower envelope.
--
-- running time: \(O(n^4 )\)
lowerEnvelope    :: ( Plane_ plane r
                    , Ord r, Fractional r, Foldable f, Ord plane
                    ) => f plane -> LowerEnvelope r
lowerEnvelope hs = undefined

triangulatedLowerEnvelope    :: ( Plane_ plane r
                                , Ord r, Fractional r, Foldable f
                                ) => f plane -> LowerEnvelope r
triangulatedLowerEnvelope hs = undefined

-}
--------------------------------------------------------------------------------

{-

-- TODO: attach the two defining halfplanes to the result

-- | Given two halfplanes h and h' computes the halfplane where h lies
-- vertically below h'.
liesBelowIn                                :: (Plane_ plane r, Ord r, Fractional r)
                                           => plane -> plane -> Maybe (HalfPlane r)
liesBelowIn (Plane_ a b c) (Plane_ a' b' c') = case b `compare` b' of
                                                 LT -> Just $ Above (LineEQ d e)
                                                 GT -> Just $ Below (LineEQ d e)
                                                 EQ -> case a `compare` a' of
                                                         LT -> Just $ RightOf f
                                                         GT -> Just $ LeftOf f
                                                         EQ -> Nothing
  where
    d = (a-a') / (b - b')
    e = (c-c') / (b - b')
    f = (c-c') / (a - a')

-}
