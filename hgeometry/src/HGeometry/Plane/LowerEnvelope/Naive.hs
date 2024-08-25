{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Plane.LowerEnvelope.Naive
  ( lowerEnvelope
  , lowerEnvelopeVertexForm
  -- , triangulatedLowerEnvelope

  , asVertex
  , belowAll
  ) where

--------------------------------------------------------------------------------

import           Control.Monad (guard)
import           Data.Foldable1
import qualified Data.Set as Set
import           HGeometry.Combinatorial.Util
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Plane.LowerEnvelope.AdjListForm (LowerEnvelope, fromVertexForm)
import           HGeometry.Plane.LowerEnvelope.VertexForm
import           HGeometry.Point

--------------------------------------------------------------------------------

-- | Brute force implementation that computes the lower envelope, by explicitly
-- considering every triple of planes.
--
-- pre: the input forms a *set* of planes, i.e. no duplicates
--
--
-- running time: \(O(n^4 )\)
lowerEnvelope    :: ( Plane_ plane r
                    , Ord r, Fractional r, Foldable1 f, Functor f, Ord plane
                    , Show plane, Show r
                    ) => f plane -> LowerEnvelope plane
lowerEnvelope hs = fromVertexForm hs $ lowerEnvelopeVertexForm hs

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- * Computing a lower envelope in vertex form

-- | Brute force implementation that computes the vertex form of the
-- lower envelope, by explicitly considering every triple of planes.
--
-- pre: the input forms a *set* of planes, i.e. no duplicates
--
--
-- general position assumptions: None. Though keep in mind that
-- e.g. if all planes are parallel there are no vertices.
--
--
--
-- running time: \(O(n^4 )\)
lowerEnvelopeVertexForm    :: ( Plane_ plane r
                              , Ord r, Fractional r, Foldable f, Ord plane
                              ) => f plane -> VertexForm plane
lowerEnvelopeVertexForm hs = foldMap (maybe mempty singleton . asVertex hs) $ uniqueTriplets hs

-- | Given all planes, and a triple, computes if the triple defines a
-- vertex of the lower envelope, and if so returns it.
asVertex                       :: (Plane_ plane r, Foldable f, Ord plane, Ord r, Fractional r)
                               => f plane -> Three plane -> Maybe (LEVertex plane)
asVertex hs t@(Three h1 h2 h3) = do v <- intersectionPoint t
                                    guard (v `belowAll` hs)
                                    pure $ LEVertex v (Set.fromList [h1,h2,h3])

-- | test if v lies below (or on) all the planes in hs
belowAll   :: (Plane_ plane r, Ord r, Num r, Foldable f) => Point 3 r -> f plane -> Bool
belowAll v = all (\h -> verticalSideTest v h /= GT)
{-# INLINE belowAll #-}




{-

triangulatedLowerEnvelope    :: ( Plane_ plane r
                                , Ord r, Fractional r, Foldable f
                                ) => f plane -> LowerEnvelope plane
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
