{-# LANGUAGE UndecidableInstances #-}
module HGeometry.LowerEnvelope.Naive
  ( lowerEnvelopeVertexForm
  , VertexForm
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
import           HGeometry.Intersection
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

-- | the intersecting line may be vertical or non-vertical
data IntersectingLine r = Vertical r
                        | NonVertical (LineEQ r)
                        deriving (Show,Eq)

-- | Given two planes, computes the line in which they intersect.
intersectingLine :: (Plane_ plane r, Fractional r, Eq r)
                 => plane -> plane -> Maybe (IntersectingLine r)
intersectingLine (Plane_ a1 b1 c1) (Plane_ a2 b2 c2)
    | b1 /= b2  = Just $ NonVertical $ LineEQ ((a2 - a1) / diffB) ((c2 - c1) / diffB)
                  -- the two planes intersect in some normal line
    | a1 /= a2  = Just $ Vertical ((c2 -c1) / (a1 - a2))
                  -- the planes intersect in a vertical line
    | otherwise = Nothing
                  -- the planes don't intersect at all
  where
    diffB = b1 - b2

-- | Computes there the three planes intersect
intersectionPoint                                    :: ( Plane_ plane r, Ord r, Fractional r)
                                                     => Three plane -> Maybe (Point 3 r)
intersectionPoint (Three h1@(Plane_ a1 b1 c1) h2 h3) =
    do l12 <- intersectingLine h1 h2
       l13 <- intersectingLine h1 h3
       case (l12,l13) of
         (Vertical _x12, Vertical _x13) -> Nothing
           -- if the xes are the same they would be the same plane even
         (Vertical x, NonVertical l)    -> vertNonVertIntersect x l
         (NonVertical l, Vertical x)    -> vertNonVertIntersect x l
         (NonVertical l, NonVertical m) -> l `intersect` m >>= \case
           Line_x_Line_Point (Point2 x y) -> Just $ Point3 x y (a1 * x + b1* y + c1)
           Line_x_Line_Line _             -> Nothing
   where
     vertNonVertIntersect x l = let y = evalAt' x l
                                    z = a1 * x + b1* y + c1
                                in Just $ Point3 x y z


{-
-- | simple implementation of the lower envelope.
--
-- running time: \(O(n^4 )\)
lowerEnvelope    :: ( Plane_ plane r
                    , Ord r, Fractional r, Foldable f, Ord plane
                    ) => f plane -> LowerEnvelope plane
lowerEnvelope hs = undefined

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
