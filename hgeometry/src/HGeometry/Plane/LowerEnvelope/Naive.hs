{-# LANGUAGE UndecidableInstances #-}
module HGeometry.Plane.LowerEnvelope.Naive
  ( lowerEnvelope
  , lowerEnvelopeWith
  ) where

--------------------------------------------------------------------------------

import           Control.Lens
import           Data.Either (partitionEithers)
import qualified Data.Foldable as F
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import           HGeometry.Ext
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.NonVertical
import           HGeometry.Line
import           HGeometry.Line.General
import qualified HGeometry.Line.LowerEnvelope as Line
import           HGeometry.Plane.LowerEnvelope.Connected
import           HGeometry.Plane.LowerEnvelope.Type
import           HGeometry.Sequence.Alternating (firstWithNeighbors)
import           HGeometry.Vector
--------------------------------------------------------------------------------

-- | Brute force implementation that computes the lower envelope, by explicitly
-- considering every triple of planes.
--
-- pre: the input forms a *set* of planes, i.e. no duplicates
--
--
-- running time: \(O(n^4 )\)
lowerEnvelope :: ( Plane_ plane r
                 , Ord r, Fractional r, Foldable1 f, Functor f, Ord plane
                 , Show plane, Show r
                 ) => f plane -> LowerEnvelope plane
lowerEnvelope = lowerEnvelopeWith bruteForceLowerEnvelope

--------------------------------------------------------------------------------

-- | Computes the lower envelope of the planes; possibly using the given function to
-- construct the minimization diagram.
--
-- In particular, we distinguish whether the planes define a connected lower envelope
-- (in that case, we actually call the algorithm on it). Or that the planes are degenerate
-- and are all parallel and thus define parallel strips. In that case, we use a two dimensional
-- lower envelope algorithm to compute the lower envelope of the (parallel) planes.
--
-- \(O(T(n) + n \log n)\).
lowerEnvelopeWith                        :: ( Plane_ plane r
                                            , Ord r, Fractional r, Foldable1 nonEmpty, Ord plane
                                          )
                                         => (NonEmpty plane -> MinimizationDiagram r plane)
                                         -> nonEmpty plane -> LowerEnvelope plane
lowerEnvelopeWith minimizationDiagram hs = case distinguish (toNonEmpty hs) of
    Left lines'                   -> ParallelStrips . fromLines $ lines'
    Right (Vector3 h1 h2 h3, hs') -> ConnectedEnvelope $ minimizationDiagram (h1 :| h2 : h3 : hs')
  where
    fromLines = firstWithNeighbors (\h _ h' -> fromMaybe err $ intersectionLine h h')
              . fmap (view extra) . view Line._Alternating
              . Line.lowerEnvelope
    err = error "lowerEnvelopeWith. absurd: neighbouring planes must intersect"


-- | We distinguish whether the planes are all parallel (left), and thus actually just
-- define a 2D lower envelope of lines, or that the planes actually have a connected lower
-- envelope. In that case, we return three planes that certify that the lower envelope
-- will be connected, as well as the rest of the planes.
--
-- (Note that those three planes that we return do not necesarily define a vertex of the
-- envelope).
--
-- running time: \(O(n)\)
distinguish                :: ( Plane_ plane r, Eq r, Fractional r
                              )
                           => NonEmpty plane -> Either (NonEmpty (LineEQ r :+ plane))
                                                       (Vector 3 plane, [plane])
distinguish hs@(h :| rest) = case findDifferent lines' of
    Left baseLine                -> Left $ fmap (flip asLine baseLine) hs
    Right (Vector2 h2 h3, rest') -> Right (Vector3 h h2 h3, rest')

  where
    lines' = map (\h' -> (perpendicularTo' <$> intersectionLine h h') :+ h') rest
    -- for each plane, compute (the downward projection of) its line of intersection with
    -- with h. We actually all rotate these lines by 90 degrees.
    --
    -- if these lines are all parallel, then our input was degenerate. We pick one of
    -- those lines baseLine, and compute the intersection of the plane h with the vertical
    -- wall through this

perpendicularTo' :: (Fractional r, Eq r) => VerticalOrLineEQ r -> VerticalOrLineEQ r
perpendicularTo' = \case
  VerticalLineThrough x    -> NonVertical $ LineEQ 0 x
  NonVertical (LineEQ 0 b) -> VerticalLineThrough b
  NonVertical (LineEQ a b) -> NonVertical $ LineEQ (1/a) b

-- | Given the planes tagged with their intersection line with some plane h0, returns
-- either a Left baseLine if all these lines are parallel, or two planes that have
-- non-paralel intersection lines (as well as the rest of the planes).
--
-- the baseLine is one of the parallel lines (or vertical if there are no lines to begin with).
findDifferent    :: (Eq r, Num r
                    )
                 => [Maybe (VerticalOrLineEQ r) :+ plane] -> Either (VerticalOrLineEQ r)
                                                                    (Vector 2 plane, [plane])
findDifferent xs = case partitionEithers $ map distinguishParallel xs of
    (_,[])                  -> Left $ VerticalLineThrough 0  -- all planes happen to be paralell
    (pars0, (l :+ h) : rest) -> case List.span (isParallelTo l) rest of
        (_,[])                     -> Left l -- degnerate; planes are all parralel
        (pars1, (_ :+ h') : rest') -> Right ( Vector2 h h'
                                            , pars0 <> map (view extra) (pars1 <> rest')
                                            )
  where
    distinguishParallel (m :+ h) = case m of
      Nothing -> Left h
      Just l  -> Right (l :+ h)

-- | given a plane h, and a line l, erect a vertical plane through l, and return the line
-- in which h intersects this vertical plane.
asLine                  :: (Plane_ plane r, Num r)
                        => plane -> VerticalOrLineEQ r -> LineEQ r :+ plane
asLine h@(Plane_ a b c) = \case
  VerticalLineThrough x      -> LineEQ b          (a*x + c)  :+ h
  NonVertical (LineEQ a' b') -> LineEQ (a + a'*b) (b*b' + c) :+ h
  -- we simply fill in the coordinates of the line into the equation for h.  this gives us
  -- line (either parameterized in terms of the y-coordinate, or in case of the
  -- x-coordinate of R^3 in the second case).
