--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LPType.LinearProgramming
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Algorithm for solving Low dimensional linear programs.
--
--------------------------------------------------------------------------------
module HGeometry.LPType.LinearProgramming
  ( lp1D, Basis1DLP(..)
  , linearProgrammingMinY
  , Basis2D(..), HalfPlane
  ) where

import           Control.Lens
import           Data.Foldable (toList)
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust, fromMaybe, isJust)
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Set as Set
import qualified Data.Set.NonEmpty as NESet
import qualified Data.Vector as V
import qualified Data.Vector.Generic as Vector
import           Data.Word
import           Debug.Trace
import           GHC.TypeLits
import           HGeometry.Ball
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane
import           HGeometry.Intersection
import           HGeometry.LPType
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Number.Real.Rational
import           HGeometry.Permutation.Shuffle
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Unbounded
import           HGeometry.Vector hiding (basis)
import           Prelude hiding (filter)
import           System.Random
import           VectorBuilder.Builder (foldable)
import           VectorBuilder.Vector (build)
import           Witherable

--------------------------------------------------------------------------------
-- * Linear programming in 1D:

-- | A solution for 1D Linear pgoramming
data Basis1DLP halfSpace = InFeasible1 halfSpace halfSpace
                         | Feasible1 halfSpace
                         | Unbounded
                         deriving (Show,Eq,Functor)

-- | Linear programming in 1D, minimizes the coordinate values.
--
-- \(O(n)\).
lp1D             :: ( Ord r, Foldable1 set
                    , HalfSpace_ halfSpace 1 r
                    , Point_ (BoundingHyperPlane halfSpace 1 r) 1 r
                    ) => set halfSpace -> Basis1DLP halfSpace
lp1D constraints = case foldr f (Bottom,Top) constraints of
    (Bottom,_)             -> Unbounded
    (ValB (Arg x p), negs) -> case negs of
      Top                         -> Feasible1 p
      ValT (Arg x' n) | x <= x'   -> Feasible1 p
                      | otherwise -> InFeasible1 p n
  where
    f h (pos,neg) = let x = h^.boundingHyperPlane.xCoord
                    in case h^.halfSpaceSign of
      Positive -> (ValB (Arg x h) `max` pos, neg)
      Negative -> (pos,                      ValT (Arg x h) `min` neg)
      -- h is bounded by a 1d-hyperplane: i.e. a point

--------------------------------------------------------------------------------
-- * Linear programming in 2D:

-- | A Basis for 2D Linear programming problems.
data Basis2D r halfSpace = EmptyBasis
                         -- ^ The solution must be unbounded
                         | Basis1 r halfSpace
                         -- ^ apparently we have a halfspace perpendicular to the
                         -- optimization direction. So it just has a fixed value r.
                         | Basis2 (Point 2 r) halfSpace halfSpace
                         -- ^ The normal case in which a feasible solution (optimum)
                         | Infeasible (Vector 3 halfSpace)
                         -- ^ Infeasible
                       deriving (Show,Eq,Functor,Foldable)



type HalfPlane r = HalfSpaceF (LineEQ r)

-- | Given a basis, compute the cost associated with the basis
lpCost :: (Eq r, Num r) => Basis2D r halfPlane -> UnBounded r
lpCost = \case
  EmptyBasis   -> MinInfinity
  Basis1 y _   -> Val y
  Basis2 p _ _ -> Val $ p^.yCoord
  Infeasible _ -> MaxInfinity

-- | Let l be some line in R^2, and let h be a halfplane. In general h and l intersect
-- in a halfline. Halfspace1 represents this halfline, as a 1D halfspace on l.
type HalfSpace1 r halfSpace = HalfSpaceF (Point 1 (r,r)) :+ (Point 2 r, halfSpace)

-- | Represents the intersection of a halfplane with some line
data Extend r halfSpace = Infeasible2 halfSpace
                        -- ^ there is no solution
                        | Partial (HalfSpace1 r halfSpace)
                        -- ^ The halfspace
                        deriving (Show)

-- | Collects the 1d halfspaces. Left signifies that we have an infeasible solution
collect :: Foldable f => f (Extend r halfSpace)
        -> Either halfSpace [HalfSpaceF (Point 1 (r,r)) :+ (Point 2 r, halfSpace)]
collect = foldr f (Right []) . toList
  where
    f (Infeasible2 h) _            = Left h
    f _               acc@(Left _) = acc
    f (Partial h)     (Right hs)   = Right (h:hs)


-- | Constructs a 1D halfspace on the bounding line
constructHalfSpaceOn :: (Fractional r, Ord r)
                     => HalfPlane r
                     -- ^ the bounding line of the new halfspace
                     -> HalfPlane r
                     -- ^ the existing halfplane
                     -> Maybe (Extend r (HalfPlane r))
constructHalfSpaceOn h h'
    | not (h `intersects` h') = Just $ Infeasible2 h'
    | otherwise               = case l `intersect` (h'^.boundingHyperPlane) of
        Just (Line_x_Line_Point p) -> Just . Partial $ halfSpace1D p :+ (p,h')
        _                          -> Nothing -- constraint is not useful
  where
    l = h^.boundingHyperPlane
    -- the halfpsace of h' on the bounding line of h
    halfSpace1D (Point2 x y) = HalfSpace newSign (Point1 (y,x))
      where
        x'  = if l^.slope >= 0 then x - 1 else x + 1
        y'  = evalAt (Point1 x') l
        newSign
          | Point2 x' y' `intersects` h' = Negative
          | otherwise                    = Positive
        -- the main idea is to pick a point (x',y') on l that is "better"/lower than the
        -- intersection point p. If this point is contained in h' then h' corresponds to
        -- a 1D halfspace that is bounded from above (i..e by p_y); hence a negative signed
        -- halfspace.



-- | Tries to extend the basis with the given halfplane
lpRecomputeBasis                            :: (Ord r, Fractional r)
                                            => HalfPlane r
                                            -> Basis2D r (HalfPlane r)
                                            -> Maybe ( Basis2D r (HalfPlane r) )
lpRecomputeBasis h@(HalfSpace sign l) basis = case basis of
    Infeasible _         -> Nothing -- already infeasible, so remains infeasible

    Basis2 p h1 h2
        | p `intersects` h -> Nothing -- solution remains the same
        | otherwise        -> Just $ case collect $ mapMaybe (constructHalfSpaceOn h) [h1,h2] of
            Left _            -> infeasible
              -- not sure if we removed anything here.
            Right constraints -> case NonEmpty.nonEmpty constraints of
              Nothing          -> error "absurd: lpExtendBasis. No constraints, so unbounded?"
              Just constraints' -> case lp1D constraints' of
                InFeasible1 _ _         -> infeasible
                Feasible1 (_ :+ (q,h')) -> Basis2 q h h'
                Unbounded               -> error "absurd: lpExtendBasis. unbounded?"
      where
        infeasible = Infeasible (Vector3 h h1 h2)

    Basis1 y h1 -> case l `intersect` (h1^.boundingHyperPlane) of
      Just (Line_x_Line_Line _)                -> Nothing -- not useful yet
      Just (Line_x_Line_Point p)
        | (p .-^ (Vector2 1 0)) `intersects` h -> Nothing
          -- h defines the 1D halfspace (-infty,p_x] on h1. So it's not useful
        | otherwise                            -> Just $ Basis2 p h1 h
      Nothing
        | b > y                                -> Just $ Basis1 b h
                                                   -- h is more restrictive than h1
        | otherwise                            -> Nothing
        where
          b = l^.intercept

    EmptyBasis -> case sign of
      Positive | l^.slope == 0 -> Just $ Basis1 (l^.intercept) h
      _                        -> Nothing -- basis remains empty


-- | Find an initial basis; i.e. some halfplanes that bound the initial solution from below
lpInitialBasis    :: (Foldable set, Ord r, Fractional r)
                  => set (HalfPlane r)
                  -> Basis2D r (HalfPlane r)
lpInitialBasis hs = case List.partition (\h -> h^.boundingHyperPlane.slope >= 0)
                       . filter (\h -> h^.halfSpaceSign == Positive)
                       . toList $ hs
                    of
  (pos:_,neg:_) -> case (pos^.boundingHyperPlane) `intersect` (neg^.boundingHyperPlane) of
                     Just (Line_x_Line_Point p) -> Basis2 p pos neg
                     _                          -> error "lpInitialBasis: absurd"
  ([],_)        -> EmptyBasis
  (poss,[])     -> case filter (\h -> h^.boundingHyperPlane.slope == 0) poss of
                     []      -> EmptyBasis
                     (pos:_) -> Basis1 (pos^.boundingHyperPlane.intercept) pos

-- | minimize the y-coordinate. (Lexicographically)
-- pre: LP is feasible
linearProgrammingMinY :: (Fractional r, Ord r, Foldable set)
                      => LPType (UnBounded r) (Basis2D r) set (HalfPlane r)
linearProgrammingMinY = LPType {
    costFunction           = lpCost
  , combinatorialDimension = 3
  , extendBasis            = lpRecomputeBasis
  , initialBasis           = lpInitialBasis
  }
