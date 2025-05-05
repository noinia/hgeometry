--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.LPType
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implements Clarkson's expected linear time algorithm for solving LPType problems.
-- (E.g. linear  programming, smallest enclosing ball, etc.)
--
--------------------------------------------------------------------------------
module HGeometry.LPType
  ( LPType(..)
  , clarkson
  , clarkson2
  , subExp
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
import           HGeometry.Combinatorial.Util
import           HGeometry.Disk
import qualified HGeometry.Disk.Smallest.Naive as Naive
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane
import           HGeometry.Intersection
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

-- | A Description of an LPType problem
data LPType t basis set a = LPType
                            { costFunction :: basis a -> t
                            -- ^ the function we are trying to minimize
                            , combinatorialDimension :: Int
                             -- ^ the combinatorial dimension of the problem
                            , extendBasis :: a
                                           -> basis a
                                           -> Maybe (basis a)
                             -- ^ function to extend the current basis into a new basis.
                             -- it returns the new basis (if it has changed) and Nothing
                             -- otherwise.

                            , initialBasis :: set a -> basis a
                             -- ^ function to construct some initial basis
                             -- the input may be large.
                            }

-- | Solves the LP Type problem. In particular, a minimization problem.
--
-- returns the cost of an optimal solution (if it exists), and the subset of elements
-- realizing it
--
-- expected: O(n) time.
clarkson            :: ( Foldable set, Monoid (set a), Vector.Vector set a
                       , Foldable basis, Ord t, Eq a, SplitGen gen
                       )
                    => gen
                    -- ^ random generator
                    -> LPType t basis set a
                    -- ^ an LP-type problem
                    -> set a
                    -> (t, basis a)
clarkson gen0 problem@(LPType v dim extendBasis _) hs
    | n <= 9* (dim*dim) = withOpt v $ clarkson2 gen0 problem hs
    | otherwise         = withOpt v $ step gen0 mempty
  where
    n      = length hs
    sqrtN' = sqrt . fromIntegral $ n
    sqrtN  = floor sqrtN'
    r      = floor $ fromIntegral dim * sqrtN'

    step gen partial = case Vector.filter (isViolated basis) hs of
        vs | null vs              -> basis  -- no more violated constraints, so we found opt.
           | length vs <= 2*sqrtN -> step gen2 (vs <> partial) -- extend and continue
           | otherwise            -> step gen2 partial -- too many violated constraints, retry
      where
        basis       = clarkson2 gen1 problem (partial <> sample gen r hs)
        (gen1,gen2) = splitGen gen

    isViolated basis h = isJust $ extendBasis h basis

-- | Take a sample of size r uniformly at random.
--
-- O(n)
--
-- TODO: make sure this is lazy enough to actually get O(r) time instead!
sample          :: ( Foldable f, Vector.Vector vector a, RandomGen gen)
                => gen -> Int -> f a -> vector a
sample gen r hs = Vector.take r $ shuffle gen hs


-- type Weight = Int

-- data Weighted a = Weighted {-# UNPACK #-}!Weight a
--                   deriving (Show,Eq)

-- -- | Get the total weight
-- totalWeight :: Foldable f => f (Weighted a) -> Weight
-- totalWeight = getSum . foldMap' (\(Weighted w _) -> Sum w)


-- -- | double the weight of some element.
-- doubleWeight                :: Weighted a -> Weighted a
-- doubleWeight (Weighted w x) = Weighted (2*w) x


-- | The clarkson2 algorithm.
clarkson2      :: ( Foldable set, Semigroup (set a), Vector.Vector set a
                  , Foldable basis, Ord t, Eq a, SplitGen gen
                  )
               => gen
                  -- ^ random generator
                -> LPType t basis set a
                -- ^ an LP-type problem
                -> set a
                -> basis a
clarkson2 gen0 problem@(LPType _ dim extendBasis _) hs
    | n <= r    = subExp gen0 problem hs
    | otherwise = step gen0 hs
  where
    n = length hs
    r = 6* (dim*dim)

    treshold hs' = length hs' `div` 3*dim

    step gen hs' = case violated basis hs' of
        Nothing                       -> basis -- no more violated constraints, so we found opt.
        Just (vs,rest)
          | length vs <= treshold hs' -> step gen2 $ (duplicate vs) <> rest
          | otherwise                 -> step gen2 hs' -- too many violated constraints, retry
      where
        basis       = subExp gen1 problem (sample gen r hs')
        (gen1,gen2) = splitGen gen

    -- Compute the violated, and satisfied constraints
    violated basis hs' = case Vector.partition (\h -> isJust $ extendBasis h basis) hs' of
      (vs,rest) | null vs   -> Nothing -- no more violated constraints
                | otherwise -> Just (vs,rest)

duplicate   :: Vector.Vector set a => set a -> set a
duplicate v = Vector.fromListN (2*Vector.length v) $ Vector.foldMap (\x -> [x,x]) v

-- weightedSample :: gen -> Weight -> f (Weighted a) -> vector a
-- weightedSample = undefined


--------------------------------------------------------------------------------

-- | SubExponential time algorithm to solve the LP-type problem
--
-- input: \(n\) constraints, combinatorial dimension \(d\)
--
-- running time: \(O(d^2 + nd) * e^{O(\sqrt{d\ln n})}\)
subExp        :: ( Foldable set, Foldable basis
                 , Ord t
                 , RandomGen gen
                 , Eq a
                 )
                 => gen
                    -- ^ random generator
                -> LPType t basis set a
                -- ^ an LP-type problem
                -> set a
                -> basis a
subExp gen (LPType _ _ extendBasis initialBasis) hs =
    subExp' (initialBasis hs) (V.toList $ shuffle gen hs)
  where
    subExp' basis = \case
      []     -> basis
      (h:gs) -> let basis' = subExp' basis gs
                in case extendBasis h basis' of
                     Nothing            -> basis'
                     Just extendedBasis -> subExp' extendedBasis gs

-- | Also return the value of the optimal solution
withOpt         :: (b -> t) -> b -> (t, b)
withOpt v basis = (v basis, basis)
