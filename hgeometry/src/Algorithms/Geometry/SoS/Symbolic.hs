module Algorithms.Geometry.SoS.Symbolic where

import           Algorithms.Geometry.SoS.Expr
import           Algorithms.Geometry.SoS.RWithIdx
import           Algorithms.Geometry.SoS.Sign (Sign(..))
import           Control.Lens
import           Control.Monad.ST.Strict
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Ext
import           Data.Foldable (toList)
import           Data.Geometry.Point.Class
import           Data.Geometry.Point.Internal
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import qualified Data.Geometry.Vector as GV
import           Data.Geometry.Vector hiding (imap)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import           Data.Maybe
import           Data.Ord (Down(..))
import           Data.Reflection
import           Data.Util
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import           GHC.TypeNats
import           Linear.Matrix
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))


--------------------------------------------------------------------------------
-- * EpsFolds


-- | An EpsFold represents the term
--
-- \[\Pi_{(i,j)\in is} \varepsilon(i,j) \]
--
-- The functions (\varepsilon\) satisfy the following property:
--
-- for every pair of indices \( (i,j) \leq (k,\ell) \) we have that:
--
-- \[ \Pi_{(a,b) \leq (i,j)} \varepsilon(a,b) > \varepsilon(k,\ell)
-- \]
--
-- Moreover, notice that, since \(\varepsilon \in (0,1)\), this
-- property also holds for any subset \(S\) of the \((a,b)\) pairs,
-- i.e.:
--
-- \[ \Pi_{(a,b) (a,b) \leq (i,j) \land (a,b) \in S} \varepsilon(a,b)
--  > \Pi_{(a,b) \leq (i,j)} \varepsilon(a,b)
--  > \varepsilon(k,\ell)
-- ]
newtype EpsFold i j = EpsFold (Bag (i,j)) deriving (Show,Semigroup,Monoid)

instance (Ord i, Ord j) => Eq (EpsFold i j) where
  e1 == e2 = (e1 `compare` e2) == EQ

instance (Ord i, Ord j) => Ord (EpsFold i j) where
  (EpsFold e1) `compare` (EpsFold e2) = e2e1 `compare` e1e2
    where
      e1e2 = maximum' $ e1 `difference` e2
      e2e1 = maximum' $ e2 `difference` e1
    -- note: If the terms are all the same, the difference of the bags is empty
    -- and thus both e1e2 and e2e1 are Nothing and thus equal.

    -- otherwise, let (i,j) be the largest term that is in e1 but not in e2.
    -- If e2 does not have any terms at all (Nothing) it will be bigger than e1
    --
    -- if e2 does have a term, let (k,l) be the largest one, then the
    -- biggest of those terms is the pair whose indices comes first.

    -- note that the pertubation becomes increasingly smaller for
    -- terms that have larger indices, moreover we have
    --
    --



singletonEpsFold :: (i,j) -> EpsFold i j
singletonEpsFold = EpsFold . singleton


hasNoPertubation             :: EpsFold i j -> Bool
hasNoPertubation (EpsFold b) = null b


--------------------------------------------------------------------------------
-- * Terms

-- | A term 'Term c es' represents a term:
--
-- \[ c \Pi_{(i,j) \in es} \varepsilon(i,j)
-- \]
--
-- for a constant c and an arbitrarily small value \(eps\), and a
-- positive value \(\delta\).
data Term i j r = Term r (EpsFold i j) deriving (Show,Eq,Functor)

-- | Creates a singleton term
term      :: r -> (i, j) -> Term i j r
term r ij = Term r $ singletonEpsFold ij

instance (Ord i, Ord j, Ord r, Num r) => Ord (Term i j r) where
  (Term c e1) `compare` (Term d e2) = case (hasNoPertubation e1, hasNoPertubation e2) of
                                        (True,True) -> c    `compare` d
                                        _           -> case (signum c, signum d) of
                                                         (-1,-1) -> e2 `compare` e1
                                                         (0,0)   -> e1 `compare` e2
                                                         (1,1)   -> e1 `compare` e2
                                                         (-1,_)  -> LT
                                                         (_,-1)  -> GT
                                                         _       -> error "SoS: Term.ord absurd"
  -- If both the eps folds are zero, and thus we just have constants
  -- then we should compare the individual terms.

  -- if *one* of the two has an eps term, then we can choose eps to be
  -- arbitrarily small, i.e. small enough so that that terms is
  -- actually smaller than the other term.  this is reflected since
  -- findMax will then return a Noting, which is smaller than anything
  -- else

  -- if both terms have epsilon terms, we first look at the sign. If
  -- they have non-negative signs we compare the eps-folds as in the
  -- paper. (Lemma 3.3). If both are negative, that reverses the
  -- ordering. If the signs are different then we can base the
  -- ordering on that.

--------------------------------------------------------------------------------

-- | Represents a Sum of terms, i.e. a value that has the form:
--
-- \[
--   \sum c \Pi_{(i,j)} \varepsilon(i,j)
-- \]
--
-- The terms are represented in order of decreasing significance.
newtype Symbolic i j r = Symbolic (Map.Map (EpsFold i j) r) deriving (Show,Eq,Functor)

-- | Produces a list of terms, in decreasing order of significance
toTerms              :: Symbolic i j r -> [Term i j r]
toTerms (Symbolic m) = map (\(ij,c) -> Term c ij) . Map.toAscList $ m

-- | Computing the Sign of an expression.
signOf   :: (Num r, Eq r) => Symbolic i j r -> Maybe Sign
signOf e = case List.dropWhile (== 0) . map (\(Term c _) -> signum c) $ toTerms e of
             []     -> Nothing
             (-1:_) -> Just Negative
             _      -> Just Positive

instance (Ord i, Ord j, Ord r, Num r) => Ord (Symbolic i j r) where
  e1 `compare` e2 = case signOf (e1 - e2) of
                      Nothing       -> EQ
                      Just Negative -> LT
                      Just Positive -> GT

instance (Ord i, Ord j, Num r, Eq r) => Num (Symbolic i j r) where
  (Symbolic e1) + (Symbolic e2) =
      Symbolic $ Map.merge Map.preserveMissing -- insert things only in e1
                           Map.preserveMissing -- insert things only in e2
                           combine
                           e1 e2
    where
      -- if things are in both e1 and e2, we add the constant terms. If they are non-zero
      -- we use this value in the map. Otherwise we drop it.
      combine = Map.zipWithMaybeMatched
                (\_ c d -> let x = c + d in if x /= 0 then Just x else Nothing)
    -- Symbolic $ Map.unionWith (+) ts ts'

  negate = fmap negate

  (Symbolic ts) * (Symbolic ts') = Symbolic $
    Map.fromListWith (+) [ (es <> es',c*d)
                         | (es, c) <- Map.toList ts
                         , (es',d) <- Map.toList ts'
                         , c*d /= 0
                         ]


  fromInteger x = constant (fromInteger x)

  signum s = case signOf s of
               Nothing       -> 0
               Just Negative -> (-1)
               Just Positive -> 1

  abs x | signum x == -1 = (-1)*x
        | otherwise      = x

----------------------------------------

-- | Creates a constant symbolic value
constant   :: (Ord i, Ord j) => r -> Symbolic i j r
constant c = Symbolic $ Map.singleton mempty c

-- | Creates a symbolic vlaue with a single indexed term. If you just need a constant (i.e. non-indexed), use 'constant'
symbolic      :: (Ord i, Ord j) => r -> (i, j) -> Symbolic i j r
symbolic r ij = Symbolic $ Map.singleton (singletonEpsFold ij) r

-- | given the value c and the index pair ij, creates the perturbed value
-- \(c + \varepsilon(i,j)\)
perturb      :: (Num r, Ord i, Ord j) => r -> (i, j) -> Symbolic i j r
perturb c ij = Symbolic $ Map.fromAscList [ (mempty,c) , (singletonEpsFold ij,1) ]


--------------------------------------------------------------------------------

-- | The word specifiies how many *duplicates* there are. I.e. If the
-- Bag maps k to i, then k has multiplicity i+1.
newtype Bag a = Bag (Map.Map a Word) deriving (Show,Eq,Ord,Monoid)

singleton   :: k -> Bag k
singleton x = Bag $ Map.singleton x 0


instance Foldable Bag where
  -- ^ Takes multiplicity into account.
  foldMap f (Bag m) =
    Map.foldMapWithKey (\k d -> foldMap f (List.replicate (fromIntegral d+1) k)) m

instance Ord k => Semigroup (Bag k) where
  (Bag m) <> (Bag m') = Bag $ Map.unionWith (\d d' -> d + d' + 1) m m'

-- | Computes the difference of the two maps
difference                   :: Ord a => Bag a -> Bag a -> Bag a
difference (Bag m1) (Bag m2) = Bag $ Map.differenceWith updateCount m1 m2
  where
    updateCount i j = let d = i - j -- note that we should actually compare (i+1) and (j+1)
                      in if d <= 0 then Nothing -- we have no copies left
                                   else Just d


maximum'         :: Bag b -> Maybe b
maximum' (Bag m) = fmap fst . Map.lookupMax $ m
