module Algorithms.Geometry.SoS.Symbolic(
    EpsFold
  , eps, mkEpsFold
  , hasNoPertubation
  , factors
  , suitableBase

  , Term(..), term, constantFactor

  , Symbolic
  , constant, symbolic, perturb

  , toTerms
  , signOf
  ) where

import           Algorithms.Geometry.SoS.Sign (Sign(..))
import           Control.Lens
import           Data.Foldable (toList)
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map
import           Data.Maybe (isNothing)
import           Test.QuickCheck (Arbitrary(..), listOf)
import           Test.QuickCheck.Instances ()

--------------------------------------------------------------------------------
-- * EpsFolds

{-
Let \(\mathcal{I}\) be a bag with indices, let \(c\) be an upper
bound on the number of times a single item may occur in
\(\mathcal{I}\), and let \(\varepsilon\) be a function mapping indices
to real numbers that satisfies:

1. \(0 < \varepsilon(j) < 1\), for all \(1 \leq j\),
2. \(\prod_{0 \leq i \leq j} \varepsilon(i)^c > \varepsilon(k)\), for all \(1 \leq j < k\)

Note that such a function exists:

\begin{lemma}
  \label{lem:condition_2}
  Let \(\delta \in (0,1)\) and \(d \geq c+1\). The function
  \(\varepsilon(i) = \delta^{d^i}\) satisfies condition 2.
\end{lemma}

\begin{proof}
  By transitivity it suffices to argue this for \(k=j+1\):

  \begin{align*}
           &\qquad \prod_{0 \leq i \leq j} \varepsilon(i)^c > \varepsilon(j+1) \\
    \equiv &\qquad \prod_{0 \leq i \leq j} (\delta^{d^i})^c > \delta^{d^{j+1}}\\
    \equiv &\qquad \prod_{0 \leq i \leq j} \delta^{cd^i}    > \delta^{d^{j+1}} \\
    \equiv &\qquad \delta^{\sum_{0 \leq i \leq j} cd^i} > \delta^{d^{j+1}} &
                                                                    \text{using
                                                                    }
                                                                    \delta \in (0,1)\\
    \equiv &\qquad \sum_{0 \leq i \leq j} cd^i < d^{j+1} \\
    \equiv &\qquad c\sum_{0 \leq i \leq j} d^i < d^{j+1} \\
  \end{align*}

  We prove this by induction.

  For the base case \(j=0\): we have \(0 < 1\), which is trivially true.

  For the step case we have the induction hypothesis
  \(c\sum_{0 \leq i \leq j} d^i < d^{j+1}\), and we have to prove that
  \(c\sum_{0 \leq i \leq j+1} d^i < d^{j+2}\):

  \begin{align*}
    c\sum_{0 \leq i \leq j+1} d^i
    &= cd^{j+1} + c\sum_{0 \leq i \leq j} d^i \\
    &< cd^{j+1} + d^{j+1}   & \text{using IH}  \\
    &= (c+1)d^{j+1}        & \text{using that } c+1 \leq d \\
    &\leq dd^{j+1}  \\
    &=d^{j+2}
  \end{align*}
  This completes the proof.
\end{proof}






An EpsFold now represents the term

\[ \prod_{i \in \mathcal{I}} \varepsilon(i) \]

for some bag \(\mathcal{I}\).


Let \(\mathcal{J}\) be some sub-bag of \(\mathcal{I}\). Note that
condition 2 implies that:

\(\prod_{i \in \mathcal{J}} \varepsilon(i) > \varepsilon(k)\), for all \(1 \leq j < k\)

This means that when comparing two EpsFolds, say \(e_1\) and \(e_2\),
representing bags \(\mathcal{I}_1\) and \(\mathcal{I}_2\),
respectively. It suffices to compare the largest index
\(j \in \mathcal{I}_1\setminus\mathcal{I}_2\) with the largest index
\(k \in \mathcal{I}_2\setminus\mathcal{I}_1\). We have that

\(e_1 > e_2\) if and only if \(j < k\).
-}
newtype EpsFold i = Pi (Bag i) deriving (Semigroup,Monoid)

-- | Gets the factors
factors         :: EpsFold i -> Bag i
factors (Pi is) = is

-- | Creates the term \(\varepsilon(i)\)
eps :: i -> EpsFold i
eps = Pi . singleton

mkEpsFold :: Ord i => [i] -> EpsFold i
mkEpsFold = Pi . foldMap singleton



-- | computes a base 'd' that can be used as:
--
-- \( \varepsilon(i) = \varepsilon^{d^i} \)
suitableBase :: EpsFold i -> Int
suitableBase = max 2 . (1+) . maxMultiplicity . factors

instance Show i => Show (EpsFold i) where
  showsPrec d (Pi b) = showParen (d > app_prec) $
                         showString "Pi " . showsPrec d (toList b)
    where
      app_prec = 10


instance Ord i => Eq (EpsFold i) where
  e1 == e2 = (e1 `compare` e2) == EQ

instance Ord i => Ord (EpsFold i) where
  (Pi e1) `compare` (Pi e2) = k `compare` j -- note that k and j are flipped here
    where
      j = maximum' $ e1 `difference` e2
      k = maximum' $ e2 `difference` e1
    -- note: If the terms are all the same, the difference of the bags is empty
    -- and thus both e1e2 and e2e1 are Nothing and thus equal.

    -- otherwise, let j be the largest term that is in e1 but not in e2.
    -- If e2 does not have any terms at all (Nothing) it will be bigger than e1
    --
    -- if e2 does have a term, let k be the largest one, then the
    -- biggest of those terms is the pair whose indices comes first.

instance (Arbitrary i, Ord i) => Arbitrary (EpsFold i) where
  arbitrary = (mkEpsFold . take 4) <$> listOf arbitrary


-- | Test if the epsfold has no pertubation at all (i.e. if it is \(\Pi_{\emptyset}\)
hasNoPertubation        :: EpsFold i -> Bool
hasNoPertubation (Pi b) = null b


--------------------------------------------------------------------------------
-- * Terms

-- | A term 'Term c es' represents a term:
--
-- \[ c \Pi_{i \in es} \varepsilon(i)
-- \]
--
-- for a constant c and an arbitrarily small value \(\varepsilon\),
-- parameterized by i.
data Term i r = Term r (EpsFold i) deriving (Eq,Functor)

-- | Lens to access the constant 'c' in the term.
constantFactor :: Lens' (Term i r) r
constantFactor = lens (\(Term c _) -> c) (\(Term _ es) c -> Term c es)


instance (Show i, Show r) => Show (Term i r) where
  showsPrec d (Term c es) = showParen (d > up_prec) $
                               showsPrec (up_prec + 1) c
                             . showString " * "
                             . showsPrec (up_prec + 1) es
    where
      up_prec = 5


-- | Creates a singleton term
term     :: r -> i -> Term i r
term r i = Term r $ eps i

instance (Ord i, Ord r, Num r) => Ord (Term i r) where
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

instance (Arbitrary r, Arbitrary (EpsFold i), Ord i) => Arbitrary (Term i r) where
  arbitrary = Term <$> arbitrary <*> arbitrary

--------------------------------------------------------------------------------
-- * Symbolic

-- | Represents a Sum of terms, i.e. a value that has the form:
--
-- \[
--   \sum c \Pi_i \varepsilon(i)
-- \]
--
-- The terms are represented in order of decreasing significance.
--
-- The main idea in this type is that, if symbolic values contains
-- \(\varepsilon(i)\) terms we can always order them. That is, two
-- Symbolic terms will be equal only if:
--
-- - they contain *only* a constant term (that is equal)
-- - they contain the exact same \(\varepsilon\)-fold.
--
newtype Symbolic i r = Sum (Map.Map (EpsFold i) r) deriving (Functor)

-- | Produces a list of terms, in decreasing order of significance
toTerms         :: Symbolic i r -> [Term i r]
toTerms (Sum m) = map (\(i,c) -> Term c i) . Map.toDescList $ m

-- | Computing the Sign of an expression. (Nothing represents zero)
signOf   :: (Num r, Eq r) => Symbolic i r -> Maybe Sign
signOf e = case List.dropWhile (== 0) . map (\(Term c _) -> signum c) $ toTerms e of
             []     -> Nothing
             (-1:_) -> Just Negative
             _      -> Just Positive

instance (Ord i, Eq r, Num r) => Eq (Symbolic i r) where
  e1 == e2 = isNothing $ signOf (e1 - e2)

instance (Ord i, Ord r, Num r) => Ord (Symbolic i r) where
  e1 `compare` e2 = case signOf (e1 - e2) of
                      Nothing       -> EQ
                      Just Negative -> LT
                      Just Positive -> GT

instance (Ord i, Num r, Eq r) => Num (Symbolic i r) where
  (Sum e1) + (Sum e2) = Sum $ Map.merge Map.preserveMissing -- insert things only in e1
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

  (Sum ts) * (Sum ts') = Sum $ Map.fromListWith (+) [ (es <> es',c*d)
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


instance (Show i, Show r) => Show (Symbolic i r) where
  showsPrec d s = showParen (d > app_prec) $
                    showString "Sum " . showsPrec d (toTerms s)
    where
      app_prec = 10

instance (Arbitrary r, Ord i, Arbitrary (EpsFold i)) => Arbitrary (Symbolic i r) where
  arbitrary = Sum <$> arbitrary

----------------------------------------

-- | Creates a constant symbolic value
constant   :: Ord i => r -> Symbolic i r
constant c = Sum $ Map.singleton mempty c

-- | Creates a symbolic vlaue with a single indexed term. If you just need a constant (i.e. non-indexed), use 'constant'
symbolic     :: Ord i => r -> i -> Symbolic i r
symbolic r i = Sum $ Map.singleton (eps i) r

-- | given the value c and the index i, creates the perturbed value
-- \(c + \varepsilon(i)\)
perturb      :: (Num r, Ord i) => r -> i -> Symbolic i r
perturb c i = Sum $ Map.fromAscList [ (mempty,c) , (eps i,1) ]


--------------------------------------------------------------------------------

-- | The word specifiies how many *duplicates* there are. I.e. If the
-- Bag maps k to i, then k has multiplicity i+1.
newtype Bag a = Bag (Map.Map a Int) deriving (Show,Eq,Ord,Arbitrary)

singleton   :: k -> Bag k
singleton x = Bag $ Map.singleton x 0


instance Foldable Bag where
  -- ^ Takes multiplicity into account.
  foldMap f (Bag m) =
    Map.foldMapWithKey (\k d -> foldMap f (List.replicate (fromIntegral d+1) k)) m
  null (Bag m) = Map.null m

instance Ord k => Semigroup (Bag k) where
  (Bag m) <> (Bag m') = Bag $ Map.unionWith (\d d' -> d + d' + 1) m m'

instance Ord k => Monoid (Bag k) where
  mempty = Bag $ Map.empty

-- | Computes the difference of the two maps
difference                   :: Ord a => Bag a -> Bag a -> Bag a
difference (Bag m1) (Bag m2) = Bag $ Map.differenceWith updateCount m1 m2
  where
    updateCount i j = let d = i - j -- note that we should actually compare (i+1) and (j+1)
                      in if d <= 0 then Nothing -- we have no copies left
                                   else Just $ d - 1


maximum'         :: Bag b -> Maybe b
maximum' (Bag m) = fmap fst . Map.lookupMax $ m


-- | maximum multiplicity of an element in the bag
maxMultiplicity         :: Bag a -> Int
maxMultiplicity (Bag m) = maximum . (0:) . map (1+) . Map.elems $ m
