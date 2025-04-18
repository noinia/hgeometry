{-# LANGUAGE QuasiQuotes #-}
module LPTypeSpec
  ( spec
  ) where

import           Control.Lens
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup
import qualified Data.Vector as Vector
import           Data.Word
import           HGeometry.HalfSpace
import           HGeometry.Line
import           HGeometry.Number.Real.Rational
import           HGeometry.Permutation.Shuffle
import           HGeometry.Point
import           HGeometry.Unbounded
import           Ipe
import           Ipe.Color
import           Paths_hgeometry
import           System.OsPath
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck ((===))
import           Test.QuickCheck.Instances ()
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder

--------------------------------------------------------------------------------

type R = RealNumber 5


--------------------------------------------------------------------------------

{-
-- subEx :: HalfSpace_ halfSpace d r
--       => Vector d r
--       -- ^ optimization direction
--       -> g halfSpace
--       -- ^ the constraints
--       -> b halfSpace
--       -- ^ the initial basis
--       -> Maybe (Point d r, [halfSpace])


data LPType t set a = LPType { optimizationGoal :: set a -> t
                             , elements         :: set a
                             , basisCost        :: set a -> t
                             }

-- | Solves the LP Type problem. In particular, a minimization problem.
--
-- returns the cost of an optimal solution (if it exists), and the subset of elements realizing it
--
-- expected: O(n)
clarkson            :: ( Foldable1 set
                       , Ord t
                       , SplitGen gen
                       )
                    => gen
                    -- ^ Random generator
                    ->  Int
                    -- ^ The combinatorial dimension
                    -> (set element -> t)
                    -- ^ The optimization goal
                    -> set element
                    -- ^ the input elements
                    -> Top (t, NonEmpty element)
clarkson gen0 dim v hs
    | n <= 9* (dim*dim) = clarkson2 dim v hs
    | otherwise         = step gen0 mempty
  where
    n = length hs
    r = floor $ fromIntegral dim * sqrt n

    step gen partial = let sol         = clarkson2 gen1 dim (partial <> sample gen r hs)
                           (gen1,gen2) = splitGen gen in
      case violated sol of
        Nothing     -> sol -- no more violated constraints, so we found opt.
        Just vs
          | length vs <= 2*sqrt n -> step gen2 (vs <> partial) -- extend and continue
          | otherwise             -> step gen2 partial -- too many violated constraints, retry

    -- computes the violated constraints
    violated = \case
      ValT (cost, basis) -> NonEmpty.nonEmpty $
                            filter (\h -> cost < v (h NonEmpty.<| basis)) (toList hs)
      Top                -> Nothing
                            -- Cost is already infinity; so adding constraints cannot
                            -- make it worse.

-- -- | Produce na infinite stream of generators
-- gens      :: SplitGen gen => gen -> NonEmpty gen
-- gens gen0 = let (g,g1) = splitGen gen0 in g NonEmpty.<| gens g1


-- | Take a sample of size r uniformly at random.
--
-- O(n)
--
-- TODO: make sure this is lazy enough to actually get O(r) time instead!
sample          :: ( Foldable1 f, V.Vector vector, RandomGen gen)
                => gen -> Int -> f a -> vector a
sample gen r hs = V.take r $ shuffle gen hs


type Weight = Int

data WithWeight a = WithWeight {-# UNPACK #-}!Weight a
                  deriving (Show,Eq)

-- | Get the total weight
totalWeight :: Foldable f => f (WithWeight a) -> Weight
totalWeight = getSum . foldMap' (\(WithWeight w _) -> w)


clarkson2      :: ( Foldable1 set
                  , Ord t
                  , RandomGen gen
                  )
               => gen
                  -- ^ random generator
              -> Int
              -- ^ The combinatorial dimension
              -> (set element -> t)
              -- ^ The optimization goal
              -> set element
              -- ^ the input elements
              -> Maybe (t, NonEmpty element)
clarkson2 gen0 dim v hs
    | n <= r    = subExp v hs
    | otherwise = step gen0 mempty (WithWeight 1 <$> hs)
  where
    n = length hs
    r = 6* (dim*dim)

    treshold hs' = totalWeight hs' `div` 3*dim

    step gen hs' = let sol         = subEx v (weightedSample gen r hs')
                       (gen1,gen2) = splitGen gen in
      case violated sol of
        Nothing        -> sol -- no more violated constraints, so we found opt.
        Just (vs,rest)
          | length vs <= treshold hs' -> step gen2 ((doubleWeight <$> vs) <> rest)
          | otherwise                 -> step gen2 hs' -- too many violated constraints, retry

    -- computes the violated constraints
    violated = \case
      ValT (cost, basis) -> case partition (\h -> cost < v (h NonEmpty.<| basis)) (toList hs) of
                              ([],_)    -> Nothing -- no more violated constraints
                              (vs,rest) -> Just (vs,rest)
      Top                -> Nothing
                            -- Cost is already infinity; so adding constraints cannot
                            -- make it worse.


weightedSample :: gen -> Weight -> f (Weighted a) -> vector a
weightedSample = undefined

--------------------------------------------------------------------------------

-- | SubExponential time algorithm to solve the LP-type problem
subExp      :: ( Foldable1 set
               , Ord t
               , RandomGen gen
                  )
               => gen
                  -- ^ random generator
              -> Int
              -- ^ The combinatorial dimension
              -> (set element -> t)
              -- ^ The optimization goal
              -> set element
              -- ^ the input elements
              -> Maybe (t, NonEmpty element)

subExp gen v hs = subExpWithBasis hs someBasis
  where
    subExpWithBasis gs basis
      | gs `eq` basis  = Just (v basis, basis)
      | otherwise      = let (h,gs') = extract1 gen gs basis
                             cost    = v basis
                         in
          case subExpWithBasis gs' basis of
            ValT sol@(_, basis') | cost < v (h NonEmpty.<| basis) ->
                                     subExpWithBasis gs (h NonEmpty.<| basis')
                                     -- add h to the basis
                                 | otherwise -> sol

    eq gs bs = all (`elem` bs) gs -- the other side is trivailly true

-}

extract1            :: (RandomGen gen, Eq a, Foldable1 f, Foldable set)
                    => gen -> f a -> set a -> (a, [a])
extract1 gen0 gs bs = go gen0
  where
    n  = length gs
    go gen = let (i,gen')  = uniformR (0,n-1) gen
                 res@(x,_) = extract i (toNonEmpty gs)
             in if x `elem` bs then go gen' else res


extract            :: Int -> NonEmpty a -> (a,[a])
extract 0 (x :| xs) = (x,xs)
extract j (x :| xs) = let (y,rest) = extract (j-1) (NonEmpty.fromList xs)
                      in (y,x:rest)

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "LPType Spec" $ do
         it "extract" $
           extract 3 (NonEmpty.fromList "foobarenzo") `shouldBe` ('b',"fooarenzo")
         prop "extract1" $
           extract1 (mkStdGen 42) (NonEmpty.fromList "foobarenzo") "foo" ===  ('b',"fooarenzo")
