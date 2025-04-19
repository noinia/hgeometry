{-# LANGUAGE QuasiQuotes #-}
module LPTypeSpec
  ( spec
  ) where

import           Control.Lens
import           Data.Foldable (toList)
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust)
import           Data.Ord (comparing)
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import           Data.Word
import           GHC.TypeLits
import           Golden
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Number.Real.Rational
import           HGeometry.Permutation.Shuffle
import           HGeometry.Point
import           HGeometry.Unbounded
import           Ipe
import           Ipe.Color
import           Prelude hiding (filter)
import           System.OsPath
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck ((===))
import           Test.QuickCheck.Instances ()
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder
import           Witherable

--------------------------------------------------------------------------------

type R = RealNumber 5


--------------------------------------------------------------------------------

-- subEx :: HalfSpace_ halfSpace d r
--       => Vector d r
--       -- ^ optimization direction
--       -> g halfSpace
--       -- ^ the constraints
--       -> b halfSpace
--       -- ^ the initial basis
--       -> Maybe (Point d r, [halfSpace])


data LPType t basis set a = LPType
                            { costFunction :: basis a -> t
                            -- ^ the function we are trying to minimize
                             , inputs      :: set a
                             -- ^
                             , combinatorialDimension :: Int
                             -- ^ the combinatorial dimension of the problem
                             , extendBasis :: a -> basis a -> basis a
                             -- ^ function to construct the an optimal basis (possibly expensive)
                             , initialBasis :: set a -> basis a
                             -- ^ function to construct some initial basis
                             -- the input may be large.
                             }



-- linearProgramming              :: Vector d r -> set halfSpace -> LPType (Top r) set halfSpace
-- linearProgramming v constraints = LPType {
--     costFunction hs        = undefined
--   , elements               = constraints
--   , combinatorialDimension = 2 -- TODO:
--   , basisFor          hs  = take d hs
--   , initialBasis      hs  = take d hs
--   }


-- data Basis halfSpace d where
--   Basis :: (i <= d + 1) => Vector i halfSpace

data Basis2D r halfSpace = Basis1 halfSpace
                         | Basis2 (Point 2 r) halfSpace halfSpace
                         -- | Basis3 halfSpace halfSpace halfSpace
                       deriving (Show,Eq,Functor,Foldable)


type HalfPlane r = HalfSpaceF (LineEQ r)

-- | Given a basis, compute the cost associated with the basis
lpCost :: (Eq r, Num r) => Basis2D r (HalfPlane r) -> Bottom r
lpCost = \case
  Basis1 (HalfSpace sign (LineEQ a b)) -> case sign of
    Negative             -> Bottom
    Positive | a == 0    -> ValB b
             | otherwise -> Bottom
  Basis2 p _ _ -> ValB $ p^.yCoord


  -- (HalfSpace sign l) (HalfSpace sign' l') -> case (sign,sign') of
  --   (Negative,_) -> Bottom
  --   (_,Negative) -> Bottom
  --   _            -> case l `intersect` l' of
  --                     Nothing -> error "absurd"
  --                     Just p  -> ValB (p^.yCoord)




-- | Tries to extend the basis with the given halfplane
lpRecomputeBasis :: (Ord r, Fractional r)
                                            => HalfPlane r
                                            -> Basis2D r (HalfPlane r) -> Basis2D r (HalfPlane r)
lpRecomputeBasis h@(HalfSpace sign l) basis = case sign of
    Negative -> basis -- shouldn't happen for now

    Positive -> case basis of
      Basis1 h1@(HalfSpace _ l1) -> case l `intersect'` l1 of
        Just p
          | signum (l1^.slope) /= signum (l^.slope) -> Basis2 p h h1
            -- if the signs are the same, the LP is unbounded and it doesn't help to add
            -- the new plane
        Nothing
          | l `isLowerThan` l1                      -> Basis1 h -- replace the basis
        _                                           -> basis

      Basis2 p h1 h2
        | p `intersects` h -> basis -- solution remains the same
        | otherwise        -> case l `intersect'` (h1^.boundingHyperPlane) of
            Nothing -> case l `intersect'` (h2^.boundingHyperPlane) of
              Just p2 -> Basis2 p2 h h2
              Nothing -> error "absurd"
                -- this shouldn't happen, as this implies h1 and h2 are colinear. However
                -- since p is their intersection point; that cannot be the case .
            Just p1 -> case l `intersect'` (h2^.boundingHyperPlane) of
              Nothing -> Basis2 p1 h h1
              Just p2 -> case p1 `cmp` p2 of
                LT -> Basis2 p1 h h2
                _  -> Basis2 p2 h h1
  where
    (LineEQ _ b) `isLowerThan` (LineEQ _ b') = b < b'
    (Point2 x y) `cmp` (Point2 x' y') = compare y y' <> compare x x'

    la `intersect'` lb = do Line_x_Line_Point p  <- la `intersect` lb
                            pure p  -- fail on something other than a point



-- | Find an initial basis; i.e. some halfplanes that bound the initial solution from below
lpInitialBasis    :: (Foldable set, Ord r, Fractional r)
                  => set (HalfPlane r)
                  -> Basis2D r (HalfPlane r)
lpInitialBasis hs = case filter (\h -> h^.halfSpaceSign == Positive) (toList hs) of
  []        -> error "lpInitialBasis: unounded?"
  (h1:rest) ->  foldr (\h basis -> case basis of
                          Basis1 _ -> lpRecomputeBasis h basis
                          _        -> basis
                        -- find a second halfplane to go along with it. As soon as
                        -- we found one we are good.
                      ) (Basis1 h1) rest

-- | minimize the y-coordinate. (Lexicographically)
-- pre: LP is feasible
linearProgrammingMinY             :: (Fractional r, Ord r, Foldable set)
                                  => set (HalfPlane r)
                                  -> LPType (Bottom r) (Basis2D r) set (HalfPlane r)
linearProgrammingMinY constraints = LPType {
    costFunction           = lpCost
  , inputs                 = constraints
  , combinatorialDimension = 2
  , extendBasis            = lpRecomputeBasis
  , initialBasis           = lpInitialBasis
  }







{-


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

-}
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
                -> (t, basis a)
subExp gen (LPType v hs _ extendBasis initialBasis) =
    case extract (initialBasis hs) hs of
      (basis,gs) -> foldr go (v basis, basis) (Vector.toList $ shuffle gen gs)
  where
    go h sol@(cost,basis) = let basis' = extendBasis h basis
                                cost'  = v basis'
                            in if cost < cost' then (cost',basis') else sol

extract          :: (Foldable basis, Eq a, Foldable set) => basis a -> set a -> (basis a, [a])
extract basis gs = (basis, filter (`notElem` basis) $ toList gs)

-- no longer needed:
{-
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
-}


--------------------------------------------------------------------------------

spec :: Spec
spec = describe "LPType Spec" $ do
         it "initialBasis" $ do
           let (h1:h2:_) = exampleLP
           lpInitialBasis exampleLP `shouldBe` (Basis2 (Point2 2.5 2.5) h2 h1)

         it "lp extend basis" $ do
           let (_:_:h3:_) = exampleLP
               basis = lpInitialBasis exampleLP
           lpRecomputeBasis h3 basis `shouldBe` basis

         it "subExp" $
           fst (subExp (mkStdGen 42) (linearProgrammingMinY exampleLP)) `shouldBe` (ValB 2.5)

         testIpe [osp|LPType/lpType_linearProgramming.ipe|]
         testIpe [osp|LPType/linearProgramming1.ipe|]
         testIpe [osp|LPType/linearProgramming2.ipe|]

         -- it "extract" $
         --   extract 3 (NonEmpty.fromList "foobarenzo") `shouldBe` ('b',"fooarenzo")
         -- prop "extract1" $
         --   extract1 (mkStdGen 42) (NonEmpty.fromList "foobarenzo") "foo" ===  ('n',"foobarezo")


exampleLP :: [HalfPlane R]
exampleLP = [ HalfSpace Positive (LineEQ 1    0)
            , HalfSpace Positive (LineEQ (-1) 5)
            , HalfSpace Positive (LineEQ 2    (-3))
            ]

--------------------------------------------------------------------------------


testIpe      :: OsPath -> Spec
testIpe inFp = describe ("linear programming on file " <> show inFp) $ do
          (halfPlanes, solution) <- runIO $ loadInputs inFp

          it "subExp correct" $ do
            let (_,basis) = subExp (mkStdGen 42) $ linearProgrammingMinY (view core <$> halfPlanes)
            Set.fromList (toList basis) `shouldBe` solution



          -- for_ segments $ \seg ->
          --   for_ halfPlanes $ \halfPlane -> do
          --     it ("intersects halfplane and line segment") $
          --       (seg `intersects` halfPlane) `shouldBe` True


loadInputs      :: OsPath -> IO ( NonEmpty (HalfPlane R :+ _)
                                , Set.Set (HalfPlane R)
                                )
loadInputs inFp = do
        inFp'      <- getDataFileName ([osp|test-with-ipe/|] <> inFp)
        Right page <- readSinglePageFile inFp'
        let (rays :: NonEmpty (HalfLine (Point 2 R) :+ _)) = NonEmpty.fromList $ readAll page
        -- take the left halfplane of every halfline
            halfPlanes = over core (toLineEQ . leftHalfPlane . asOrientedLine) <$> rays
            solution   = foldMap (\(h :+ ats) ->
                                    if lookupAttr SStroke ats == Just red
                                    then Set.singleton h else mempty
                                 ) halfPlanes
        pure (halfPlanes, solution)

toLineEQ :: HalfSpaceF (LinePV 2 R) -> HalfPlane R
toLineEQ = over boundingHyperPlaneLens (fromJust . toLinearFunction)

    -- goldenWith [osp|data/test-with-ipe/golden/|]
    --                 (ipeContentGolden { name = theName  })
    --                 ( myIpeTest halfPlanes)
