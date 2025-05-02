{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
module LPTypeSpec
  ( spec
  , render
  , bug2
  ) where

import           Control.Lens
import           Data.Foldable (toList, foldMap')
import           Data.Foldable1
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromJust, isJust)
import           Data.Ord (comparing)
import           Data.Proxy
import           Data.Semigroup
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Generic as Vector
import           Data.Word
import           Debug.Trace
import           GHC.TypeLits
import           Golden
import           HGeometry.Combinatorial.Util
import           HGeometry.Ext
import           HGeometry.Foldable.Util
import           HGeometry.HalfLine
import           HGeometry.HalfSpace
import           HGeometry.HyperPlane
import           HGeometry.Instances ()
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Number.Real.Rational
import           HGeometry.Permutation.Shuffle
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Unbounded
import           HGeometry.Vector hiding (basis)
import           Ipe
import           Ipe.Color
import           Prelude hiding (filter)
import           System.OsPath
import           System.Random
import           Test.Hspec hiding (Arg(..))
import           Test.Hspec.QuickCheck
import           Test.QuickCheck ( (===),property,Discard(..), counterexample
                                 , Arbitrary(..), oneof, suchThat
                                 , withDiscardRatio, withMaxSuccess
                                 )
import           Test.QuickCheck.Instances ()
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder
import           Witherable
--------------------------------------------------------------------------------

type R = RealNumber 5



--------------------------------------------------------------------------------
-- * 1D Linear Programming

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



{-
-- | Linear programming in 1D, minimizes the coordinate values.
--
-- \(O(n)\).
lp1D             :: (Ord r, Foldable1 set) => set (HalfSpaceF r) -> Basis1DLP r
lp1D constraints = case foldr f (Bottom,Top) constraints of
    (Bottom,_)             -> Unbounded
    (ValB (Arg x p), negs) -> case negs of
      Top                         -> Feasible1 p
      ValT (Arg x' n) | x <= x'   -> Feasible1 p
                      | otherwise -> InFeasible1 p n
  where
    f h@(HalfSpace sign x) (pos,neg) = case sign of
      Positive -> (ValB (Arg x h) `max` pos, neg)
      Negative -> (pos,                      ValT (Arg x h) `min` neg)
-}


--------------------------------------------------------------------------------

-- subEx :: HalfSpace_ halfSpace d r
--       => Vector d r
--       -- ^ optimization direction
--       -> g halfSpace
--       -- ^ the constraints
--       -> b halfSpace
--       -- ^ the initial basis
--       -> Maybe (Point d r, [halfSpace])



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



-- linearProgramming              :: Vector d r -> set halfSpace -> LPType (Top r) set halfSpace
-- linearProgramming v constraints = LPType {
--     costFunction hs        = undefined
--   , elements               = constraints
--   , combinatorialDimension = 2 -- TODO:
--   , basisFor          hs  = take d hs
--   , initialBasis      hs  = take d hs
--   }


--------------------------------------------------------------------------------




-- data Basis halfSpace d where
--   Basis :: (i <= d + 1) => Vector i halfSpace

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

instance (Arbitrary r, Eq r, Fractional r) => Arbitrary (Basis2D r (HalfSpaceF (LineEQ r))) where
  arbitrary = oneof [ pure EmptyBasis
                    , do y <- arbitrary
                         pure $ Basis1 y (HalfSpace Positive (LineEQ 0 y))
                    , do l  <- arbitrary
                         l' <- arbitrary `suchThat` (\(LineEQ a _) ->
                                                       signum a /= signum (l^.slope))
                         case l `intersect` l' of
                           Just (Line_x_Line_Point p) ->
                             pure $ Basis2 p (HalfSpace Positive l) (HalfSpace Positive l')
                           _ -> arbitrary -- retry
-- don't generate infeasible basises for now
                    ]



type HalfPlane r = HalfSpaceF (LineEQ r)

-- | Given a basis, compute the cost associated with the basis
lpCost :: (Eq r, Num r) => Basis2D r (HalfPlane r) -> UnBounded r
lpCost = \case
  EmptyBasis   -> MinInfinity
  Basis1 y _   -> Val y
  Basis2 p _ _ -> Val $ p^.yCoord
  Infeasible _ -> MaxInfinity

  -- (HalfSpace sign (LineEQ a b)) -> case sign of
  --                                           Negative             -> MinInfinity
  --                                           Positive | a == 0    -> Val b
  --                                                    | otherwise -> MinInfinity


type HalfSpace1 r halfSpace = HalfSpaceF (Point 1 (r,r)) :+ (Point 2 r, halfSpace)

-- lp1D' :: Ord r => NonEmpty (HalfSpace1 r halfSpace) -> Basis1DLP (HalfSpace1 r halfSpace)
-- lp1D' = lp1D





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


-- | Constructs a 1D halfspace
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






--------------------------------------------------------------------------------


-- | Solves the LP Type problem. In particular, a minimization problem.
--
-- returns the cost of an optimal solution (if it exists), and the subset of elements realizing it
--
-- expected: O(n)
clarkson            :: ( Foldable set, Monoid (set a), Vector.Vector set a
                       , Foldable basis, Ord t, Eq a, SplitGen gen
                       )
                    => gen
                    -- ^ random generator
                    -> LPType t basis set a
                    -- ^ an LP-type problem
                    -> set a
                    -> (t, basis a)
clarkson gen0 problem@(LPType _ dim extendBasis _) hs
    | n <= 9* (dim*dim) = clarkson2 gen0 problem hs
    | otherwise         = step gen0 mempty
  where
    n      = length hs
    sqrtN' = sqrt . fromIntegral $ n
    sqrtN  = floor sqrtN'
    r      = floor $ fromIntegral dim * sqrtN'

    step gen partial = case violated extendBasis basis hs of
        Nothing                  -> sol -- no more violated constraints, so we found opt.
        Just (vs,_)
          | length vs <= 2*sqrtN -> step gen2 (vs <> partial) -- extend and continue
          | otherwise            -> step gen2 partial -- too many violated constraints, retry
      where
        sol@(_,basis) = clarkson2 gen1 problem (partial <> sample gen r hs)
        (gen1,gen2)   = splitGen gen

    -- -- computes the violated constraints
    -- violated = \case
    --   ValT (cost, basis) -> NonEmpty.nonEmpty $
    --                         filter (\h -> cost < v (h NonEmpty.<| basis)) (toList hs)
    --   Top                -> Nothing
    --                         -- Cost is already infinity; so adding constraints cannot
    --                         -- make it worse.

-- -- | Produce na infinite stream of generators
-- gens      :: SplitGen gen => gen -> NonEmpty gen
-- gens gen0 = let (g,g1) = splitGen gen0 in g NonEmpty.<| gens g1



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


-- TODO: just return the basis. compute the value at the end

-- | The clarkson2 algorithm.
clarkson2      :: ( Foldable set, Semigroup (set a), Vector.Vector set a
                  , Foldable basis, Ord t, Eq a, SplitGen gen
                  )
               => gen
                  -- ^ random generator
                -> LPType t basis set a
                -- ^ an LP-type problem
                -> set a
               -> (t, basis a)
clarkson2 gen0 problem@(LPType _ dim extendBasis _) hs
    | n <= r    = subExp gen0 problem hs
    | otherwise = step gen0 hs
  where
    n = length hs
    r = 6* (dim*dim)

    treshold hs' = length hs' `div` 3*dim

    step gen hs' = case violated extendBasis basis hs' of
        Nothing -> sol -- no more violated constraints, so we found opt.
        Just (vs,rest)
          | length vs <= treshold hs' -> step gen2 $ (duplicate vs) <> rest
          | otherwise                 -> step gen2 hs' -- too many violated constraints, retry
      where
        sol@(_,basis) = subExp gen1 problem (sample gen r hs')
        (gen1,gen2)   = splitGen gen

-- | computes the set of violated constraints, and the set of satisified constraints
violated                      :: (Vector.Vector set a, Foldable set)
                              => (a -> basis -> Maybe b)
                              -> basis
                              -> set a -> Maybe (set a, set a)
violated extendBasis basis hs = case Vector.partition isViolated hs of
    (vs,rest) | null vs   -> Nothing -- no more violated constraints
              | otherwise -> Just (vs,rest)
  where
    isViolated h = isJust $ extendBasis h basis



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
                -> (t, basis a)
subExp gen (LPType v _ extendBasis initialBasis) hs =
    let basis = subExp' (initialBasis hs) (V.toList $ shuffle gen hs)
    in (v basis, basis)
  where
      -- traceShowWith ("subExp'",basis,"inp: ",inpGs,) $ case inpGs of
    subExp' basis = \case
      []     -> basis
      (h:gs) -> let basis' = subExp' basis gs
                in case extendBasis h basis' of
                     Nothing            -> basis'
                     Just extendedBasis -> subExp' extendedBasis gs


      -- TODO: there is still a small diff. in that
      -- the orig description picks a random item from "removed <> gs"
      -- whereas here we always pick the removed ones "first/last" instead of at random

-- | Remoevs the items from the basis from the input set.
extract           :: (Foldable basis, Eq a, Foldable set) => basis a -> set a -> (basis a, [a])
extract basis' gs = (basis', filter (`notElem` basis') $ toList gs)

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


-- instance HyperPlane_ (Point 1 r) 1 r where

-- instance NonVerticalHyperPlane_ (Point 1 r) 1 r --

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "LPType Spec" $ do
         describe "1D LP" $ do
           it "1D feasible example" $ do
             lp1D (NonEmpty.fromList [ HalfSpace Positive (Point1 0 :: Point 1 R)
                                     , HalfSpace Negative (Point1 10)
                                     , HalfSpace Positive (Point1 3)
                                     , HalfSpace Negative (Point1 5)
                                     , HalfSpace Positive (Point1 2)
                                     ]) `shouldBe` Feasible1 (HalfSpace Positive (Point1 3))

           it "1D infeasible example" $ do
             lp1D (NonEmpty.fromList [ HalfSpace Positive (Point1 0 :: Point 1 R)
                                     , HalfSpace Negative (Point1 10)
                                     , HalfSpace Positive (Point1 3)
                                     , HalfSpace Negative (Point1 5)
                                     , HalfSpace Negative (Point1 2)
                                     ]) `shouldBe` InFeasible1 (HalfSpace Positive (Point1 3))
                                                               (HalfSpace Negative (Point1 2))

         it "initialBasis" $ do
           let (h1:h2:_) = exampleLP
           lpInitialBasis exampleLP `shouldBe` (Basis2 (Point2 2.5 2.5) h1 h2)

         it "lp extend basis" $ do
           let (_:_:h3:_) = exampleLP
               basis' = lpInitialBasis exampleLP
           lpRecomputeBasis h3 basis' `shouldBe` Nothing

         it "subExp" $
           fst (subExp (mkStdGen 42) linearProgrammingMinY exampleLP) `shouldBe` (Val 2.5)

         it "manual infeasible" $ do
           let h1 = HalfSpace Positive (LineEQ 0.29166 38.66671) :: HalfSpaceF (LineEQ R)
               h2 = HalfSpace Positive (LineEQ (-0.6) 195.2)
               h3 = HalfSpace Negative (LineEQ 0 80)
               ib = lpInitialBasis [h1,h2]
           lpRecomputeBasis h3 ib `shouldBe` Just (Infeasible (Vector3 h3 h1 h2))

         prop "feasible means feasible" $ withMaxSuccess 50 $ withDiscardRatio 1000 $
           \gen (halfPlanes :: [HalfPlane R]) ->
             case subExp (mkStdGen gen) linearProgrammingMinY halfPlanes of
               (_,Basis2 v _ _) -> property $ all (v `intersects`) halfPlanes
               _                -> property Discard

         -- prop "feasible means interseciton point" $
         --   \gen (halfPlanes :: HalfPlane R) ->
         --     case subExp (mkStdGen gen) (linearProgrammingMinY halfPlanes) of
         --       (_,Basis2 v _ _) -> property $ isAVertex v
         --       _                -> property Discard


         modifyMaxSize (const 1000) $ prop "clarkson2 same as subExp" $
           \gen (halfPlanes :: V.Vector (HalfPlane R)) ->
             let (resSubExp, _) = subExp (mkStdGen gen) linearProgrammingMinY halfPlanes
                 (res,_)        = clarkson2 (mkStdGen gen) linearProgrammingMinY halfPlanes
             in res `shouldBe` resSubExp

         modifyMaxSize (const 1000) $ prop "clarkson2 same as subExp (positives)" $
           \gen (lines' :: [LineEQ R]) ->
             let (resSubExp, _) = subExp (mkStdGen gen)    linearProgrammingMinY halfPlanes
                 (res,_)        = clarkson2 (mkStdGen gen) linearProgrammingMinY halfPlanes
                 halfPlanes     = V.fromList [ HalfSpace Positive l | l <- lines' ]
             in res `shouldBe` resSubExp

         modifyMaxSize (const 1000) $ prop "clarkson same as clarkson2 (positives)" $
           \gen (lines' :: [LineEQ R]) ->
             let (res2, _)  = clarkson2 (mkStdGen gen) linearProgrammingMinY halfPlanes
                 (res,_)    = clarkson  (mkStdGen gen) linearProgrammingMinY halfPlanes
                 halfPlanes = V.fromList [ HalfSpace Positive l | l <- lines' ]
             in res `shouldBe` res2

         prop "brute force test" $
           \gen (halfPlanes :: [HalfPlane R]) ->
             let res = subExp (mkStdGen gen) linearProgrammingMinY halfPlanes
             in counterexample (show res) $
             case bruteForceSolutions halfPlanes of
               [] -> case res of
                       (_, EmptyBasis)   -> True -- for now
                       (_, Basis1 _ h)   -> h^.boundingHyperPlane.slope == 0
                       (_, Infeasible _) -> True
                       (_, Basis2 _ _ _) -> False
               vs -> case res of
                       (_, Basis2 v _ _) -> v `elem` vs
                       (_, EmptyBasis)   -> True -- could be true
                       (_, Basis1 _ _)   -> True -- could be true
                       _                 -> False

         testIpe [osp|LPType/lpType_linearProgramming.ipe|]
         testIpe [osp|LPType/linearProgramming1.ipe|]
         testIpe [osp|LPType/linearProgramming2.ipe|]
         testIpe [osp|LPType/linearProgramming2_simpl.ipe|]
         testIpe [osp|LPType/infeasible_lp.ipe|]

         bug
         bug2

         -- it "extract" $
         --   extract 3 (NonEmpty.fromList "foobarenzo") `shouldBe` ('b',"fooarenzo")
         -- prop "extract1" $
         --   extract1 (mkStdGen 42) (NonEmpty.fromList "foobarenzo") "foo" ===  ('n',"foobarezo")


bruteForceSolutions    :: ( Ord r, Fractional r
                          , HyperPlane_ line 2 r
                          , IsIntersectableWith line line
                          , Intersection line line ~ Maybe (LineLineIntersectionG r line)
                          , Dimension line ~ 2, NumType line ~ r
                          , Foldable set, Functor set
                          )
                       => set (HalfSpaceF line) -> [Point 2 r]
bruteForceSolutions hs = mapMaybe intersectionPoint $ uniquePairs (view boundingHyperPlane <$> hs)
  where
    intersectionPoint (Two l l') = case l `intersect` l' of
      Just (Line_x_Line_Point p) | all (p `intersects`) hs -> Just p
      _                                                    -> Nothing

bug = describe "bug" $ do
        let cs = [HalfSpace Positive (LineEQ 3 (-1.5))
                 ,HalfSpace Positive (LineEQ (-5.5) 2.6)
                 ,HalfSpace Positive (LineEQ 5 4)
                 ,HalfSpace Negative (LineEQ (-3) 2)
                 ]
            gen = 1
            res = subExp (mkStdGen gen) linearProgrammingMinY cs

            ib = lpInitialBasis cs

        it "initial basis" $
          ib `shouldSatisfy` (\case
             Basis2 _ a b -> a == cs !! 0 && b == cs !! 1
             _            -> False)

        -- prop "extend 2" $
        --   lpRecomputeBasis (cs !! 2) ib `shouldBe` Nothing

        -- prop "extend 23" $
        --   lpRecomputeBasis (cs !! 3) (lpRecomputeBasis (cs !! 2) ib) `shouldSatisfy` (\case
        --     (Infeasible _) -> True
        --     _              -> False)

        -- prop "extend 3" $
        --   lpRecomputeBasis (cs !! 3) ib `shouldBe` ans


        prop "thebug" $ counterexample (show res) $
          case res of
            (_, Basis2 v _ _) -> all (v `intersects`) cs
            (_, Infeasible _) -> True
            _                 -> False
        it "bruteforce" $
          bruteForceSolutions cs `shouldBe` []

        it "bruteforce 234" $
          bruteForceSolutions (drop 1 cs) `shouldBe` []

bug2 = describe "bug2" $ do
        let cs :: [HalfPlane R]
            cs = [HalfSpace Positive (LineEQ 13.33333 (-17.5))        -- :+ red
                 ,HalfSpace Positive (LineEQ (-16.57143) (-11.26316)) -- :+ purple
                 ,HalfSpace Positive (LineEQ (-3.05883) 17.5)         -- :+ blue
                 ,HalfSpace Positive (LineEQ 5 (-11.2))               -- :+ green
                 ]
            gen = 1
            res = subExp (mkStdGen gen) linearProgrammingMinY cs

            ib = lpInitialBasis cs

        -- it "initial basis" $
        --   ib `shouldSatisfy` (\case
        --      Basis2 _ a b -> a == cs !! 0 && b == cs !! 1
        --      _            -> False)

        -- it "extend 0" $
        --   let ib = Basis2 (Point2 3.5131 6.60655) (cs !! 2) (cs !! 3) in
        --   lpRecomputeBasis (cs !! 0) ib `shouldSatisfy` (\case
        --     Just (Basis2 _ a b,[_]) -> a == cs !! 0 && b == cs !! 2
        --     _                       -> False
        --                                                 )

        -- prop "recompute basis -> removed correct" $
        --   \(h :: HalfPlane R) basis ->
        --     case lpRecomputeBasis h basis of
        --       Just (newBasis,removed) -> Set.fromList (h : toList basis)
        --                                  ===
        --                                  Set.fromList (toList newBasis <> removed)
        --       Nothing -> property Discard

        prop "thebug" $ counterexample (show res) $
          case res of
            (_, Basis2 v _ _) -> all (v `intersects`) cs
            (_, Infeasible _) -> True
            _                 -> False
        it "bruteforce" $
          bruteForceSolutions cs `shouldSatisfy` ((== 2) . length)

        -- it "bruteforce 234" $
        --   bruteForceSolutions (drop 1 cs) `shouldBe` []



exampleLP :: [HalfPlane R]
exampleLP = [ HalfSpace Positive (LineEQ 1    0)
            , HalfSpace Positive (LineEQ (-1) 5)
            , HalfSpace Positive (LineEQ 2    (-3))
            ]


instance HalfSpace_ core d r => HalfSpace_ (core :+ extra) d r where
  type BoundingHyperPlane (core :+ extra) d r = BoundingHyperPlane core d r
  boundingHyperPlane = core.boundingHyperPlane
  halfSpaceSign = core.halfSpaceSign

--------------------------------------------------------------------------------






 -- [HalfSpace Positive (Arg (89.86891~,175.55038~) (Point2 175.55038~ 89.86891~,HalfSpace Positive (LineEQ 0.29166~ 38.66671~)))

 -- ,HalfSpace Positive (Arg (22.64946~,287.58279~) (Point2 287.58279~ 22.64946~,HalfSpace Positive (LineEQ (-0.06897~) 42.48275~)))]

testIpe      :: OsPath -> Spec
testIpe inFp = describe ("linear programming on file " <> show inFp) $ do
          (halfPlanes', solution) <- runIO $ loadInputs inFp
          let halfPlanes :: V.Vector _
              halfPlanes = fromFoldable halfPlanes'

          -- let (h1:|h2:h3:_) = (view core <$> halfPlanes)
          -- runIO $ mapM_ print (view core <$> halfPlanes)

          -- it "collect" $
          --   (collect $ mapMaybe (constructHalfSpaceOn h2) [h1,h3]) `shouldBe`
          --   Right []

          -- it "extend initialBasis" $ do
          --   let basis = lpInitialBasis [h1,h3]
          --   lpRecomputeBasis h2 basis `shouldBe` (Basis2 (Point2 89 175) h2 h1)

-- HalfSpace Positive (LineEQ 0.29166~ 38.66671~)
-- HalfSpace Positive (LineEQ (-0.06897~) 42.48275~)


-- HalfSpace Positive (LineEQ (-0.6) 195.2)

          prop "subExp correct, " $
            let (sol,basis') = subExp (mkStdGen 42) linearProgrammingMinY
                                      (view core <$> halfPlanes)
            in counterexample (show sol) $ Set.fromList (toList basis') === solution

          prop "clarkson correct, " $
            let (sol,basis') = clarkson (mkStdGen 42)  linearProgrammingMinY
                                        (view core <$> halfPlanes)
            in counterexample (show sol) $ Set.fromList (toList basis') === solution


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

toLineEQ                :: HalfSpaceF (LinePV 2 R) -> HalfPlane R
toLineEQ (HalfSpace _ l)
    | l^.direction.xComponent > 0 = HalfSpace Positive l' -- sign was already positive
    | otherwise                   = HalfSpace Negative l'
  where
    l' = fromJust . toLinearFunction $ l


    -- goldenWith [osp|data/test-with-ipe/golden/|]
    --                 (ipeContentGolden { name = theName  })
    --                 ( myIpeTest halfPlanes)


--------------------------------------------------------------------------------

render = writeIpeFile [osp|/tmp/out.ipe|] . addStyleSheet opacitiesStyle . singlePageFromContent $
           map (iO . ipeConstraint) cs
  where
    cs :: [HalfPlane R :+ IpeColor R]
    cs = [HalfSpace Positive (LineEQ 13.33333 (-17.5))        :+ red
         ,HalfSpace Positive (LineEQ (-16.57143) (-11.26316)) :+ purple
         ,HalfSpace Positive (LineEQ (-3.05883) 17.5)         :+ blue
         ,HalfSpace Positive (LineEQ 5 (-11.2))               :+ green
         ]

ipeConstraint (h :+ c) = ipeHalfPlane c $ h&boundingHyperPlaneLens %~ fromLineEQ
                                           &halfSpaceSign %~ \case
                                                                Positive -> Negative
                                                                Negative -> Positive
