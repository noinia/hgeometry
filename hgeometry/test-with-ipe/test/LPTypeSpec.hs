{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
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
import           HGeometry.HyperPlane
import           HGeometry.Intersection
import           HGeometry.Line
import           HGeometry.Line.General
import           HGeometry.Number.Real.Rational
import           HGeometry.Permutation.Shuffle
import           HGeometry.Point
import           HGeometry.Properties
import           HGeometry.Unbounded
import           HGeometry.Vector
import           Ipe
import           Ipe.Color
import           Prelude hiding (filter)
import           System.OsPath
import           System.Random
import           Test.Hspec hiding (Arg(..))
import           Test.Hspec.QuickCheck
import           Test.QuickCheck ((===))
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
constructHalfSpaceOn h@(HalfSpace sign l) h'@(HalfSpace sign' l')
    | not (h `intersects` h') = Just $ Infeasible2 h'
    | otherwise               = case l `intersect` l' of
        Just (Line_x_Line_Point p) -> Just . Partial $ halfSpace1D p :+ (p,h')
        _                          -> Nothing -- constraint is not useful
  where
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
          | l1 `isLowerThan` l                      -> Basis1 h
            -- the bounding lines are parallel, but the new one is higher than
            -- the one in the basis. So the new one is more restrictive.
        _                                           -> basis

      Basis2 p h1 h2
        | p `intersects` h -> basis -- solution remains the same
        | otherwise        -> case collect $ mapMaybe (constructHalfSpaceOn h) [h1,h2] of
            Left _h'           -> error "infeasible" -- TODO
            Right constraints -> case NonEmpty.nonEmpty constraints of
              Nothing          -> error "absurd: unbounded?"
              Just constraints' -> case lp1D constraints' of
                InFeasible1 _ _         -> error "infeasible" -- TODO
                Feasible1 (_ :+ (q,h')) -> Basis2 q h h'
                Unbounded                              -> error "absurd: unbounded?"

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
         testIpe [osp|LPType/linearProgramming2_simpl.ipe|]

         -- it "extract" $
         --   extract 3 (NonEmpty.fromList "foobarenzo") `shouldBe` ('b',"fooarenzo")
         -- prop "extract1" $
         --   extract1 (mkStdGen 42) (NonEmpty.fromList "foobarenzo") "foo" ===  ('n',"foobarezo")


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
          (halfPlanes, solution) <- runIO $ loadInputs inFp

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


          let (sol,basis) = subExp (mkStdGen 42) $ linearProgrammingMinY (view core <$> halfPlanes)
          it ("subExp correct, " <> show sol) $ do
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
