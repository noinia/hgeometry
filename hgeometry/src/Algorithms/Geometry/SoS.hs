--------------------------------------------------------------------------------
-- |
-- Module      :  Algorithms.Geometry.SoS
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Implementation of
-- Simulation of Simplicity: A Technique to Cope with Degenerate Cases in Geometric Algorithms
--
-- By
-- Herbert Edelsbrunner and Ernst Peter Mucke
--
--------------------------------------------------------------------------------
module Algorithms.Geometry.SoS
  ( module Algorithms.Geometry.SoS.Sign
  , module Algorithms.Geometry.SoS.Orientation
  , module Algorithms.Geometry.SoS.Determinant
  ) where

-- import Algorithms.Geometry.SoS.Internal
import Algorithms.Geometry.SoS.Orientation
import Algorithms.Geometry.SoS.Determinant
import Algorithms.Geometry.SoS.Sign

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------



-- sideTest'             :: ( SoS p, Dimension p ~ 2, r ~ NumType p
--                          , Eq r, Num r
--                          ) => [p] -> Sign
-- sideTest' (q:p1:p2:_) = sideTest q (Vector2 p1 p2)






--------------------------------------------------------------------------------


----------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------





-- instance (i `CanAquire` Point d r, Arity d) => P i d r `CanAquire` Point d (R i) where
--   aquire (P i) = Point $ pure ()




--------------------------------------------------------------------------------



--------------------------------------------------------------------------------


--------------------------------------------------------------------------------


-- -- TODO: Remove this one
-- instance HasIndex (Point d r :+ Int) where
--   indexOf = view extra


-- test1 :: Sign
-- test1 = sideTest (Point1 1 :+ 0 :: Point 1 Int :+ Int) (Vector1 $ Point1 5 :+ 1)

-- test2 :: Sign
-- test2 = sideTest (Point1 5 :+ 0 :: Point 1 Int :+ Int) (Vector1 $ Point1 5 :+ 1)


-- test3 :: Sign
-- test3 = sideTest (Point2 (-1) 5 :+ 0 :: Point 2 Int :+ Int) (Vector2 (Point2 0 0  :+ 1)
--                                                                      (Point2 0 10 :+ 2)
--                                                             )


-- pattern Point1 x = Point (Vector1 x)


-- testV :: Sign
-- testV = simulateSimplicity sideTest' [ Point2 (-1) 5
--                                      , Point2 0 0
--                                      , Point2 0 10
--                                      ]





--------------------------------------------------------------------------------








-- cmpSignificance                   :: Ord k => Bag k -> Bag k -> Ordering
-- cmpSignificance (Bag e1) (Bag e2) = e1e2 `compare` e2e1
--   where
--     e1e2 = fmap fst . Map.lookupMax $ e1 `Map.difference` e2
--     e2e1 = fmap fst . Map.lookupMax $ e2 `Map.difference` e1



-- -- | Represents a Sum of terms, i.e. a value that has the form:
-- --
-- -- \[
-- --   \sum c \Pi_{(i,j)} \varepsilon(i,j)
-- -- \]
-- newtype Symbolic i j r = Symbolic [Term i j r] deriving (Show,Eq,Functor)

-- instance (Ord i, Ord j, Num r) => Num (Symbolic i j r) where
--   (Symbolic ts) + (Symbolic ts') = Symbolic (ts `addTerms` ts')
--   negate = fmap negate
--   (Symbolic ts) * (Symbolic ts') = Symbolic $ multiplyTerms ts ts'
--   fromInteger x = constant (fromInteger x)
--   -- abs x | signum x == -1 = (-1)*x
--   --       | oterwise       = x

--   -- signum = undefined










-- -- | Adds two lists of terms
-- addTerms        :: forall i j r. (Ord i, Ord j, Num r)
--                 => [Term i j r] -> [Term i j r] -> [Term i j r]
-- addTerms ts ts' = (\(eps,c) -> Term c eps) <$> Map.toList m
--   where
--     m :: Map.Map (EpsFold i j) r
--     m = Map.fromListWith (+) [ (eps,c) | (Term c eps) <- ts <> ts' ]

-- multiplyTerms        :: forall i j r. (Ord i, Ord j, Num r)
--                      => [Term i j r] -> [Term i j r] -> [Term i j r]
-- multiplyTerms ts ts' = (\(eps,c) -> Term c eps) <$> Map.toList m
--   where
--     m :: Map.Map (EpsFold i j) r
--     m = Map.fromListWith (+) [ (es <> es',c*d) | (Term c es) <- ts, (Term d es') <- ts' ]




-- orderedTerms               :: (Ord i, Ord j) => Symbolic i j r -> [Term i j r]
-- orderedTerms (Symbolic ts) = List.sortBy (\(Term _ e1) (Term _ e2) -> cmpSignificance e1 e2) ts

















  -- zipWith (\j x -> Term x $ singleton (i,j)) [0..] . toList






-- orderTerms               :: (Ord i, Ord j) => Symbolic i j r -> Symbolic i j r
-- orderTerms (Symbolic ts) = Symbolic $ List.sortBy cmpSignificance ts



-- fromPoint'   :: Foldable f => i -> f r -> Symbolic i Int r
-- fromPoint' i = Symbolic . zipWith (\j x -> Term x [(i,j)]) [0..] . toList



-- testZ :: Symbolic Int Int Int
-- testZ = (5 + 6) *





  --   case sign i of
  --                   (-1) -> Negative $ fromInteger i
  --                   0    -> Zero
  --                   _    -> Positive $ fromInteger i
  -- negate        = \case
  --   Negative c -> Positive c
  --   Positive c -> Negative c


-- newtype N = N String deriving (Show,Eq)


-- instance Num N where
--   (N x) + (N y) = N $ x <> "+" <> y
--   (N x) * (N y) = N $ x <> y
--   negate  (N x) = N ("negate(" <> x <> ")")
--   fromInteger = N . show


-- n       :: (Ord i, Ord j) => String -> i -> j -> Symbolic i j N
-- n x i j = Symbolic [Term (N x) mempty, Term 1 (singleton (i,j))]





-- testM3 = det33 $ V3 (fromPoint' [N "px", N "py"] <> 1)
--                     (fromPoint' [N "px", N "py"] <> 1)
--    (fromPoint' [N "px", N "py"] <> 1)
-- -- (V3 (N "qx") (N "qy") 1)
-- --                     (V3 (N "rx") (N "ry") 1)
