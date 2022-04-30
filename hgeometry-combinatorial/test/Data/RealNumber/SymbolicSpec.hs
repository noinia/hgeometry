module Data.RealNumber.SymbolicSpec where

import Data.Traversable
import Data.Indexed
import Data.RealNumber.Symbolic
import GHC.Generics
import System.Random
import System.Random.Stateful
import Test.Hspec
import Test.QuickCheck

--------------------------------------------------------------------------------

spec :: Spec
spec = do epsSpec
          termSpec
          symbolicSpec

epsSpec :: Spec
epsSpec = describe "Eps-fold tests" $ do
    it "eval consistent" $ property $ \(SmallInt e1) (SmallInt e2) ->
      let ef1   = mkEpsFold [e1]
          ef2   = mkEpsFold [e2]
          delta = (1/2)
          d     = 2
      in evalEps d delta (ef1 <> ef2) == evalEps d delta ef1 * evalEps d delta ef2
    it "order correct" $ property $ \(NonNegative i) ->
      (mempty :: EpsFold Int) > mkEpsFold [i]


termSpec :: Spec
termSpec = pure ()
-- termSpec = describe "Term Tests" $ do
--     it "Ord < " $ property $ \a b (NonNegative i) (NonNegative j) -> a < b ==>
--       term' a i < term' b j `shouldBe` True
  -- this property is not actually true!

symbolicSpec :: Spec
symbolicSpec = describe "Symbolic Tests" $ do
    it "Ord < " $ property $ \a b (NonNegative i) (NonNegative j) -> a < b ==>
      perturb' a i < perturb b j `shouldBe` True
    it "eval exprs" $ property $ \(eI :: Expr Integer) ->
      let e = perturbAll eI
      in roundToConstant (eval e)
         `shouldBe`
         eval eI

--------------------------------------------------------------------------------

newtype SmallInt = SmallInt Int deriving (Show,Eq,Ord,Num,Enum,Real,Integral)

instance Bounded SmallInt where
  minBound = 0
  maxBound = 3
instance Arbitrary SmallInt where
  arbitrary = arbitrarySizedBoundedIntegral

--------------------------------------------------------------------------------

symbolic' :: Integer -> Index -> Symbolic Index Integer
symbolic' = symbolic

perturb' :: Integer -> Index -> Symbolic Index Integer
perturb' = perturb

term' :: Integer -> Index -> Term Index Integer
term' = term



--------------------------------------------------------------------------------

data Op = Plus | Min | Times deriving (Show,Eq,Enum,Generic)

instance Uniform Op
instance UniformRange Op where
  uniformRM (l,u) gen = toEnum <$> uniformRM (fromEnum l, fromEnum u) gen
instance Random Op

evalOp :: Num a => Op -> (a -> a -> a)
evalOp = \case
  Plus  -> (+)
  Min   -> (-)
  Times -> (*)


instance Arbitrary Op where
  arbitrary = chooseAny

data Expr a = Const a
            | BinOp Op (Expr a) (Expr a)
            deriving (Show,Eq,Functor)

instance Foldable Expr where
  foldMap = foldMapDefault
instance Traversable Expr where
  traverse f = go
    where
      go = \case
        Const x        -> Const <$> f x
        BinOp op e1 e2 -> BinOp op <$> go e1 <*> go e2

type IntExpr = Expr Integer

instance Arbitrary a => Arbitrary (Expr a) where
  arbitrary = frequency [ (2, Const <$> arbitrary)
                        , (1, BinOp <$> arbitrary <*> arbitrary <*> arbitrary)
                        ]

eval :: Num a => Expr a -> a
eval = \case
  Const c        -> c
  BinOp op e1 e2 -> evalOp op (eval e1) (eval e2)



-- | Perturbs all elements with a unique offset
perturbAll :: Num r => Traversable t => t r -> t (Symbolic Index r)
perturbAll = labelWith (flip perturb)
