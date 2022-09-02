{-# LANGUAGE TemplateHaskell #-}
module Algorithms.Geometry.SoS.Expr where

import           Control.Lens
import qualified Data.List as List

--------------------------------------------------------------------------------

data Expr v r = Constant r
              | Negate (Expr v r)
              | Sum  [Expr v r]
              | Prod [Expr v r]
              | Var v
              deriving (Show,Eq)
makePrisms ''Expr


foldExpr :: (r -> b) -> (b -> b) -> ([b] -> b) -> ([b] -> b) -> (v -> b) -> Expr v r -> b
foldExpr con' neg' sum' prod' var' = go
  where
    go = \case
      Constant c -> con' c
      Negate e   -> neg'  $ go e
      Sum es     -> sum'  $ map go es
      Prod es    -> prod' $ map go es
      Var v      -> var' v

-- | Test if the expression has any variables.
hasVariables :: Expr v r -> Bool
hasVariables = foldExpr (const False)
                        id
                        or
                        or
                        (const True)

instance (Num r) => Num (Expr i r) where
  fromInteger = Constant . fromInteger
  abs _ = error "'abs' not defined for Algorithms.Geometry.SoS.Expr.Expr"
  signum _ = error "'signum' not defined for Algorithms.Geometry.SoS.Expr.Expr"
  negate      = \case
    Negate e -> e
    e        -> Negate e

  (Sum es) + (Sum es') = Sum $ es <> es'
  (Sum es) + e         = Sum (e:es)
  e        + (Sum es)  = Sum (e:es)
  e        + e'        = Sum [e,e']

  (Prod es) * (Prod es') = Prod $ es <> es'
  (Prod es) * e          = Prod (e:es)
  e         * (Prod es)   = Prod (e:es)
  e         * e'          = Prod [e,e']


simplify :: (Num r, Eq r) => Expr v r -> Expr v r
simplify = \case
  Prod es  -> case filter (isn't $ _Constant.only 1) es of
                []  -> Constant 1
                es' -> Prod $ map simplify es'
  Sum  es  -> case filter (isn't $ _Constant.only 0) es of
                []  -> Constant 0
                es' -> Sum $ map simplify es'
  Negate e -> Negate $ simplify e
  e        -> e

prettyP :: (Show r, Show v) => Expr v r -> String
prettyP = \case
  Constant c  -> show c
  Negate e    -> "(-1)*(" <> prettyP e <> ")"
  Prod es     -> mconcat [ "("
                            , List.intercalate ")*(" (prettyP <$> es)
                            , ")"
                            ]
  Sum es     -> mconcat [ "("
                        , List.intercalate ") + (" (prettyP <$> es)
                        , ")"
                        ]
  Var v -> show v
