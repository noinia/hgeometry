{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Compose where

import Data.Singletons
import Data.Singletons.TH
import Data.Vinyl

--------------------------------------------------------------------------------

data Uni1 = A | B | C

type family ElF1 (f :: Uni1) where
  ElF1 A = Int
  ElF1 B = String
  ElF1 C = Bool

data ElF1Sym0 :: (TyFun Uni1 *) -> *
type instance Apply ElF1Sym0 f = ElF1 f
type ElF1Sym1 f = ElF1 f

----------------------------------------

data Uni2 = D | E

type family ElF2 (f :: Uni2) where
   ElF2 D = Int
   ElF2 E = String


data ElF2Sym0 :: (TyFun Uni2 *) -> *
type instance Apply ElF2Sym0 f = ElF2 f
type ElF2Sym1 f = ElF2 f

----

genSingletons [ ''Uni1, ''Uni2 ]

----------------------------------------

type family EitherElF (lElF :: TyFun lu * -> *)
                      (rElF :: TyFun ru * -> *)
                      (fld :: Either lu ru) where
  EitherElF lElF rElF (Left l)  = Apply lElF l
  EitherElF lElF rElF (Right r) = Apply rElF r


-- Note that the following easier variant does not work, since we cannot pass
-- around partially applied type families.

-- type family EitherElF (lf :: l -> *) (rf :: r -> *) (fld :: Either l r) where
--   EitherElF lf rf (Left l)  = lf l
--   EitherElF lf rf (Right r) = rf r

--------------------------------------------------------------------------------

str :: EitherElF ElF1Sym0 ElF2Sym0 (Right E)
str = "frank"

--------------------------------------------------------------------------------

newtype EitherAttr (lElF :: TyFun lu * -> *)
                   (rElF :: TyFun ru * -> *)
                   (fld :: Either lu ru) =
  EitherAttr { _unEitherAttr :: EitherElF lElF rElF fld }


class Assign either where
  (=:) :: sing f -> EitherElF lElF rElF (either f) -> EitherAttr lElF rElF (either f)
  _fld =: x = EitherAttr x

instance Assign Left where
instance Assign Right where

frank :: Rec (EitherAttr ElF1Sym0 ElF2Sym0) [Left A,Left C,Right E]
frank =  (SA =: 5)
      :& (SC =: True)
      :& (SE =: "frank")
      :& RNil
