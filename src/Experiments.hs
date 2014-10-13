{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Experiments where

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

-- newtype Attr1 f = Attr1 { _unAttr1 :: ElF1 f }

-- instance Show (Attr1 A)  where show (Attr1 x) = "A: " ++ show x
-- instance Show (Attr1 B)  where show (Attr1 x) = "B: " ++ show x
-- instance Show (Attr1 C)  where show (Attr1 x) = "C: " ++ show x

----------------------------------------

data Uni2 = D | E

type family ElF2 (f :: Uni2) where
   ElF2 D = Int
   ElF2 E = String


data ElF2Sym0 :: (TyFun Uni2 *) -> *
type instance Apply ElF2Sym0 f = ElF2 f
type ElF2Sym1 f = ElF2 f


-- -- newtype Attr2 f = Attr2 { _unAttr2 :: ElF2 f }

-- -- instance Show (Attr2 D)  where show (Attr2 x) = "D: " ++ show x
-- -- instance Show (Attr2 E)  where show (Attr2 x) = "E: " ++ show x

----------------------------------------

type family EitherElF (lElF :: TyFun lu * -> *)
                      (rElF :: TyFun ru * -> *)
                      (fld :: Either lu ru) where
  EitherElF lElF rElF (Left l)  = Apply lElF l
  EitherElF lElF rElF (Right r) = Apply rElF r


-- type family EitherElF (lf :: l -> *) (rf :: r -> *) (fld :: Either l r) where
--   EitherElF lf rf (Left l)  = lf l
--   EitherElF lf rf (Right r) = rf r



-- type ElFBoth (f :: Either Uni1 Uni2) = EitherElF ElF1 ElF2 f


type family ElFBoth (fld :: Either Uni1 Uni2) where
  ElFBoth (Left l) = ElF1 l
  ElFBoth (Right r) = ElF2 r


--------------------------------------------------------------------------------

genSingletons [ ''Uni1, ''Uni2 ]

str :: ElFBoth (Right E)
str = "frank"


str' :: EitherElF ElF1Sym0 ElF2Sym0 (Right E)
str' = "frank"


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



--------------------------------------------------------------------------------

-- newtype EitherAttr (lf :: l -> *) (rf :: r -> *) (fld :: Either l r) =
--   EitherAttr { _unEitherAttr :: EitherElF lf rf fld }

-- class Assign either where
--   (=:) :: sing f -> EitherElF lf rf (either f) -> EitherAttr lf rf (either f)


-- instance Assign Left where
--   f =: x = EitherAttr x

-- instance Assign Right where
--   f =: x = EitherAttr x


-- frank :: Rec (EitherAttr ElF1 ElF2) [Left A,Left C,Right E]
-- frank =  (SA =: AA 5)
--       :& (SC =: CC True)
--       :& (SE =: EE "frank")
--       :& RNil


newtype AttrBoth f = AttrBoth {_unAttrBoth :: ElFBoth f }

-- class Assign eitherC where
--   (=:) :: sing u -> ElFBoth (eitherC u) -> AttrBoth (eitherC u)
--   f =: x = AttrBoth x

-- instance Assign Left
-- instance Assign Right

-- frank :: Rec AttrBoth [Left A,Left C,Right E]
-- frank =  (SA =: 5)
--       :& (SC =: True)
--       :& (SE =: "frank")
--       :& RNil

















-- -- type family EitherUni (uproxy :: KProxy u) (f :: u) :: * where
-- --   EitherUni ::









-- -- instance Show (lf l) => Show (EitherAttr lf rf (Left l)) where (EitherAttr x) = show x







-- -- type family ElFBoth (f :: Either Uni1 Uni2) where
-- --   ElFBoth (Left l)  = ElF1 l
-- --   ElFBoth (Right r) = ElF2 r



-- -- type family UnionF (l :: * -> *) (r :: * -> *) (t :: Either  where
-- --   UnionF


-- -- newtype AttrBoth f = AttrBoth { _unAttrBoth :: ElFBoth f }

-- -- makeLenses ''AttrBoth

-- -- instance Show (AttrBoth (Left A))  where show (AttrBoth x) = "A: " ++ show x
-- -- instance Show (AttrBoth (Left B))  where show (AttrBoth x) = "B: " ++ show x
-- -- instance Show (AttrBoth (Left C))  where show (AttrBoth x) = "C: " ++ show x
-- -- instance Show (AttrBoth (Right D)) where show (AttrBoth x) = "D: " ++ show x
-- -- instance Show (AttrBoth (Right E)) where show (AttrBoth x) = "E: " ++ show x

-- -- (=:) :: sing f -> ElFBoth f -> AttrBoth f
