{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Data.TypeLevel.Partition where


-- Warning: Uses the vinyl stuff from the new-vinyl (aka upcoming Vinyl 0.5) branch
import Data.Vinyl

import Data.Promotion.Prelude.Eq
import Data.Promotion.Prelude.List
import Data.Proxy

import Data.Singletons

--------------------------------------------------------------------------------

data Fields = A | B

type family ElF (fld :: Fields) :: * where
  ElF A = Int
  ElF B = String

newtype ElfA f = EA { unA :: ElF f }

instance Show (ElfA A) where show (EA x) = "A: " ++ show x
instance Show (ElfA B) where show (EA x) = "B: " ++ show x

r :: Rec ElfA [A,B,A,B]
r = EA 5 :& EA "foo" :& EA 1 :& EA "bar" :& RNil

--------------------------------------------------------------------------------

class FilterRec (rs :: [u]) (fld :: u) where
  filterRec :: Proxy fld -> Rec f rs -> Rec f (Filter ((:==$$) fld) rs)
                         -- [f fld]

-- The version that simply returns a [f fld] works. Trying to return a
-- (filtered) Rec though, because Ideally I would want to apply an arbitrary
-- predicate on the field (fld) to determine if it should be included or not.

instance FilterRec '[] fld where
  filterRec _ RNil = RNil

instance FilterRec rs fld => FilterRec (fld ': rs) fld where
  filterRec pf (r :& rs) = r :& filterRec pf rs

-- instance FilterRec rs fld => FilterRec (r ': rs) fld where
--   filterRec (_ :& rs) = filterRec rs

-- r' :: [ElfA A]
-- r' = filterRec r


--- This produces the following error message:

{-#

src/Data/TypeLevel/Partition.hs:52:28-47: Could not deduce (Data.Promotion.Prelude.List.Case_1628081500 …
                        ((:==$$) r) r rs1 ((:==$$) r) (r : rs1) (r :== r)
                      ~ (r : Filter ((:==$$) r) rs1))
    from the context (FilterRec rs fld)
      bound by the instance declaration
      at /Users/frank/workspace/hs-projects/hgeometry/src/Data/TypeLevel/Partition.hs:51:10-54
    or from ((fld : rs) ~ (r : rs1))
      bound by a pattern with constructor
                 :& :: forall (k :: BOX) (f :: k -> *) (r :: k) (rs :: [k]).
                       f r -> Rec f rs -> Rec f (r : rs),
               in an equation for ‘filterRec’
      at /Users/frank/workspace/hs-projects/hgeometry/src/Data/TypeLevel/Partition.hs:52:17-23
    Expected type: Rec f (Filter ((:==$$) fld) (fld : rs))
      Actual type: Rec f (r : Filter ((:==$$) r) rs1)
    Relevant bindings include
      rs :: Rec f rs1
        (bound at /Users/frank/workspace/hs-projects/hgeometry/src/Data/TypeLevel/Partition.hs:52:22)
      r :: f r
        (bound at /Users/frank/workspace/hs-projects/hgeometry/src/Data/TypeLevel/Partition.hs:52:17)
      pf :: Proxy fld
        (bound at /Users/frank/workspace/hs-projects/hgeometry/src/Data/TypeLevel/Partition.hs:52:13)
      filterRec :: Proxy fld
                   -> Rec f (fld : rs) -> Rec f (Filter ((:==$$) fld) (fld : rs))
        (bound at /Users/frank/workspace/hs-projects/hgeometry/src/Data/TypeLevel/Partition.hs:52:3)
    In the expression: r :& filterRec pf rs
    In an equation for ‘filterRec’:
        filterRec pf (r :& rs) = r :& filterRec pf rs
    In the instance declaration for ‘FilterRec (fld : rs) fld’
Compilation failed.

-#}
