{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
module Data.TypeLevel.Filter where

import Data.Proxy
import Data.Vinyl
--------------------------------------------------------------------------------

-- data Fields = A | B

-- type family ElF (fld :: Fields) :: * where
--   ElF A = Int
--   ElF B = String

-- newtype ElfA f = EA { unA :: ElF f }

-- instance Show (ElfA A) where show (EA x) = "A: " ++ show x
-- instance Show (ElfA B) where show (EA x) = "B: " ++ show x

-- r :: Rec ElfA [A,B,A,B]
-- r = EA 5 :& EA "foo" :& EA 1 :& EA "bar" :& RNil

--------------------------------------------------------------------------------

class FilterRec (rs :: [u]) (fld :: u) where
  filterRec :: Proxy fld -> Rec f rs -> [f fld]

-- The version that simply returns a [f fld] works. Trying to return a
-- (filtered) Rec though, because Ideally I would want to apply an arbitrary
-- predicate on the field (fld) to determine if it should be included or not.

instance FilterRec '[] fld where
  filterRec _ RNil = []

instance FilterRec rs fld => FilterRec (fld ': rs) fld where
  filterRec pf (r :& rs) = r : filterRec pf rs

instance FilterRec rs fld => FilterRec (r ': rs) fld where
  filterRec pf (_ :& rs) = filterRec pf rs

-- r' :: [ElfA A]
-- r' = filterRec Proxy r

-- r'' :: [ElF A]
-- r'' = map unA . filterRec (Proxy :: Proxy A) $ r
