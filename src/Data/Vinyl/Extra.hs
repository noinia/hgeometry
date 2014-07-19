{-# LANGUAGE UndecidableInstances #-}    -- Def showable
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Vinyl.Extra where

import Data.Vinyl
import Data.Vinyl.Idiom.Identity

import Data.Vinyl.Universe.Geometry

import Data.Type.List


--------------------------------------------------------------------------------


-- $setup
-- >>> :{
--  let x    = SNatField :: SDField 1
--      y    = SNatField :: SDField 2
--      name = SSymField :: SSField "name" String
-- :}



type Showable r fs = RecAll (TElField r) Identity fs Show






-- | Type class that allows us to split a Vinyl Record based on types. I.e.
--
-- >>> :{
-- let
--   s :: (PlainTRec Int '[DField 1], PlainTRec Int '["name" :~> String])
--   s = splitRec $ x =: 10 <+> name =: "foo"
--   (a,b) = s
-- in (rshow a, rshow b)
-- :}
-- ("{ axis_1 =: 10 }","{ name =: \"foo\" }")
class Split (xs :: [*]) (ys :: [*]) where
  splitRec :: Rec el f (xs ++ ys) -> (Rec el f xs, Rec el f ys)

instance Split '[] ys where
  splitRec r = (RNil,r)

instance Split xs ys => Split (x ': xs) ys where
  splitRec (r :& rs) = let (rx,ry) = splitRec rs
                       in (r :& rx, ry)
