{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Properties
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Defines some generic geometric properties e.g. Dimensions, NumType, and
-- Intersection types.
--
--------------------------------------------------------------------------------
module Data.Geometry.Properties( module Data.Intersection
                               , Dimension
                               , NumType
                               ) where

import GHC.TypeLits
import Data.Ext
import Data.Intersection
import Data.Range

-------------------------------------------------------------------------------

-- | A type family for types that are associated with a dimension. The
-- dimension is the dimension of the geometry they are embedded in.
type family Dimension t :: Nat

-- | A type family for types that have an associated numeric type.
type family NumType t :: *

--------------------------------------------------------------------------------


type instance NumType   (core :+ ext) = NumType   core
type instance Dimension (core :+ ext) = Dimension core

type instance NumType [t] = NumType t
type instance NumType (Range a) = a





-- type IsAlwaysTrueFromEither a b = (VTL.RIndex b [a,b] ~ ((VTL.S VTL.Z)))
--                                   -- VTL.RIndex b [a,b] ~ ((VTL.S VTL.Z))(b  ∈ [a,b])

-- -- | Convert an either to a CoRec. The type class constraint is silly, and is
-- -- triviall true. Somehow GHC does not see that though.
-- fromEither           :: IsAlwaysTrueFromEither a b => Either a b -> CoRec Identity [a,b]
-- fromEither (Left x)  = coRec x
-- fromEither (Right x) = coRec x


-- -- fromEither'           :: ( RElem b [a,b] ((VTL.S VTL.Z))
-- --                          ) => Either a b -> CoRec Identity [a,b]

-- fromEither'           :: ( VTL.RIndex b [a,b] ~ ((VTL.S VTL.Z))
-- --                           VTL.RIndex b '[b] ~ VTL.Z
--                          ) => Either a b -> CoRec Identity [a,b]
-- fromEither' (Left x)  = coRec x
-- fromEither' (Right x) = coRec x

-- type family Union g h :: *

-- class IsUnionableWith g h where
--   union :: g -> h -> Union g h
