{-# LANGUAGE  TemplateHaskell  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Plane.LowerEnvelope.Separator.Type
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Separator for ap lanar graph
--
--------------------------------------------------------------------------------
module HGeometry.Plane.LowerEnvelope.Connected.Separator.Type
  ( Separator(Separator), separator, inside, outside
  ) where

import Control.Lens hiding (inside,outside)

--------------------------------------------------------------------------------

-- | A separator
data Separator a = Separator { _separator :: a
                             , _inside    :: a
                             , _outside   :: a
                             }
                 deriving (Show,Eq,Functor,Foldable,Traversable)

makeLenses ''Separator

instance Semigroup a => Semigroup (Separator a) where
  (Separator sep is os) <> (Separator sep' is' os') =
    Separator (sep <> sep') (is <> is') (os <> os')

instance (Monoid a) => Monoid (Separator a) where
  mempty = Separator mempty mempty mempty
