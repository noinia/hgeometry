{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Layers
  ( LayerStatus(..)
  , toggleLayerStatus

  , Layer(Layer)
  , HasLayer(..)

  , Layers(Layers)
  , HasLayers(..)
  , theLayers

  , allLayers
  ) where

import           Control.Lens hiding (view, element, (<|))
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup (sconcat)
import qualified Data.Sequence as Seq
import           Miso
import           Miso.String (MisoString,ToMisoString(..), ms)

--------------------------------------------------------------------------------

data LayerStatus = Hidden | Visible
  deriving (Show,Eq)

toggleLayerStatus :: LayerStatus -> LayerStatus
toggleLayerStatus = \case
  Hidden  -> Visible
  Visible -> Hidden

type LayerIx = MisoString

data Layer = Layer { _name        :: LayerIx
                   , _layerStatus :: LayerStatus
                   }
             deriving (Show,Eq)
makeClassy ''Layer

data Layers = Layers { _beforeActive :: Seq.Seq Layer
                     , _activeLayer  :: Layer
                     , _afterActive  :: Seq.Seq Layer
                     }
            deriving (Show,Eq)

theLayers                              :: Layers -> Seq.Seq Layer
theLayers (Layers before active after) = before <> Seq.singleton active <> after

makeClassy ''Layers


type instance Index   Layers = LayerIx
type instance IxValue Layers = Layer


findX     :: (a -> Bool) -> Traversal' (Seq.Seq a) a
findX p f = traverse $ \x -> if p x then f x else pure x
-- I kind of want to report whether the predicate matched something as well

instance Ixed Layers where
  -- | this implementation somewhat sucks
  ix i f (Layers before active after)
    | i == active^.name = (\a -> Layers before a after) <$> f active
    | otherwise         = Layers <$> findX (\l -> l^.name == i) f before
                                 <*> pure active
                                 <*> findX (\l -> l^.name == i) f after

--------------------------------------------------------------------------------





--------------------------------------------------------------------------------

instance ToMisoString Integer where
  toMisoString = toMisoString . show


-- | Infinite sequence of layers
allLayers :: NonEmpty Layer
allLayers = fmap (flip Layer Visible) allLayerNames

allLayerNames :: NonEmpty MisoString
allLayerNames = sconcat $ greekAlphabet
                        <| NonEmpty.zipWith (\i -> fmap (<> ms i))
                                            (NonEmpty.fromList [(1 :: Integer)..])
                                            (NonEmpty.repeat greekAlphabet)

greekAlphabet :: NonEmpty MisoString
greekAlphabet = NonEmpty.fromList
  ["alpha","beta","gamma","delta","epsilon","zeta","eta","theta","iota","kappa","lambda","mu","nu","xi","omicron","pi","rho","sigma","tau","upsilon","phi","chi","psi","omega"]
