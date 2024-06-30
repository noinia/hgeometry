{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Layers
  ( LayerStatus(..)
  , toggleLayerStatus

  , Layer(Layer)
  , HasLayer(..)

  , Layers
  , HasLayers(..)
  , theLayers
  , addLayer

  , allLayers
  , initialLayers
  ) where

import           Control.Lens hiding (view, element, (<|))
import qualified Data.List.Infinite as Inf
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

data Layers = Layers { _beforeActive    :: Seq.Seq Layer
                     , _activeLayer     :: Layer
                     , _afterActive     :: Seq.Seq Layer
                     , _availableLayers :: Inf.Infinite Layer
                     -- ^ the remaining available layers, by definition this is
                     -- allLayers with all before, active, and after layers removed
                     }

instance Eq Layers where
  (Layers before active after _) == (Layers before' active' after' _) =
    before == before' && active == active' && after == after'
  -- we ignore the availableLayers, since by def they must be the same

instance Show Layers where
  show (Layers before active after _) = "Layers " <> show before
                                                  <> " " <> show active
                                                  <> " " <> show after

theLayers                                :: Layers -> Seq.Seq Layer
theLayers (Layers before active after _) = before <> Seq.singleton active <> after

makeClassy ''Layers


type instance Index   Layers = LayerIx
type instance IxValue Layers = Layer


findX     :: (a -> Bool) -> Traversal' (Seq.Seq a) a
findX p f = traverse $ \x -> if p x then f x else pure x
-- I kind of want to report whether the predicate matched something as well

instance Ixed Layers where
  -- | this implementation somewhat sucks
  ix i f (Layers before active after remaining)
    | i == active^.name = (\a -> Layers before a after remaining) <$> f active
    | otherwise         = Layers <$> findX (\l -> l^.name == i) f before
                                 <*> pure active
                                 <*> findX (\l -> l^.name == i) f after
                                 <*> pure remaining

--------------------------------------------------------------------------------





--------------------------------------------------------------------------------

instance ToMisoString Integer where
  toMisoString = toMisoString . show


-- | Infinite sequence of layers
allLayers :: Inf.Infinite Layer
allLayers = fmap (flip Layer Visible) allLayerNames

allLayerNames :: Inf.Infinite MisoString
allLayerNames = Inf.concat $
     greekAlphabet Inf.:< Inf.zipWith (\i -> fmap (<> ms i))
                                      (Inf.iterate' succ (1 :: Integer))
                                      (Inf.repeat greekAlphabet)

greekAlphabet :: NonEmpty MisoString
greekAlphabet = NonEmpty.fromList
  ["alpha","beta","gamma","delta","epsilon","zeta","eta","theta","iota","kappa","lambda","mu","nu","xi","omicron","pi","rho","sigma","tau","upsilon","phi","chi","psi","omega"]


--------------------------------------------------------------------------------

-- | Adds a layer to our layers
addLayer     :: Layers -> Layers
addLayer lrs = let (newLayer, available) = Inf.uncons $ lrs^.availableLayers
               in lrs&afterActive %~ (|> newLayer)
                     &availableLayers .~ available

--------------------------------------------------------------------------------

-- | The initial value to use for layers
initialLayers :: Layers
initialLayers = let (initLayer, available) = Inf.uncons allLayers
                in Layers mempty initLayer mempty available
