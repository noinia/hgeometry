{-# LANGUAGE OverloadedStrings          #-}
module Miso.Bulma.Generic where

import           Miso
import           Miso.String (MisoString, ToMisoString(..), ms)

--------------------------------------------------------------------------------

useBulmaRemote :: View action
useBulmaRemote = div_ [] [bulmaLink,iconLink]

bulmaLink :: View action
bulmaLink = link_ [ rel_ "stylesheet"
                   , href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.5/css/bulma.min.css"
                   , textProp "integrity" "sha256-vK3UTo/8wHbaUn+dTQD0X6dzidqc5l7gczvH+Bnowwk="
                   , textProp "crossorigin" "anonymous"
                   ]

iconLink :: View action
iconLink = Miso.script_ [ src_ "https://use.fontawesome.com/releases/v5.3.1/js/all.js"
                        , defer_ "true"
                        ] []

--------------------------------------------------------------------------------


icon    :: MisoString -> View action
icon cs = i_ [ class_ cs, textProp "aria-hidden" "true"] []
