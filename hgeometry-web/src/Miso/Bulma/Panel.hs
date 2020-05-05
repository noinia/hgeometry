{-# LANGUAGE OverloadedStrings          #-}
module Miso.Bulma.Panel where

import           Miso
import           Miso.String (MisoString)
import           Miso.Bulma.Generic

--------------------------------------------------------------------------------

panel :: [View action] -> View action
panel = div_ [class_ "panel is-full"]

panelBlock :: [View action] -> View action
panelBlock = div_ [class_ "panel-block"]

panelIcon   :: MisoString -> View action
panelIcon i = span_ [ class_ "panel-icon"]
                    [ icon i]
