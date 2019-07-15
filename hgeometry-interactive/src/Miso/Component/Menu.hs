{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Miso.Component.Menu where

import           Control.Lens hiding (view, element)
import           Miso
import           Miso.String (MisoString)

--------------------------------------------------------------------------------

data MenuChild action = MenuChild { _childLabel :: MisoString
                                  , _childAction :: Maybe action
                                  }
makeLenses ''MenuChild

data MenuItem action = MenuItem { _label    :: MisoString
                                , _action   :: Maybe action
                                , _children :: [MenuChild action]
                                }
makeLenses ''MenuItem

newtype Menu action = Menu {_items :: [MenuItem action]}
makeLenses ''Menu

--------------------------------------------------------------------------------

menu   :: Menu action -> View action
menu m = nav_ [ class_ "navbar"
              , textProp "role" "navigation"
              , textProp "aria-label" "main navigation"
              ]
              [ div_ [class_ "navbar-brand"]
                     [a_ [ class_ "navbar-item"]
                         [ text "Brand"]
                     ]
              , div_ [class_ "navbar-menu"]
                     [div_ [class_ "navbar-start"]
                           [level0 c | c <- chs]
                     , div_ [class_ "navbar-end"]
                            [div_ [class_ "navbar-item"]
                                  [text "right!"]
                            ]
                     ]
              ]
  where
    Menu chs = m


withAction    :: Maybe action -> [Attribute action] -> [Attribute action]
withAction ma = maybe id (\a -> (onClick a:)) ma


level0 :: MenuItem action -> View action
level0 (MenuItem l ma chs') = case chs' of
    []  -> a_   (withAction ma [ class_ "navbar-item"])
                [ text l]
    chs -> div_ (withAction ma [class_ "navbar-item has-dropdown is-hoverable"])
                [ Miso.a_ [ class_ "navbar-link"]
                          [text l]
                , div_    [class_ "navbar-dropdown"]
                          [level1 c | c <- chs]
                ]

level1                  :: MenuChild action -> View action
level1 (MenuChild l ma) = a_ (withAction ma [class_ "navbar-item"])
                             [text l]
