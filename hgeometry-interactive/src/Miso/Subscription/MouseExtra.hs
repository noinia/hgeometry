{-# LANGUAGE OverloadedStrings          #-}
module Miso.Subscription.MouseExtra where


import Control.Monad.IO.Class
import GHCJS.Marshal
import JavaScript.Object
import JavaScript.Object.Internal

import Miso
import Miso.String(MisoString)

import Debug.Trace

relativeMouseSub        :: MisoString -> ((Int,Int) -> action) -> Sub action
relativeMouseSub elemId f = \sink -> do
    elem <- getElementById elemId
    addEventListener elem "mousemove" $
      \mouseEvent -> do
        alert elemId
        Just x <- fromJSVal =<< getProp "clientX" (Object mouseEvent)
        Just y <- fromJSVal =<< getProp "clientY" (Object mouseEvent)
        liftIO (sink $ f (x,y))
