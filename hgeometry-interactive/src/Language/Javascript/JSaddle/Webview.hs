{-# LANGUAGE OverloadedStrings          #-}
module Language.Javascript.JSaddle.Webview where

import           Control.Concurrent
import qualified Data.Text as T (unpack, pack)
import qualified Graphics.UI.Webviewhs as WHS
import           Language.Javascript.JSaddle (JSM, Results)
import qualified Language.Javascript.JSaddle.Warp as JSaddle
import           System.Directory (getCurrentDirectory)


--------------------------------------------------------------------------------

run           :: Int -> JSM () -> IO ()
run port main = do
  -- pwd <- getCurrentDirectory
  -- let uri = "file://" <> T.pack pwd <> "/"
  let uri = "http://localhost:" <> (T.pack $ show port)
  _thdId <- forkIO $ JSaddle.run port main
  WHS.createWindowAndBlock
    WHS.WindowParams
      { WHS.windowParamsTitle      = "JSaddle"
      , WHS.windowParamsUri        = uri
      , WHS.windowParamsWidth      = 1600
      , WHS.windowParamsHeight     = 1000
      , WHS.windowParamsResizable  = False
      , WHS.windowParamsDebuggable = True
      }
