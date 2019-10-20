module LibArith where

import Control.Monad.IO.Class(liftIO)
import Foreign.C.Types (CInt (CInt))
import qualified Foreign.Lua as Lua
import Foreign.Lua(NumResults, peek, push)

foreign export ccall
  add :: Lua.State -> IO NumResults

foreign export ccall
  run :: Lua.State -> IO NumResults

add :: Lua.State -> IO NumResults
add l = Lua.runWith l $ do
  i1 <- peek 1
  i2 <- peek 2
  push (i1 + i2 :: Lua.Number)
  return 1

data IpeUI = IpeUI { _pageNumber :: Int
                   , _viewNumber :: Int
                   }


type MethodIndex = Int

type Error = String
type IpeObject = Lua.Number

type HaskellIpelet = IpeUI -> IO IpeObject


myIpelet             :: HaskellIpelet
myIpelet (IpeUI i j) = pure $ (fromIntegral i + fromIntegral j :: Lua.Number)


run :: Lua.State -> IO NumResults
run = runIpelet myIpelet


runIpelet            :: HaskellIpelet
                     -> Lua.State
                     -> IO NumResults
runIpelet hsIpelet l = Lua.runWith l $ do
                         pno  <- peek 1
                         vno  <- peek 2
                         -- doc  <- peek 3
                         let ui = IpeUI pno vno
                         res <- liftIO . hsIpelet $ ui
                         push res
                         return 1
