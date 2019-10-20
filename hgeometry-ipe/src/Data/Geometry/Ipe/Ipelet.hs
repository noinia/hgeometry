module Data.Geometry.Ipe.Ipelet where

import Data.Geometry.Ipe.Types
import Data.Geometry.Ipe.UI
import Data.Fixed
import Foreign.Lua(Pushable(..),Peekable(..), Lua)


type Error = String

-- newtype Ipelet = Ipelet { _run :: UI Rational -> Either Error (IpeObject Rational) }

newtype Ipelet = Ipelet (UI Pico -> IO (IpeObject Pico))


instance HasResolution p => Pushable (Fixed p) where
  push = push . (realToFrac :: Fixed p -> Double)

instance HasResolution p => Peekable (Fixed p) where
  peek i = realToFrac <$> (peek i :: Lua Double)





-- runIpelet            :: Ipelet
--                      -> Lua.State
--                      -> IO NumResults
-- runIpelet hsIpelet l = Lua.runWith l $ do
--                          pno  <- peek 1
--                          vno  <- peek 2
--                          -- doc  <- peek 3
--                          let ui = IpeUI pno vno
--                          res <- liftIO . hsIpelet $ ui
--                          push res
--                          return 1
