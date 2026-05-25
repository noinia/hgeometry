module Plane.Debug
  ( traceShow
  , traceShowWith
  , traceShowId
  , traceWith
  , traceStack
  ) where

import Debug.Pretty.Simple
import Text.Pretty.Simple

--------------------------------------------------------------------------------

traceShow   :: Show a => a -> b -> b
traceShow a = traceWith (const $ show a)
{-# WARNING traceShow "'traceShow' remains in code" #-}

traceShowWith   :: Show b => (a -> b) -> a -> a
traceShowWith f = traceWith (show . f)
{-# WARNING traceShowWith "'traceShowWith' remains in code" #-}

traceShowId   :: Show a => a -> a
traceShowId x = traceShow x x
{-# WARNING traceShowId "'traceShowId' remains in code" #-}

traceWith     :: (a -> String) -> a -> a
traceWith f x = pTraceOpt colorTTY outputOptions (f x) x
{-# WARNING traceWith "'traceWith' remains in code" #-}

traceStack :: String -> a -> a
traceStack = pTraceStackOpt colorTTY outputOptions
{-# WARNING traceStack "'traceStack' remains in code" #-}

--------------------------------------------------------------------------------

colorTTY :: CheckColorTty
colorTTY = CheckColorTty

outputOptions :: OutputOptions
outputOptions = defaultOutputOptionsDarkBg
                 { outputOptionsPageWidth     = 120
                 , outputOptionsCompact       = True
                 , outputOptionsCompactParens = True
                 }
