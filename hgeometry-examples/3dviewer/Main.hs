{-# LANGUAGE CPP                        #-}
module Main(main) where
import qualified App

--------------------------------------------------------------------------------
-- | WASM export, required when compiling w/ the WASM backend.
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

main :: IO ()
main = App.main
