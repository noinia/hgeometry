{-# LANGUAGE OverloadedStrings          #-}
module Miso.String.Util where

import qualified Data.List as List
import           Miso.String (MisoString, ToMisoString, ms)

unwords :: [MisoString] -> MisoString
unwords = mconcat @MisoString . List.intersperse " "
