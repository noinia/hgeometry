module Main where

import Data.Monoid
import Control.Applicative
import Options.Applicative
import Data.Data

--------------------------------------------------------------------------------

import qualified Demo.DrawGPX as DrawGPX
import qualified Demo.WriteEnsemble as EnsembleWriter
import qualified Demo.MinDisk as MinDisk
import qualified Demo.Delaunay as Delaunay




--------------------------------------------------------------------------------

data Options = BAPC           BAPCOptions
             | DrawGPX        ()
             | EnsembleWriter EnsembleWriter.Options
             | MinDisk        MinDisk.Options
             | Delaunay       Delaunay.Options
             deriving Data

parser :: Parser Options
parser = subparser (
       command' DrawGPX        (noOpts $ header "Draw GPX")
    <> command' EnsembleWriter EnsembleWriter.options
    <> command' MinDisk        MinDisk.options
    <> command' Delaunay       Delaunay.options
    )


mainWith       :: Options -> IO ()
mainWith opts' = case opts' of
  BAPC _              -> putStrLn "not yet"
  DrawGPX _           -> DrawGPX.main
  EnsembleWriter opts -> EnsembleWriter.mainWith opts
  MinDisk opts        -> MinDisk.mainWith opts
  Delaunay opts       -> Delaunay.mainWith opts


--------------------------------------------------------------------------------

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Example programs for HGeometry. Use -h to get a list of programs."
               <> header   "hgeometry-examples - Examples for HGeometry"
               )

--------------------------------------------------------------------------------

main :: IO ()
main = execParser options >>= mainWith


command'          :: Data o => (a -> o) -> ParserInfo a -> Mod CommandFields o
command' constr p = command (show . toConstr $ constr undefined) (constr <$> p)


noOpts :: InfoMod () -> ParserInfo ()
noOpts = info (pure ())







data BAPCOptions = BAPCOptions { year :: Int
                               }
                   deriving Data
