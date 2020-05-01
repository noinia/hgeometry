module Main where

import Data.Monoid
import Control.Applicative
import Options.Applicative
import Data.Data

--------------------------------------------------------------------------------

import qualified Demo.DrawGPX as DrawGPX
import qualified Demo.ExtractGPX as ExtractGPX
import qualified Demo.WriteEnsemble as EnsembleWriter
import qualified Demo.MinDisk as MinDisk
import qualified Demo.Delaunay as Delaunay
import qualified Demo.ExpectedPairwiseDistance as ExpPWD
import qualified Demo.TriangulateWorld as TriangulateWorld
import qualified Demo.FloatingPoint as FloatingPoint

import qualified Ipe.GrowingDisks as GrowingDisks

--------------------------------------------------------------------------------

data Options = BAPC                      BAPCOptions
             | DrawGPX                   DrawGPX.Options
             | ExtractGPX                ExtractGPX.Options
             | EnsembleWriter            EnsembleWriter.Options
             | MinDisk                   MinDisk.Options
             | Delaunay                  Delaunay.Options
             | ExpectedPairwiseDistance  ExpPWD.Options
             | TriangulateWorld          TriangulateWorld.Options
             | FloatingPoint             FloatingPoint.Options
             | GrowingDisks              GrowingDisks.Options
             deriving Data

parser :: Parser Options
parser = subparser (
       command' DrawGPX                        DrawGPX.options
    <> command' ExtractGPX                     ExtractGPX.options
    <> command' EnsembleWriter                 EnsembleWriter.options
    <> command' MinDisk                        MinDisk.options
    <> command' Delaunay                       Delaunay.options
    <> command' ExpectedPairwiseDistance       ExpPWD.options
    <> command' TriangulateWorld               TriangulateWorld.options
    <> command' FloatingPoint                  FloatingPoint.options
    <> command' GrowingDisks                   GrowingDisks.options
    )


mainWith       :: Options -> IO ()
mainWith opts' = case opts' of
  BAPC _                              -> putStrLn "not yet"
  DrawGPX opts                        -> DrawGPX.mainWith opts
  ExtractGPX opts                     -> ExtractGPX.mainWith opts
  EnsembleWriter opts                 -> EnsembleWriter.mainWith opts
  MinDisk opts                        -> MinDisk.mainWith opts
  Delaunay opts                       -> Delaunay.mainWith opts
  ExpectedPairwiseDistance opts       -> ExpPWD.mainWith opts
  TriangulateWorld opts               -> TriangulateWorld.mainWith opts
  FloatingPoint opts                  -> FloatingPoint.mainWith opts
  GrowingDisks opts                   -> GrowingDisks.mainWith opts

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
