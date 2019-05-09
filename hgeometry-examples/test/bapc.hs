module Main where


import qualified BAPC2012.Gunslinger
import qualified BAPC2014.Armybase



import           Control.Applicative
import           Control.Monad(unless)
import           System.Exit

type Algorithm = String -> String

examplesPrefixPath :: FilePath
examplesPrefixPath = "data/"


data BAPCTest = BAPC { name      :: String
                     , directory :: FilePath
                     , algo      :: Algorithm
                     , files     :: [(FilePath,FilePath)]
                     }


bapcTests :: [BAPCTest]
bapcTests = [ BAPC "Armybase" "BAPC2014" BAPC2014.Armybase.armybase
                   [ ("sample.in",   "sample.out")
                   , ("testdata.in", "testdata.out")
                   ]
            -- , BAPC "Gunslinger" "BAPC2012" BAPC2012.Gunslinger.gunslinger
            --        [ ("sampleG.in",   "sampleG.out")
            --        , ("G.in", "G.out")
            --        ]
            ]

main :: IO ()
main = mapM_ runBAPCTest bapcTests

--------------------------------------------------------------------------------

runBAPCTest                   :: BAPCTest -> IO ()
runBAPCTest (BAPC n p alg fs) = do
  let dash = replicate 80 '-'
  putStrLn dash
  putStrLn $ "Running tests for " ++ n
  putStrLn dash
  b <- runTests p alg fs
  unless b exitFailure
  putStrLn $ "Tests for " ++ n ++ " PASSED."
  putStrLn dash


runTests     :: FilePath -> Algorithm -> [(FilePath, FilePath)] -> IO Bool
runTests p f = runTests' f . map (both (p' ++))
  where
    p'           = concat [examplesPrefixPath, p, "/"]
    both g (a,b) = (g a, g b)

-- | Given an algorithm and a list of pairs: (inputFile,solutionFile), run all tests
runTests'   :: Algorithm -> [(FilePath, FilePath)] -> IO Bool
runTests' f = allM (uncurry $ runTest f)

runTest                       :: Algorithm
                              -> FilePath -> FilePath
                              -> IO Bool
runTest f inFile solutionFile = (\input solution -> f input == solution)
                             <$> readFile inFile
                             <*> readFile solutionFile

--------------------------------------------------------------------------------


allM   :: (Functor m , Monad m) => (a -> m Bool) -> [a] -> m Bool
allM f = fmap and . mapM f
