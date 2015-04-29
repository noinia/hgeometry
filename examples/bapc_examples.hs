module Main where

import qualified BAPC2014.Armybase
import           Control.Applicative
import           Control.Monad(unless)
import           System.Exit

type Algorithm = String -> String

examplesPrefixPath = "examples/"


main = do
  b <- runTests BAPC2014.Armybase.main [ ("BAPC2014/sample.in",   "BAPC2014/sample.out")
                                       , ("BAPC2014/testdata.in", "BAPC2014/testdata.out")
                                       ]
  unless b
    exitFailure


allM   :: (Functor m , Monad m) => (a -> m Bool) -> [a] -> m Bool
allM f = fmap and . mapM f

runTests   :: Algorithm -> [(FilePath, FilePath)] -> IO Bool
runTests f = runTests' f . map (both (examplesPrefixPath ++))
  where
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
