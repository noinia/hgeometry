module Demo.GenPoints where

import Control.Applicative
import Control.Monad(replicateM)
import qualified Data.Set as S
import Data.Monoid
import Data.Geometry
import Data.Geometry.Ipe
import System.Random
import Options.Applicative

data Options = Options { _numPoints :: Int
                       , _outFile   :: FilePath
                       }

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "generate an ipe file with random points."
               <> header   "GenPoints"
               )
  where
    parser = Options
          <$> option auto (help "Number of points"
                         <> short 'n'
                          )
          <*> strOption (help "Output File (in ipe7 xml format)"
                         <> short 'o'
                        )

pageWidth = 600
pageHeight = 800



mainWith (Options n outFile) = do
    pts <- S.toList . S.fromList <$> replicateM n randomPoint
    writeIpeFile outFile . singlePageFromContent . map (asIpeObject' mempty) $ pts


randomPoint :: IO (Point 2 Double)
randomPoint = Point2 <$> randomRIO (0,pageWidth) <*> randomRIO (0,pageHeight)
