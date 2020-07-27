module Ipe.GrowingDisks where

import           Control.Lens
import           Data.Data
import           Data.Ext
import           Data.Geometry.Ball
import           Data.Geometry.Boundary
import           Data.Geometry.Box
import           Data.Geometry.Ellipse (ellipseToCircle)
import           Data.Geometry.Ipe
import           Data.Geometry.PolyLine
import           Data.Geometry.Properties
-- import           Data.Geometry.ZeroSet
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (mapMaybe)
import           Data.RealNumber.Rational
import           Data.Util
import           Options.Applicative

-- import Debug.Trace
--------------------------------------------------------------------------------


type R = Double

data Options = Options { inFile  :: FilePath
                       , outFile :: FilePath
                       } deriving (Show,Eq,Data)

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "given an ipe file some disks, create scaled copies of them"
               <> header   "growingdisks"
               )
  where
    parser = Options
          <$> strOption (help "Input file (in ipe7 xml format)"
                         <> short 'i'
                        )
          <*> strOption (help "Output File (in ipe7 xml format)"
                         <> short 'o'
                        )

mainWith (Options inFile outFile) = readSinglePageFile inFile >>= \case
    Left err -> print err
    Right p  -> runPage p outFile


type ReadInput = [Circle () R]

readInput      :: IpePage R -> Either String ReadInput
readInput page = case map (^.core) . readAll $ page of
                      []   -> Left $ "no disks found "
                      dsks -> Right dsks

runPage         :: IpePage R -> FilePath -> IO ()
runPage page fp = case res of
                    Left e    -> print e
                    Right inp -> writeIpeFile fp $ ipeFile inp
  where
    res = mkScaled <$> readInput page


mkScaled ds = fmap mkPage $ NonEmpty.fromList [1..n]
  where
    n = 10
    mkPage i = fromContent . fmap (grow $ 1 + (fromIntegral i / fromIntegral n)) $ ds
    grow lam d = iO . defIO $ d&from _Boundary.squaredRadius %~ (*(lam*lam))

withError   :: e -> Maybe a -> Either e a
withError e = maybe (Left e) Right
