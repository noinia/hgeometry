{-# LANGUAGE ScopedTypeVariables #-}
module Demo.MinDisk where

import           Algorithms.Geometry.SmallestEnclosingBall.RandomizedIncrementalConstruction
import           Algorithms.Geometry.SmallestEnclosingBall.Types
import           Control.Lens
import           Data.Data
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry
import           Data.Geometry.Ipe
import qualified Data.Traversable as Tr
import           Options.Applicative
import           System.Random

--------------------------------------------------------------------------------

newtype Options = Options { inPath  :: FilePath }
                deriving Data

options :: ParserInfo Options
options = info (helper <*> parser)
               (  progDesc "Given an ipe file with a set of points, computes the smallest enclosing disk of the points."
               <> header   "MinDisk - Computes the smallest enclosing disk of a set of points"
               )
  where
    parser = Options
          <$> strOption (help "Input File (in ipe7 xml format)")

--------------------------------------------------------------------------------

diskResult :: Floating r => IpeOut (DiskResult p r) Group r
diskResult (DiskResult d pts) = ipeGroup (iO' d  : (F.toList . fmap g $ pts))
  where
    g p = iO' (p^.core)

mainWith              :: Options -> IO ()
mainWith (Options fp) = do
    ep <- readSinglePageFile fp
    gen <- getStdGen
    case ep of
      Left err                       -> print err
      Right (ipeP :: IpePage Double) ->
        case map ext $ ipeP^..content.Tr.traverse._IpeUse.core.symbolPoint of
          pts@(_:_:_) -> do
                           let res = smallestEnclosingDisk gen pts
                           printAsIpeSelection . iO . diskResult $ res
          _           -> putStrLn "Not enough points!"


      -- polies = ipeP^..content.Tr.traverse._IpePath.core._asPolyLine

      -- print pls
      -- mapM_ (print . (^.enclosingDisk) . minDisk' gen) polies
      -- mapM_ (printAsIpeSelection . asIpe diskResult . minDisk' gen) polies


minDisk' :: RandomGen g => g -> PolyLine 2 () Double -> DiskResult () Double
minDisk' = minDisk

minDisk    :: (Ord r, Fractional r, RandomGen g) => g -> PolyLine 2 () r -> DiskResult () r
minDisk gen pl = smallestEnclosingDisk gen (F.toList $ pl^.points)
