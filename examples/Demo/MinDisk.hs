{-# LANGUAGE ScopedTypeVariables #-}
module Demo.MinDisk where

import           Algorithms.Geometry.SmallestEnclosingBall.RandomizedIncrementalConstruction
import           Algorithms.Geometry.SmallestEnclosingBall.Types
import           Control.Applicative
import           Control.Lens
import           Data.Data
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry
import           Data.Geometry.Ball
import           Data.Geometry.Ipe
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Line
import           Data.Geometry.PolyLine
import qualified Data.LSeq as LSeq
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Traversable as Tr
import           Data.Vinyl
import           Options.Applicative
import           System.Environment (getArgs)
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

diskResult :: Floating r => IpeOut (DiskResult p r) (IpeObject r)
diskResult = IpeOut f
  where
    f (DiskResult d pts) = asIpeGroup (asIpeObject d mempty : (F.toList . fmap g $ pts))
    g p = asIpeObject (p^.core) mempty


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
                           printAsIpeSelection . asIpe diskResult $ res
          _           -> putStrLn "Not enough points!"


      -- polies = ipeP^..content.Tr.traverse._IpePath.core._asPolyLine

      -- print pls
      -- mapM_ (print . (^.enclosingDisk) . minDisk' gen) polies
      -- mapM_ (printAsIpeSelection . asIpe diskResult . minDisk' gen) polies


minDisk' :: RandomGen g => g -> PolyLine 2 () Double -> DiskResult () Double
minDisk' = minDisk

minDisk    :: (Ord r, Fractional r, RandomGen g) => g -> PolyLine 2 () r -> DiskResult () r
minDisk gen pl = smallestEnclosingDisk gen (F.toList $ pl^.points)
