{-# LANGUAGE ScopedTypeVariables #-}
module MinDisk where

import           Algorithms.Geometry.SmallestEnclosingBall.RandomizedIncrementalConstruction
import           Algorithms.Geometry.SmallestEnclosingBall.Types

import           Control.Lens hiding (only)
import           Data.Ext
import qualified Data.Foldable as F
import qualified Data.Traversable as Tr
import           Data.Geometry
import           Data.Geometry.Ball
import           Data.Geometry.Line
import           Data.Geometry.PolyLine
import           Data.Monoid

--import           Data.Geometry.Ipe

import           Data.Geometry.Ipe
import           Data.Geometry.Ipe.Types
import           Data.Maybe
import           Data.Seq2
import           Data.Vinyl
import           System.Environment(getArgs)
import           System.Random


diskResult :: Floating r => IpeOut (DiskResult p r) (IpeObject r)
diskResult = IpeOut f
  where
    f (DiskResult d pts) = asIpeGroup (asIpeObject d mempty : (F.toList . fmap g $ pts))
    g p = asIpeObject (p^.core) mempty

main = do
  (fp:_) <- getArgs
  main' fp


main' fp = do
    ep <- readSinglePageFile fp
    gen <- getStdGen
    case ep of
      Left err                       -> print err
      Right (ipeP :: IpePage Double) ->
        case map only $ ipeP^..content.Tr.traverse._IpeUse.core.symbolPoint of
          (a:b:rest) -> do
                          let res = smallestEnclosingDisk gen a b rest
                          printAsIpeSelection . asIpe diskResult $ res
          _          -> putStrLn "Not enough points!"


      -- polies = ipeP^..content.Tr.traverse._IpePath.core._asPolyLine

      -- print pls
      -- mapM_ (print . (^.enclosingDisk) . minDisk' gen) polies
      -- mapM_ (printAsIpeSelection . asIpe diskResult . minDisk' gen) polies


minDisk' :: RandomGen g => g -> PolyLine 2 () Double -> DiskResult () Double
minDisk' = minDisk

minDisk    :: (Ord r, Fractional r, RandomGen g) => g -> PolyLine 2 () r -> DiskResult () r
minDisk gen pl = let (a :<< (pts :> b)) = viewl $ pl^.points
             in smallestEnclosingDisk gen a b (F.toList pts)
