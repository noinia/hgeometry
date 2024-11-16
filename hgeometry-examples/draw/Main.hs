{-# LANGUAGE QuasiQuotes #-}
module Main(main) where

import           Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import           HGeometry.Instances ()
import           HGeometry.Number.Real.Rational
import           HGeometry.PlaneGraph
import           HGeometry.PlaneGraph.Instances
import           HGeometry.Point
import           Ipe
import           System.OsPath
import           Test.QuickCheck

--------------------------------------------------------------------------------

type R = RealNumber 5

renderGraph    :: PlaneGraph QuickCheckWorld (Point 2 R) () () -> IpePage R
renderGraph gr = fromContent $
                 concat [ [ iO $ defIO p   | p <- gr^..vertices ]
                        , [ iO $ defIO seg | seg <- gr^..edgeSegments ]
                        ]

main :: IO ()
main = do
  grs <- NonEmpty.fromList <$> sample' arbitrary
  let outFp = [osp|foo.ipe|]
  writeIpeFile outFp $ ipeFile (renderGraph <$> grs)

  -- (grs :: [PlaneGraph QuickCheckWorld (Point 2 R) () ()]) <- sample' arbitrary
  -- mapM_ print grs
