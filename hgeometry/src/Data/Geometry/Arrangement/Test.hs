module Data.Geometry.Arrangement.Test where

import Control.Lens
import Data.Ext
import Data.Geometry.Arrangement.Internal
import Data.Geometry.Box
import Data.Geometry.Ipe
import Data.Geometry.Line
import Data.Geometry.LineSegment
import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.Draw
import Data.Geometry.Point
import Data.Geometry.Properties

--------------------------------------------------------------------------------
data Test = Test

readArr :: FilePath -> IO (Arrangement Test () () () Rational)
readArr fp = do  Right ls <- fmap f <$> readSinglePageFile fp
                 print ls
                 pure $ constructArrangement (Identity Test) ls
  where
    f     :: IpePage Rational -> [Line 2 Rational :+ ()]
    f page = [ ext $ supportingLine s
             | (s :+ ats) <- segs
             ]
      where
        segs = page^..content.traverse._withAttrs _IpePath _asLineSegment

readArr' :: IO (Arrangement Test () () () Rational)
readArr' = readArr "test/Data/Geometry/arrangement.ipe"

test :: IO ()
test = do
         let outFile = "/tmp/out.ipe"
         arr <- readArr'
         let ls = arr^..inputLines.traverse
             (segs,parts') = computeSegsAndParts (arr^.boundedArea) ls
             out = [ asIpe drawPlanarSubdivision (arr^.subdivision) ]
         -- print arr
         print "darts:"
         mapM_ (f $ arr^.subdivision) $ traverseLine ((head ls)^.core) arr


         let (q :+ (),_,_,_) = corners $ arr^.boundedArea
         print $ findStartVertex q arr
         writeIpeFile outFile . singlePageFromContent $ out
  where
    f ps d = let (e:+ _) = edgeSegment d ps in printAsIpeSelection e
