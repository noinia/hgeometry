module Geometry.Arrangement.Test where

import Control.Lens
import Data.Ext
import Geometry.Arrangement.Internal
import Geometry.Box
import Geometry.Ipe
import Geometry.Line
import Geometry.LineSegment
import Geometry.PlanarSubdivision
import Geometry.PlanarSubdivision.Draw
import Geometry.Point
import Geometry.Properties

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
readArr' = readArr "test/Geometry/arrangement.ipe"

{- HLINT ignore test -}
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
