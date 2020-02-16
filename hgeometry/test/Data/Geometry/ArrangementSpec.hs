{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.ArrangementSpec where

import           Control.Lens
import qualified Data.ByteString as B
import           Data.Ext
import           Data.Geometry
import           Data.Geometry.Arrangement
import           Data.Geometry.Arrangement.Draw
import           Data.Geometry.Ipe
import           Test.Hspec
import           Test.Util(runOnFile)

spec :: Spec
spec = testCases "test/Data/Geometry/arrangement.ipe"


testCases    :: FilePath -> Spec
testCases fp = runIO (readInputFromFile fp) >>= \case
    Left e    -> it "reading arrangement file" $
                   expectationFailure $ "Failed to read ipe file " ++ show e
    Right tcs -> mapM_ toSpec tcs


--   ipeF <- beforeAll $ readInputFromFile "tests/Data/Geometry/pointInPolygon.ipe"
--   describe "Point in Polygon tests" $ do
--     it "returns the first element of a list" $ do
--       head [23 ..] `shouldBe` (23 :: Int)


data TestCase r = TestCase { _lines      :: [Line 2 r :+ ()]
                           , _outFile    :: FilePath -- ^ filename of the output arrangement,
                                                     -- as an ipe file
                           }
                  deriving (Show)

data Test = Test

drawArr    :: [Line 2 Rational :+ a] -> B.ByteString
drawArr ls = let arr = constructArrangement (Identity Test) ls
                 out = [ iO $ drawArrangement arr ]
                 Just bs = toIpeXML . singlePageFromContent $ out
             in bs


toSpec                       :: TestCase Rational -> Spec
toSpec (TestCase ls outFile) = do
    runOnFile "test drawing arrangement"  outFile (pure $ drawArr ls)

-- toSingleSpec poly r q = it msg $ (q `inPolygon` poly) `shouldBe` r
--   where
--     msg = "Point in polygon test with " ++ show q



-- toSpec (TestCase poly is bs os) = do
--                                     describe "inside tests" $
--                                       mapM_ (toSingleSpec poly Inside) is
--                                     describe "on boundary tests" $
--                                       mapM_ (toSingleSpec poly OnBoundary) bs
--                                     describe "outside tests" $
--                                       mapM_ (toSingleSpec poly Outside) os

readInputFromFile    :: FilePath -> IO (Either ConversionError [TestCase Rational])
readInputFromFile fp = fmap f <$> readSinglePageFile fp
  where
    f      :: IpePage Rational -> [TestCase Rational]
    f page = [ TestCase [ ext $ supportingLine s
                        | (s :+ _ats) <- segs
                        ]
                        (fp <> ".out.ipe")
             ]
      where
        segs = page^..content.traverse._withAttrs _IpePath _asLineSegment
    -- f page = [ TestCase poly
    --                     [ s^.symbolPoint | s <- myPoints ats, isInsidePt  s ]
    --                     [ s^.symbolPoint | s <- myPoints ats, isBorderPt  s ]
    --                     [ s^.symbolPoint | s <- myPoints ats, isOutsidePt s ]
    --          | (poly :+ ats) <- polies
    --          ]
    --   where
    --     polies = page^..content.traverse._withAttrs _IpePath _asSimplePolygon
    --     syms   = page^..content.traverse._IpeUse


    --     myPoints polyAts = [s | (s :+ ats) <- syms, belongsToPoly ats polyAts ]

    --     -- We test a point/polygon combination if they have the same color
    --     belongsToPoly symAts polyAts =
    --         lookupAttr colorP symAts == lookupAttr colorP polyAts

    --     -- A point i inside if it is a disk
    --     isInsidePt   :: IpeSymbol r -> Bool
    --     isInsidePt s = s^.symbolName == "mark/disk(sx)"

    --     -- Boxes are on the boundary
    --     isBorderPt s = s^.symbolName == "mark/box(sx)"

    --     -- crosses are outside the polygon
    --     isOutsidePt s = s^.symbolName == "mark/cross(sx)"

    --     colorP = Proxy :: Proxy Stroke
