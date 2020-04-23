{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Ipe.ReaderSpec where

import Test.Hspec
import Data.Ext
import Data.Geometry
import Data.Geometry.Ipe
import Data.Geometry.Ipe.Path
import Data.ByteString(ByteString)
import Data.Proxy


-- | specializes to use Double as Numtype
fromIpeXML' :: IpeRead (t Double) => ByteString -> Either ConversionError (t Double)
fromIpeXML' = fromIpeXML


spec :: Spec
spec = do
    describe "IpeReadText" $ do
      it "parses a polyline into a Path" $
        ipeReadText "\n128 656 m\n224 768 l\n304 624 l\n432 752 l\n"
        `shouldBe` Right ops
    describe "IpeReadAttrs" $ do
      it "parses a symbols attributes" $
        (show $ readXML useTxt
                >>= ipeReadAttrs (Proxy :: Proxy IpeSymbol) (Proxy :: Proxy Double))
        `shouldBe`
        "Right (Attrs {NoAttr, NoAttr, NoAttr, NoAttr, Attr IpeColor (Named \"black\"), NoAttr, NoAttr, Attr IpeSize (Named \"normal\")})"
    describe "IpeRead" $ do
      it "parses a Symbol" $
        fromIpeXML' useTxt
        `shouldBe` Right useSym
    -- it "parses a path" $
      --   (show $ fromIpeXML'
          -- "<use name=\"mark/disk(sx)\" pos=\"320 736\" size=\"normal\" stroke=\"black\"/>"
-- )
      --   `shouldBe`
-- "Right (Path {_pathSegments = PolyLineSegment (PolyLine {_points = Seq2 (Point2 [128.0,656.0] :+ ()) (fromList [Point2 [224.0,768.0] :+ (),Point2 [304.0,624.0] :+ ()]) (Point2 [432.0,752.0] :+ ())}) :< fromList []})"


  where
    useTxt = "<use name=\"mark/disk(sx)\" pos=\"320 736\" size=\"normal\" stroke=\"black\"/>"
    useSym = Symbol (Point2 320 736) "mark/disk(sx)"
--     symAttrs =

    translatedUse = "<use matrix=\"1 0 0 1 4.44908 -4.21815\" name=\"mark/disk(sx)\" pos=\"320 736\" size=\"normal\" stroke=\"black\"/>"




    ops = [ MoveTo $ Point2 128 (656 :: Double)
          , LineTo $ Point2 224 768
          , LineTo $ Point2 304 624
          , LineTo $ Point2 432 752
          ]
