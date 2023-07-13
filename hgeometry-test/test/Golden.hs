{-# LANGUAGE QuasiQuotes #-}
module Golden where

import qualified Data.ByteString.Lazy as ByteString
import           Data.Functor.Contravariant
import           Ipe
import           System.OsPath
import           Test.Hspec.WithTempFile

--------------------------------------------------------------------------------

-- | Construct a golden test from a given ipe file
ipeFileGolden :: (Show r, IpeWriteText r) => Golden ByteString.ByteString (IpeFile r)
ipeFileGolden = byteStringGolden { actualWriter = WriteActual writeIpeFile
                                 , goldenFile   = [osp|hgeometry-test.ipe|]
                                 }

-- | Construct a golden test from a given ipe page
ipePageGolden :: (Show r, IpeWriteText r) => Golden ByteString.ByteString (IpePage r)
ipePageGolden = contramap singlePageFile ipeFileGolden

-- | Construct a golden test from a bunch of ipe objects
ipeContentGolden :: (Show r, IpeWriteText r) => Golden ByteString.ByteString [IpeObject r]
ipeContentGolden = contramap fromContent ipePageGolden
