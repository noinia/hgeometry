{-# LANGUAGE QuasiQuotes #-}
module Golden where

import           Control.Monad ((>=>))
import qualified Data.ByteString.Lazy as ByteString
import           Data.Eq.Approximate
import           Data.Functor.Contravariant
import           GHC.TypeNats
import           Ipe
import qualified Paths_hgeometry as Paths
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

-- | Get the data file as an OsPath
getDataFileName :: OsPath -> IO OsPath
getDataFileName = decodeFS >=> Paths.getDataFileName >=> encodeFS


-- | Ipe File that uses ApproximateEquality at the given tollerance to
-- test for equality between the results.
ipeFileGoldenApproxEq :: forall tol r. ( Show r, IpeWriteText r, Ord r, Fractional r
                                       , KnownNat tol
                                       )
                      => Golden (IpeFile (AbsolutelyApproximateValue (Digits tol) Double))
                                (IpeFile r)
ipeFileGoldenApproxEq = ipeFileGolden { actualWriter   = WriteActual writeIpeFile
                                      , writeGolden    = writeIpeFile
                                      , readGoldenFile = readIpeFileThrow
                                      , prettyGolden   = show
                                      }
  where
    readIpeFileThrow    :: OsPath
                        -> IO (IpeFile (AbsolutelyApproximateValue (Digits tol) Double))
    readIpeFileThrow fp = readIpeFile fp >>= \case
      Left err -> fail (show err)
      Right p  -> pure p


--   mapGolden (readIpeFile

--   :: ByteString -> IpeFile ... )
--                                   (toIpeXML :: IpeFile (Approx ) -> ByteString)
--                                   ipeFileGolden

-- ipeFileGolden' :: (Show r, IpeWriteText r) => Golden (Maybe ByteString.ByteString) (IpeFile r)
-- ipeFileGolden' = mapGolden Just (fromMaybe mempty) ipeFileGolden


  -- contramap coerce ipeFileGolden
  -- where
  --   f :: IpeFile (AbsolutelyApproximateValue tol r) -> IpeFile r
  --   f = coerce

    -- fmap unwrapAbsolutelyApproximateValue

    -- (fmap (AbsolutelyApproximateValue @tol))

  -- mapGolden
  --                                 (fmap unwrapAbsolutelyApproximateValue)
  --                                 ipeFileGolden








  -- spec { actualWriter   = f <$> actualWriter spec
  --                         , writeGolden    =
  --                         , readGoldenFile = fmap f . readGoldenFile spec
  --                         , prettyGolden   =
  --                         }
