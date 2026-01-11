{-# LANGUAGE QuasiQuotes #-}
module GiftwrapSpec
  ( spec
  , main'
  ) where

import Data.Foldable
import Control.Lens
import Data.Maybe
import Golden
import HGeometry.Kernel
import HGeometry.Ext
import HGeometry.Vector
import Ipe
import Ipe.Color
import System.OsPath
import Test.Hspec
import Test.Hspec.WithTempFile
import Test.QuickCheck.Instances ()
import R
import Wavefront qualified
import Data.Coerce
import Codec.Wavefront.Object
import Codec.Wavefront.Face
import Codec.Wavefront.Element
import Codec.Wavefront.Point qualified as W
import Codec.Wavefront.Location (Location(..))
import Data.Vector qualified as Vector
import Data.List.NonEmpty qualified as NonEmpty
import Data.List qualified as List
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Builder (floatDec, charUtf8, intDec, Builder, hPutBuilder)
import Prelude hiding (unwords, unlines, writeFile)
import System.File.OsPath (withBinaryFile)
import System.IO (IOMode(..))
--------------------------------------------------------------------------------


-- byteStringGolden

spec :: Spec
spec = describe "giftwrap tests" $ do
         pure ()
         -- goldenWith [osp|data/test-with-ipe/ConvexHull/|]
         --   (ipeFileGolden { name      = [osp||] }
         --   )
         --   ( let myLines'     = (iO . defIO) <$> myLines


main' :: IO ()
main' = run [osp|data/test-with-ipe/golden/Render/cube/cube.obj|]
            [osp|data/test-with-ipe/ConvexHull/cube.out.obj|]

run inFP outFp = Wavefront.fromFile inFP >>= \case
    Left err  -> error err
    Right obj -> toFile outFp $ extendObj (allLocations obj) [] obj

allLocations :: WavefrontOBJ -> Vector.Vector (Point 3 R)
allLocations = fmap toPoint3 . objLocations
  where
    toPoint3 l = let p = l^.asPoint.to projectPoint
                 in p&coordinates %~ realToFrac


toFile fp obj = writeFile fp $ unlines [ locs
                                       , pts
                                       , faces
                                       ]
  where
    locs = unlines
         . fmap (\(Location x y z w) ->
                       unwords [charUtf8 'v', floatDec x, floatDec y, floatDec z, floatDec w]
                ) $ objLocations obj
    faces = unlines
          . fmap (\e -> let Face u v w vs = elValue e
                            renderIdx v'  = intDec $ faceLocIndex v'
                            -- TODO: also render the texture coords if needed
                        in unwords $ charUtf8 'f' : renderIdx u : renderIdx v : renderIdx w :
                                                    map renderIdx vs
                 ) $ objFaces obj
    pts = unlines
        . fmap (\e -> let W.Point p = elValue e
                      in unwords [charUtf8 'p', intDec p]
               ) $ objPoints obj

    unwords = fold . List.intersperse (charUtf8 ' ')
    unlines :: Foldable f => f Builder -> Builder
    unlines = fold . List.intersperse (charUtf8 '\n') . toList





  -- Builder.writeFile

-- toByteString     :: WavefrontOBJ ->
-- toByteString obj = writeFile' $

-- writeFile' = Os


-- toOBJOut :: Foldable f => Vector.Vector (Point 3 R) -> f Face -> WavefrontOBJ
-- toOBJOut = extendObj emptyObj

extendObj            :: Foldable f
                     => Vector.Vector (Point 3 R) -- additional points
                     -> f Face -- ^ additional faces
                     -> WavefrontOBJ -- ^ file we are extending
                     -> WavefrontOBJ
extendObj pts fs obj = obj { objLocations = objLocations obj <> locs
                           , objFaces     = objFaces obj     <> fs'
                           , objPoints    = objPoints obj    <> imap asPoint' pts
                           }
  where
    n    = length (objLocations obj)
    fs'  = Vector.empty
    locs = fmap (\p -> let Point3 x y z = p&coordinates %~ realToFrac
                       in Location x y z 1
                ) pts

    -- points are 1-indexed; so offset by +1
    asPoint' i _ = Element { elObject         = Nothing
                           , elGroups         = []
                           , elMtl            = Nothing
                           , elSmoothingGroup = 0
                           , elValue          = W.Point (n+i+1)
                           }

emptyObj = WavefrontOBJ { objLocations = Vector.empty
                        , objTexCoords = Vector.empty
                        , objNormals   = Vector.empty
                        , objPoints    = Vector.empty
                        , objLines     = Vector.empty
                        , objFaces     = Vector.empty
                        , objMtlLibs   = Vector.empty
                        }




--------------------------------------------------------------------------------
-- * These should move to Bytestring I guess


modifyFile :: IOMode -> OsPath -> Builder -> IO ()
modifyFile mode f bld = withBinaryFile f mode (`hPutBuilder` bld)

-- | Write a 'Builder' to a file.
--
-- Similarly to 'hPutBuilder', this function is more efficient than
-- using 'Data.ByteString.Lazy.hPut' . 'toLazyByteString' with a file handle.
--
-- @since 0.11.2.0
writeFile :: OsPath -> Builder -> IO ()
writeFile = modifyFile WriteMode
