module Plane.ObjEncoder
  ( writeFile

  ) where

import           Data.Foldable
import           System.OsPath
import           Codec.Wavefront.Object
import           Codec.Wavefront.Element
import           Codec.Wavefront.Location
import           Codec.Wavefront.Face
import qualified System.File.OsPath as File
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.List as List
import           Prelude hiding (unwords, unlines, writeFile)

--------------------------------------------------------------------------------


writeFile        :: OsPath -> WavefrontOBJ -> IO ()
writeFile fp obj = File.writeFile fp $ encode obj

encode     :: WavefrontOBJ -> ByteString
encode obj = Builder.toLazyByteString $ unlines
             [ vertices
             , faces
             ]
  where
    vertices = unlines . fmap encodeLocation          $ objLocations obj
    faces    = unlines . fmap (encodeElem encodeFace) $ objFaces obj


encodeLocation (Location x y z w) = unwords
                                    [ Builder.char8 'v'
                                    , Builder.floatDec x
                                    , Builder.floatDec y
                                    , Builder.floatDec z
                                    , Builder.floatDec w
                                    ]

encodeElem      :: (a -> Builder.Builder) -> Element a -> Builder.Builder
encodeElem f el = f $ elValue el

encodeFace                   :: Face -> Builder.Builder
encodeFace (Face i j k rest) = unwords $
                               [ Builder.char8 'f'
                               , encodeFaceIndex i
                               , encodeFaceIndex j
                               , encodeFaceIndex k
                               ] <> map encodeFaceIndex rest

encodeFaceIndex    :: FaceIndex -> Builder.Builder
encodeFaceIndex fi = Builder.intDec (faceLocIndex fi)

unwords :: [Builder.Builder] -> Builder.Builder
unwords = mconcat . List.intersperse (Builder.char8 ' ')

unlines :: Foldable list => list (Builder.Builder) -> Builder.Builder
unlines = mconcat . List.intersperse (Builder.char8 '\n') . toList
