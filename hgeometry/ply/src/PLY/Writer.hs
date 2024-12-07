{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Ply.Writer
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Helper module to write PLY files; so that we can export simple 3D scenes.
--
--------------------------------------------------------------------------------
module PLY.Writer
  ( renderOutputToFile
  , renderOutput
  ) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.Foldable as F
import           Data.Foldable1
import           HGeometry.Ext
import           HGeometry.Point
import qualified System.File.OsPath as File
import           System.OsPath

--------------------------------------------------------------------------------

-- | Writes the the points and triangles to a file in PLY format.
renderOutputToFile           :: ( Foldable1 f, Foldable1 g, Foldable1 face
                                , Point_ point 3 r, Show r)
                             => OsPath -> f (point :+ Int) -> g (face Int) -> IO ()
renderOutputToFile fp pts ts = File.writeFile fp $ renderOutput pts ts

-- | Generates the content of the PLY file for the given non-empty list of vertices and
-- the non-empty list of convex faces.
--
-- Each face is given by a non-empty list of vertexId's, which are assumed to be
-- 0-indexed.
--
renderOutput                :: ( Foldable1 f, Foldable1 g
                               , Foldable1 face
                               , Point_ point 3 r
                               , Show r)
                            => f (point :+ Int) -> g (face Int) -> Char8.ByteString
renderOutput vertices faces =
    Char8.unlines $ hdr <> map renderVtx (F.toList vertices) <> map renderFace (F.toList faces)
  where
    hdr = ["ply"
          , "format ascii 1.0"
          ,"element vertex " <> (showT $ F.length vertices)
          ,"property float32 x"
          ,"property float32 y"
          ,"property float32 z"
          ,"element face " <> (showT $ F.length faces)
          ,"property list uchar int vertex_index"
          ,"end_header"
          ]

-- | Writes a vertex to ply format
renderVtx          :: (Point_ point 3 r, Show r) => (point :+ extra) -> Char8.ByteString
renderVtx (p :+ _) = let Point3 x y z = over coordinates showT $ p^.asPoint
                     in Char8.unwords [x,y,z]

-- | Writes a face to ply format.
renderFace      :: Foldable1 face => face Int -> Char8.ByteString
renderFace face = Char8.unwords . map showT $ (F.length face) : (F.toList face)

-- | Helper to output stuff to bytestrings
showT :: Show a => a -> Char8.ByteString
showT = Char8.pack . show
