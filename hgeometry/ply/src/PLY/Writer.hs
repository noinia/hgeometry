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
import           HGeometry.Triangle
import qualified System.File.OsPath as File
import           System.OsPath

--------------------------------------------------------------------------------

-- | Writes the the points and triangles to a file in PLY format.
renderOutputToFile           :: (Foldable1 f, Point_ point 3 r, Show r)
                             => OsPath -> f (point :+ Int)
                             -> [Triangle (point :+ Int)]
                             -> IO ()
renderOutputToFile fp pts ts = File.writeFile fp $ renderOutput pts ts

-- | Generates the content of the PLY file for the given non-empty list of points and the
-- list of triangles
--  assumes points are 0 indexed.
renderOutput                             :: (Foldable1 f, Point_ point 3 r, Show r)
                                         => f (point :+ Int) -> [Triangle (point :+ Int)]
                                         -> Char8.ByteString
renderOutput (F.toList -> pts) ts =
    Char8.unlines $ hdr <> map renderPt pts <> map renderTri ts
  where
    hdr = ["ply"
          , "format ascii 1.0"
          ,"element vertex " <> (showT $ length pts)
          ,"property float32 x"
          ,"property float32 y"
          ,"property float32 z"
          ,"element face " <> (showT $ length ts)
          ,"property list uchar int vertex_index"
          ,"end_header"
          ]

-- | Writes a Point to ply format
renderPt          :: (Point_ point 3 r, Show r) => (point :+ extra) -> Char8.ByteString
renderPt (p :+ _) = let Point3 x y z = over coordinates showT $ p^.asPoint
                    in Char8.unwords [x,y,z]

-- | Writes a triangle to ply format
renderTri                  :: (Point_ point 3 r, Show r)
                           => Triangle (point :+ Int) -> Char8.ByteString
renderTri (Triangle p q r) = let i a = showT $ a^.extra in Char8.unwords ["3",i p, i q, i r]

-- | Helper to output stuff to bytestrings
showT :: Show a => a -> Char8.ByteString
showT = Char8.pack . show
