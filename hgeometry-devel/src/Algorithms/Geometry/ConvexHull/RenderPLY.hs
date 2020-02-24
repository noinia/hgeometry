{-# LANGUAGE OverloadedStrings #-}
module Algorithms.Geometry.ConvexHull.RenderPLY where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Point
import           Data.Geometry.Triangle
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

renderOutputToFile           :: Show r
                             => FilePath -> NonEmpty (Point 3 r :+ Int) -> [Triangle 3 Int r]
                             -> IO ()
renderOutputToFile fp pts ts = Text.writeFile fp $ renderOutput pts ts

-- assumes points are 0 indexed.
renderOutput                             :: Show r
                                         => NonEmpty (Point 3 r :+ Int) -> [Triangle 3 Int r]
                                         -> Text.Text
renderOutput (NonEmpty.toList -> pts) ts = Text.unlines $ hdr <> map renderPt pts <> map renderTri ts
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

renderPt          :: Show r => (Point 3 r :+ extra) -> Text.Text
renderPt (p :+ _) = let Point3 x y z = showT <$> p in Text.unwords [x,y,z]

renderTri                  :: Triangle 3 Int r -> Text.Text
renderTri (Triangle p q r) = let i a = showT $ a^.extra in Text.unwords ["3",i p, i q, i r]


showT :: Show a => a -> Text.Text
showT = Text.pack . show
