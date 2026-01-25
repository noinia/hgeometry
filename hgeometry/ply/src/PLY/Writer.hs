{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
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


  , Coloured(..)
  , VertexAttributes(VertexAttributes), vtxColour
  ) where

import           Control.Lens
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Colour.SRGB (Colour, RGB(..), toSRGB24)
import           Data.Default
import qualified Data.Foldable as F
import           Data.Foldable1
import           Data.Kind (Type)
import           Data.Proxy
import           HGeometry.Ext
import           HGeometry.Point
import qualified System.File.OsPath as File
import           System.OsPath

--------------------------------------------------------------------------------

data Coloured = Coloured | NotColoured
  deriving (Show,Eq)

type family VtxColour (coloured :: Coloured) :: Type where
  VtxColour 'Coloured   = Colour Double
  VtxColour NotColoured = ()

newtype VertexAttributes (coloured :: Coloured) =
  VertexAttributes { _vtxColour :: (VtxColour coloured)
                   }
makeLenses ''VertexAttributes


deriving instance Show (VtxColour coloured) => Show (VertexAttributes coloured)
deriving instance Eq   (VtxColour coloured) => Eq (VertexAttributes coloured)

instance Default (VertexAttributes NotColoured) where
  def = VertexAttributes  ()


-- | Writes the the points and triangles to a file in PLY format.
renderOutputToFile           :: ( Foldable1 f, Foldable1 g, Foldable1 face
                                , Point_ point 3 r, Show r, RenderVtxColour coloured
                                )
                             => OsPath
                             -> f (Int, point :+ VertexAttributes coloured)
                             -> g (face Int) -> IO ()
renderOutputToFile fp pts ts = File.writeFile fp $ renderOutput pts ts

-- | Generates the content of the PLY file for the given non-empty list of vertices and
-- the non-empty list of convex faces.
--
-- Each face is given by a non-empty list of vertexId's, which are assumed to be
-- 0-indexed.
--
renderOutput                :: forall f g face point r coloured.
                               ( Foldable1 f, Foldable1 g
                               , Foldable1 face
                               , Point_ point 3 r
                               , Show r, RenderVtxColour coloured
                               )
                            => f (Int, point :+ VertexAttributes coloured)
                            -> g (face Int) -> Char8.ByteString
renderOutput vertices faces =
    Char8.unlines $ hdr <> map renderVtx (F.toList vertices) <> map renderFace (F.toList faces)
  where
    hdr = [ "ply"
          , "format ascii 1.0"
          , "element vertex " <> (showT $ F.length vertices)
          , "property float32 x"
          , "property float32 y"
          , "property float32 z"
          ] <> vtxColourHeader (Proxy @coloured) <>
          [ "element face " <> (showT $ F.length faces)
          , "property list uchar int vertex_index"
          , "end_header"
          ]


class RenderVtxColour coloured where
  -- | The attributes for the vertex colour
  vtxColourHeader :: proxy coloured -> [Char8.ByteString]
  vtxColourHeader = const []
  -- | Renders the actual vertex color
  renderVtxColour   :: proxy coloured -> VtxColour coloured -> [Char8.ByteString]
  renderVtxColour _ = const []


instance RenderVtxColour 'Coloured where
  vtxColourHeader _ = [ "property uchar red"
                      , "property uchar green"
                      , "property uchar blue"
                      ]
  renderVtxColour _ c = case showT <$> toSRGB24 c of
                          RGB r g b -> [r,g,b]

instance RenderVtxColour NotColoured



renderAttributes  :: forall coloured. RenderVtxColour coloured
                  => VertexAttributes coloured -> [Char8.ByteString]
renderAttributes (VertexAttributes c) = renderVtxColour (Proxy @coloured) c


-- | Writes a vertex to ply format
renderVtx               :: (Point_ point 3 r, Show r, RenderVtxColour coloured)
                        => (index, point :+ VertexAttributes coloured) -> Char8.ByteString
renderVtx (_, p :+ ats) = let Point3 x y z = over coordinates showT $ p^.asPoint
                          in Char8.unwords $ [x,y,z] <> renderAttributes ats

-- | Writes a face to ply format.
renderFace      :: Foldable1 face => face Int -> Char8.ByteString
renderFace face = Char8.unwords . map showT $ (F.length face) : (F.toList face)

-- | Helper to output stuff to bytestrings
showT :: Show a => a -> Char8.ByteString
showT = Char8.pack . show
