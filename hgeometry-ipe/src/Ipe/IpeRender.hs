--------------------------------------------------------------------------------
-- |
-- Module      :  Ipe.IpeRender
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Use 'iperender' to generate png, pdf, or svg files.
--
-- Note that all functions in this module require that 'iperender' is
-- installed (it is bundled with ipe) and available on the path.

--------------------------------------------------------------------------------
module Ipe.IpeRender where

import qualified System.Process.Typed as Process

--------------------------------------------------------------------------------

-- | Call 'iperender' to produce an image of the specified type.
--
-- note that pdf files produces with iperender cannot be opened with ipe.
ipeRenderWith                          :: Options -- ^ the options to use
                                       -> FileType -- ^ output file type
                                       -> FilePath -- ^ input file path
                                       -> FilePath -- ^ output file path
                                       -> IO ()
ipeRenderWith options fType inFp outFp =
    Process.withProcessWait processCfg $ \_iperenderProc -> pure ()
  where
    processCfg = Process.proc "iperender" args
    args = [ "-" <> show fType
           , "-page", show (pageNumber options)
           , "-view", show (viewNumber options)
           , "-resolution", show (resolution options)
           ] <>
           [ "-transparent" | TransparentBackground == transparent options ] <>
           [ "-nocrop"      | NoCrop == crop options ] <>
           [ inFp
           , outFp
           ]

-- | Call 'iperender' with the default options.
--
-- note that pdf files produces with iperender cannot be opened with ipe.
ipeRender :: FileType -> FilePath -> FilePath -> IO ()
ipeRender = ipeRenderWith defaultOptions

--------------------------------------------------------------------------------

-- | Output filetypes supported by iperender
data FileType = PNG | EPS | PDF | SVG
  deriving (Eq,Ord,Enum)

instance Show FileType where
  show = \case
    PNG -> "png"
    EPS -> "eps"
    PDF -> "pdf"
    SVG -> "svg"

-- | Options for iperender
data Options = Options { pageNumber  :: Int
                       , viewNumber  :: Int
                       , resolution  :: Int
                       , transparent :: Background
                       , crop        :: Crop
                       } deriving (Show,Eq,Ord)

-- | The default options in Ipe
defaultOptions :: Options
defaultOptions = Options 1 1 72 TransparentBackground Crop

-- | Whether or not to render a transparent background in output png
-- images.
data Background = OpaqueBackground | TransparentBackground
  deriving (Show,Read,Eq,Ord,Enum)

-- | Whether or not to crop the output image.
data Crop = NoCrop | Crop
  deriving (Show,Read,Eq,Ord,Enum)
