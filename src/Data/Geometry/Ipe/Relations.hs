{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Ipe.Relations where

import           Control.Applicative
import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Ball
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Line
import           Data.Geometry.Point
import qualified Data.List.NonEmpty as NE
import qualified Data.Traversable as Tr

import           Data.Geometry.Properties
import           Data.Geometry.Transformation(Transformation(..), Matrix
                                             , transformBy, transformationMatrix
                                             , translation, uniformScaling, (|.|))
import           Data.Semigroup
import qualified Data.Seq2     as S2
import           Data.Text


--------------------------------------------------------------------------------

symbolWithName :: Text -> IpeSymbol r -> Maybe (IpeSymbol r)
symbolWithName n s@(Symbol _ n')
                   | n == n'   = Just s
                   | otherwise = Nothing


_mark   :: Text -> Prism' (IpeSymbol r) (Point 2 r)
_mark n = prism' (flip Symbol n)
                 (fmap _symbolPoint . symbolWithName n)


_diskMark :: Prism' (IpeSymbol r) (Point 2 r)
_diskMark = _mark "mark/disk(sx)"


--------------------------------------------------------------------------------


-- _polylinePath :: Prism' (Path r) (PolyLine 2 () r)
_polylinePath :: Prism' (Path r) (PolyLine 2 () r)
_polylinePath = prism' (Path . S2.l1Singleton . PolyLineSegment)
                       (fmap combine . Tr.mapM (^?_PolyLineSegment) . _pathSegments)

combine :: S2.ViewL1 (PolyLine 2 () r) -> PolyLine 2 () r
combine = sconcat . S2.toNonEmpty
