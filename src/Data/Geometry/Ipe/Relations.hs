{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Ipe.Relations where


import           Control.Applicative
import           Control.Lens
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Ball
import           Data.Geometry.Ipe.Attributes
import           Data.Geometry.Ipe.Types
import           Data.Geometry.Line
import           Data.Geometry.Point
import qualified Data.List.NonEmpty as NE
import           Data.Proxy
import qualified Data.Traversable as Tr

import           Data.Geometry.Properties
import           Data.Geometry.Transformation(Transformation(..), Matrix
                                             , transformBy, transformationMatrix
                                             , translation, uniformScaling, (|.|))
import           Data.Semigroup
import qualified Data.Seq2     as S2
import           Data.Text(Text)
import           Data.Vinyl


--------------------------------------------------------------------------------


-- | Given a prism and a geometry object, convert it into an ipe geometry object.
asIpe     :: Prism' i g -> g -> i
asIpe p g = p # g


-- | Given a Prism to convert a geometry object into an ipe geometry object,
-- the geometry object, and a record with its attributes, construct its corresponding
-- ipe Object.
asIpeObject         :: ( Rec (f r) ats ~ IpeObjectAttrElF r (it ats)
                       , RevIpeObjectValueElF i ~ it
                       , i ~ IpeObjectValueElF r (it ats)
                       )
                    => Prism' i g
                    -> g -> Rec (f r) ats -> IpeObject r (it ats)
asIpeObject p g ats = asObject (asIpe p g) ats

-- | Given one of the ipe values, (i.e. a Path, IpeSymbol, etc.) and a Rec of
-- the appropriate corresponding type, construct an ipe Object from the two.
asObject     :: ( Rec (f r) ats ~ IpeObjectAttrElF r (it ats)
                , RevIpeObjectValueElF (IpeObjectValueElF r (it ats)) ~ it
                )
             => IpeObjectValueElF r (it ats)
             -> IpeObjectAttrElF r (it ats)
             -> IpeObject r (it ats)
asObject x r = IpeObject $ x :+ r


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

--------------------------------------------------------------------------------


(=:) :: proxy s -> SymbolAttrElf s r ->  Rec (SymbolAttribute r) '[s]
_ =: x = SymbolAttribute x :& RNil



test2 = asObject  (Symbol origin "foo")
                  ((SSymbolStroke =: IpeColor (Named "red")))


test1 = asIpeObject _diskMark origin $ (SSymbolStroke =: IpeColor (Named "red"))
                                       <+>
                                       RNil
