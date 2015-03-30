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
import           Data.Geometry.Transformation( Transformation(..), Matrix
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

-- _circle :: (Floating r, Ord r) => Prism' (Path r) (Circle r)
-- _circle = prism' (Path . view _singletonSeq)
--                  (fmap Path . review singletonSeq)



-- | For PathSegment -> Circle we only need Num, so use the `fromCircle'
-- function instead if that is the goal.
_ellipseSegment :: (Floating r, Ord r) => Prism' (PathSegment r) (Circle r)
_ellipseSegment = prism' fromCircle f
  where
    f (EllipseSegment m) = fromEllipse (Ellipse m)
    f _                  = Nothing


fromCircle            :: Floating r => Circle r -> PathSegment r
fromCircle (Ball c r) = EllipseSegment m
  where
    m = translation (toVec c) |.| uniformScaling (sqrt r) ^. transformationMatrix
    -- m is the matrix s.t. if we apply m to the unit circle centered at the origin, we
    -- get the input circle.

fromEllipse :: (Num r, Ord r) => Operation r -> Maybe (Circle r)
fromEllipse (Ellipse m) | q `onBall` b = Just b
                        | otherwise    = Nothing
  where
    t = Transformation m
    c = transformBy t origin
    p = transformBy t (point2 1 0)
    q = transformBy t (point2 0 1)
    b = fromCenterAndPoint c p



--------------------------------------------------------------------------------

_polyLineSegment :: Prism' (PathSegment r) (PolyLine 2 () r)
_polyLineSegment = _PolyLineSegment


-- _polyP :: Prism' (Path r) (PolyLine 2 () r)
-- _polyP = pathSegments.
--          _mergedSingletonSeq

-- mergePathSegments           :: S2.ViewL1 (PathSegment r) -> Maybe (PathSegment r)
-- mergePathSegments (s :< ss) = F.foldr f (Just s) ss
--   where
--     f _   Nothing = Nothing
--     f s' (Just t) = s' `merge` t

--     (PolyLineSegment p) `merge` (PolyLineSegment q) = Just $ PolyLineSegment $ p <> q
--     _                                               = Nothing


-- | A prism between a nonempty list and a single elent. The elements of the
-- non-empty sequence are combined back into the one element using their
-- semigroup instance.
_mergedSingletonSeq :: Semigroup a => Prism' (S2.ViewL1 a) a
_mergedSingletonSeq = prism' S2.l1Singleton (Just . sconcat . S2.toNonEmpty)


-- | Prism to convert between an element, and a singleton ViewL1. Note that
-- this version is explicit in checking that the list given contains exactly one
-- element. If the seq contains more than one, it produces a nothing.
_singletonSeq :: Prism' (S2.ViewL1 a) a
_singletonSeq = prism' S2.l1Singleton (f . S2.toNonEmpty)
  where
    f (x NE.:| []) = Just x
    f _            = Nothing


-- _singlePathSegment = Prism' (Path r) (PathSegment r)
-- _singlePathSegment = prism' (Path . S2.l1Singleton)
--                             (
--                             )

-- _polylinePath :: Prism' (Path r) (PolyLine 2 () r)
_polyLinePath :: Prism' (Path r) (PolyLine 2 () r)
_polyLinePath = prism' (Path . S2.l1Singleton . PolyLineSegment)
                       (fmap combine . Tr.mapM (^?_PolyLineSegment) . _pathSegments)

combine :: S2.ViewL1 (PolyLine 2 () r) -> PolyLine 2 () r
combine = sconcat . S2.toNonEmpty

--------------------------------------------------------------------------------

-- _singlePage :: Prism' (IpePage gs r) (Group gs r)
-- _singlePage = prism' (IpePage [] [])






--------------------------------------------------------------------------------

(=:) :: proxy s -> SymbolAttrElf s r ->  Rec (SymbolAttribute r) '[s]
_ =: x = SymbolAttribute x :& RNil



test2 = asObject  (Symbol origin "foo")
                  ((SSymbolStroke =: IpeColor (Named "red")))


test1 = asIpeObject _diskMark origin $ (SSymbolStroke =: IpeColor (Named "red"))
                                       <+>
                                       RNil
