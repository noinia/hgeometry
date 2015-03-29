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

asIpe     :: Prism' i g -> g -> i
asIpe p g = p # g


-- asIpe' :: Prism' i g -> g -> IpeObject (NumType i) '[]

-- asIpe' :: Prism' i g -> g -> Rec (a (NumType i) ats) -> i :+ (Rec (a (NumType i) ats))

(=:) :: proxy s -> SymbolAttrElf s r ->  Rec (SymbolAttribute r) '[s]
_ =: x = SymbolAttribute x :& RNil




-- asIpe'       :: Prism' i g -> g -> Rec a ats -> i :+ Rec a ats
-- asIpe' p g r = p # g :+ r

asIpe'       :: Prism' i g -> g -> Rec (a (NumType i)) ats -> i :+ Rec (a (NumType i)) ats
asIpe' p g r = p # g :+ r


asIpe''       :: (Rec (f r) ats ~ IpeObjectAttrElF r (it ats)
                 , r            ~ NumType i
                 , i            ~ IpeObjectValueElF r (it ats)
                 )
              => Prism' i g -> g -> IpeObjectAttrElF r (it ats) -> IpeObject r (it ats)
asIpe'' p g r = IpeObject $ asIpe' p g r
-- TODO: we need that IpeObjectValueElF is injective, so that i determines f. I guess
-- the test below should compile then:
-- TODO: We also need that it is injective?
-- test = asIpe'' _diskMark origin $ (SSymbolStroke =: IpeColor (Named "red"))
--                                  <+>
--                                  RNil

-- asIpe'''       :: (Rec (f r) ats ~ IpeObjectAttrElF r (it ats)
--                   , r            ~ NumType i
--                   , i            ~ IpeObjectValueElF r (it ats)
--                   )
--                   => Prism' i g -> g -> Rec (f r) ats -> IpeObject r (it ats)
-- asIpe''' p g r = IpeObject $ asIpe' p g r
-- | We need to expose ats to the outside world as well


-- asIpe''' :: (

--             )




-- | IpeObjectValueElF/IpeObjectAttrElF needs to be injective in 'it'.
asObject       :: Proxy it -> Proxy ats -> IpeObjectValueElF r (it ats) -> IpeObjectAttrElF r (it ats)
               -> IpeObject r (it ats)
asObject _ _ x r = IpeObject $ x :+ r

asObject''       :: ( Rec (f r) ats    ~ IpeObjectAttrElF r (it ats)
                    , RevIpeObjectValueElF (IpeObjectValueElF r (it ats)) ~ it
                   )
                => IpeObjectValueElF r (it ats) -> IpeObjectAttrElF r (it ats)
               -> IpeObject r (it ats)
asObject'' x r = IpeObject $ x :+ r


asObject'       :: ( Rec (f r) ats ~ IpeObjectAttrElF r (it ats)
                   )
                => Proxy it ->
                   IpeObjectValueElF r (it ats) -> IpeObjectAttrElF r (it ats)
               -> IpeObject r (it ats)
asObject' _ x r = IpeObject $ x :+ r


-- foo
--   :: IpeObjectValueElF r fld
--      -> IpeObjectAttrElF r fld -> IpeObject r fld
-- foo x r = IpeObject $ x :+ r

test2 = asObject'' -- (Proxy :: Proxy IpeUse)
                  (Symbol origin "foo")
                  ((SSymbolStroke =: IpeColor (Named "red")))

-- test = asIpe''' _diskMark origin $ (SSymbolStroke =: IpeColor (Named "red"))
--                                  <+>
--                                  RNil
--   where
--     pr = Proxy :: Proxy SymbolAttribute


test1 = asIpe' _diskMark origin $ (SSymbolStroke =: IpeColor (Named "red"))
                                 <+>
                                 RNil


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
