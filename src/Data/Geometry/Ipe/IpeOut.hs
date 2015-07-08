{-# LANGUAGE OverloadedStrings #-}
module Data.Geometry.Ipe.IpeOut where

import           Control.Applicative
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Ball
import           Data.Geometry.Ipe.Attributes
import           Data.Geometry.Ipe.Types
import           Data.Geometry.PolyLine
import           Data.Geometry.Point
import qualified Data.List.NonEmpty as NE
import           Data.Proxy
import qualified Data.Traversable as Tr

--------------------------------------------------------------------------------

newtype IpeOut g i = IpeOut { asIpe :: g -> i }



-- | Given a Prism to convert a geometry object into an ipe geometry object,
-- the geometry object, and a record with its attributes, construct its corresponding
-- ipe Object.
asIpeObject         :: ( Rec (f r) ats ~ IpeObjectAttrElF r (it ats)
                       , RevIpeObjectValueElF i ~ it
                       , i ~ IpeObjectValueElF r (it ats)
                       )
                    => IpeOut g i
                    -> g -> Rec (f r) ats -> IpeObject r (it ats)
asIpeObject f g ats = asObject (asIpe f g) ats

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


mark   :: Text -> IpeOut (Point 2 r) (IpeSymbol r)
mark n = IpeOut $ flip Symbol n

diskMark :: IpeOut (Point 2 r) (IpeSymbol r)
diskMark = mark "mark/disk(sx)"



--------------------------------------------------------------------------------

-- lineSegment :: ToIpe (LineSegment 2 p r) (PathSegment r)
-- lineSegment = PolyLineSegment ()



--path



circle                   :: Floating r => IpeOut (Circle p r) (PathSegment r)
circle = ToIpe circle'
  where
    circle' (Ball (c :+ _) r) = EllipseSegment m
      where
        m = translation (toVec c) |.| uniformScaling (sqrt r) ^. transformationMatrix
        -- m is the matrix s.t. if we apply m to the unit circle centered at the origin, we
        -- get the input circle.
