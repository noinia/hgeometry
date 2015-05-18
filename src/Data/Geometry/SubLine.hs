{-# LANGUAGE TemplateHaskell  #-}
module Data.Geometry.SubLine where

import Control.Lens
import Data.Ext
import Data.Geometry.Properties
import Data.Geometry.Interval
import Data.Geometry.Line.Internal
import Data.Geometry.Point
import Data.Geometry.Vector

data SubLine d l u p r = SubLine { _line     :: Line d r
                                 , _subRange :: GInterval l u p r
                                 }
                         -- deriving (Show)
makeLenses ''SubLine



-- | Get the point at the given position along line, where 0 corresponds to the
-- anchorPoint of the line, and 1 to the point anchorPoint .+^ directionVector
pointAt              :: (Num r, Arity d) => r -> Line d r -> Point d r
pointAt a (Line p v) = p .+^ (a *^ v)






-- | Annotate the subRange with the actual ending points
fixEndPoints    :: (Num r, Arity d) => SubLine d l u p r -> SubLine d l u (Point d r :+ p) r
fixEndPoints sl = sl&subRange %~ f
  where
    ptAt = flip pointAt (sl^.line)
    f (Range (l :+ le) (u :+ ue)) = Range (l :+ (ptAt l :+ le))
                                          (u :+ (ptAt u :+ ue))



-- data NoIntersection = NoIntersection deriving (Show,Read,Eq,Ord)

-- type family IntersectionOf g h :: [*]


-- type Intersection1 g h = CoRec Identity (IntersectionOf g h)


type instance IntersectionOf (SubLine 2 l u p r) (SubLine 2 l' u' q r) = [ NoIntersection
                                                                         , Point 2 r
                                                                         ]




type instance IntersectionOf (Range l u a) (Range l u a) = [ NoIntersection
                                                           , a
                                                           , Range l u a
                                                           ]
type instance IntersectionOf (Range l u a) (Range l' u a) = [ NoIntersection
                                                            , a
                                                            , Range l  u a
                                                            , Range l' u a
                                                            ]
