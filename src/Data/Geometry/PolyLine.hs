module Data.Geometry.PolyLine where


import Data.Geometry.Point
import Data.Ext

import qualified Data.Sequence as S

newtype PolyLine d r pe = PolyLine { unPolyLine :: S.Seq (Point d r :+ pe) }
                          -- deriving (Show,Eq,Ord)


data LineSegment d r pe = LineSegment { _start :: Point d r :+ pe
                                      , _end   :: Point d r :+ pe
                                      }
                          -- deriving (Show,Eq,Ord)



-- lineSegments :: [LineSegment d r pe :+ le]
