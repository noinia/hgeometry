module Data.Geometry.PolyLine where


import Data.Geometry.Point
import Data.Ext

import qualified Data.Sequence as S

newtype PolyLine d r e = PolyLine { unPolyLine :: S.Seq (Point d r :+ e) }
                         -- deriving (Show,Eq,Ord)


data LineSegment d r e = LineSegment { _start :: Point d r :+ e
                                     , _end   :: Point d r :+ e
                                     }
                         -- deriving (Show,Eq,Ord)



-- lineSegments :: [LineSegment d r pe :+ e']
