module Data.Geometry.Arrangement( Arrangement(..)
                                , inputLines, subdivision, boundedArea, unboundedIntersections
                                , ArrangementBoundary

                                , constructArrangement
                                , constructArrangementInBox
                                , constructArrangementInBox'

                                , traverseLine
                                , findStart, findStartVertex, findStartDart
                                , follow
                                ) where


import Data.Geometry.Arrangement.Internal
