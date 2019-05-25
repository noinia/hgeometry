--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Arrangement
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing an Arrangement of lines in \(\mathbb{R}^2\).
--
--------------------------------------------------------------------------------
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
