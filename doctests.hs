import Test.DocTest

import Data.Monoid

main = doctest $ ["-isrc" ] ++ ghcExts ++ files

ghcExts = map ("-X" ++)
          [ "TypeFamilies"
          , "GADTs"
          , "KindSignatures"
          , "DataKinds"
          , "TypeOperators"
          , "ConstraintKinds"
          , "PolyKinds"
          , "RankNTypes"

          , "PatternSynonyms"
          , "ViewPatterns"
          , "MultiParamTypeClasses"

          , "StandaloneDeriving"
          , "GeneralizedNewtypeDeriving"
          , "FlexibleInstances"
          , "FlexibleContexts"
          ]

files = mconcat [ geomModules
                , dataModules
                ]

prefixWith s = map (\s' -> "src/" <> s <> s')


dataModules = prefixWith "Data/" [ "Range.hs"
                                 ]

geomModules = prefixWith "Data/Geometry/" [ "Point.hs"
                                          , "Vector.hs"
                                          , "Line.hs"
                                          , "Line/Internal.hs"                                                                        , "Interval.hs"
                                          , "LineSegment.hs"
                                          , "PolyLine.hs"
                                          , "Ball.hs"
                                          , "Box.hs"
                                          ]
