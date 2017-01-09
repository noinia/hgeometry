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
          , "TupleSections"
          , "MultiParamTypeClasses"
          , "LambdaCase"
          , "TupleSections"


          , "StandaloneDeriving"
          , "GeneralizedNewtypeDeriving"
          , "DeriveFunctor"
          , "DeriveFoldable"
          , "DeriveTraversable"
          , "AutoDeriveTypeable"
          , "DeriveGeneric"
          , "FlexibleInstances"
          , "FlexibleContexts"
          ]

files = mconcat [ geomModules
                , dataModules
                ]

prefixWith s = map (\s' -> "src/" <> s <> s')


dataModules = prefixWith "Data/" [ "Range.hs"
                                 , "CircularList/Util.hs"
                                 , "Permutation.hs"
                                 , "CircularSeq.hs"
                                 , "PlanarGraph.hs"
                                 ]

geomModules = prefixWith "Data/Geometry/" [ "Point.hs"
                                          , "Vector.hs"
                                          , "Line.hs"
                                          , "Line/Internal.hs"                                                                        , "Interval.hs"
                                          , "LineSegment.hs"
                                          , "PolyLine.hs"
                                          , "Polygon.hs"
                                          , "Ball.hs"
                                          , "Box.hs"
                                          ]
