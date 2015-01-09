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

          , "StandaloneDeriving"
          , "GeneralizedNewtypeDeriving"
          , "FlexibleInstances"
          , "FlexibleContexts"
          ]

files = geomModules

geomModules = map ("src/Data/Geometry/" <>) [ "Point.hs"
                                            , "Vector.hs"
                                            , "PolyLine.hs"
                                            ]
