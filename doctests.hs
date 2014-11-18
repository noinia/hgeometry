import Test.DocTest

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

files = ["src/Data/Geometry/Point.hs"]
