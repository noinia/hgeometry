import Test.DocTest

main = doctest $ ["-isrc" ] ++ ghcExts ++ files


ghcExts = map ("-X" ++)
          [ "TypeFamilies"
          , "GADTs"
          , "KindSignatures"
          , "DataKinds"
          , "TypeOperators"
          , "ConstraintKinds"
          ]

files = ["src/Data/Geometry/Point.hs"]
