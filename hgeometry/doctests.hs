import Test.DocTest

main :: IO ()
main = doctest $ ["-isrc" ] ++ ghcExts ++ files

ghcExts :: [String]
ghcExts = map ("-X" ++)
          [ "TypeFamilies"
          , "GADTs"
          , "KindSignatures"
          , "DataKinds"
          , "TypeOperators"
          , "ConstraintKinds"
          , "PolyKinds"
          , "RankNTypes"
          , "TypeApplications"
          , "ScopedTypeVariables"

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
          , "DeriveGeneric"
          , "FlexibleInstances"
          , "FlexibleContexts"
          , "DerivingStrategies"
          , "DerivingVia"
          ]

files :: [String]
files = map toFile modules

toFile :: String -> String
toFile = (\s -> "src/" <> s <> ".hs") . replace '.' '/'

replace     :: Eq a => a -> a -> [a] -> [a]
replace a b = go
  where
    go []                 = []
    go (c:cs) | c == a    = b:go cs
              | otherwise = c:go cs

modules :: [String]
modules =
  [ "Data.PlaneGraph.IO"

  , "Geometry.Point"
  , "Geometry.Vector"
  , "Geometry.Transformation"
  , "Geometry.Line"
  , "Geometry.Line.Internal"
  , "Geometry.Interval"
  , "Geometry.LineSegment"
  , "Geometry.PolyLine"
  , "Geometry.Polygon"
  , "Geometry.Ball"
  , "Geometry.Box"
  , "Geometry.HyperPlane"

  -- , "Algorithms.Geometry.HiddenSurfaceRemoval.HiddenSurfaceRemoval"
  , "Algorithms.Geometry.ConvexHull.Naive"
  , "Algorithms.Geometry.ConvexHull.JarvisMarch"

  , "Geometry.SoS.Orientation"
  , "Algorithms.Geometry.InPolygon"
  ]
