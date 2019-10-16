HGeometry-ipe
=============

This package provides an API for reading and writing Ipe
(http://ipe.otfried.org) files. This is all very work in
progress. Hence, the API is experimental and may change at any time!

Here is an example showing reading a set of points from an Ipe file,
computing the DelaunayTriangulation, and writing the result again to
an output file

```haskell
mainWith                          :: Options -> IO ()
mainWith (Options inFile outFile) = do
    ePage <- readSinglePageFile inFile
    case ePage of
      Left err                         -> print err
      Right (page :: IpePage Rational) -> case page^..content.traverse._IpeUse of
        []         -> putStrLn "No points found"
        syms@(_:_) -> do
           let pts  = syms&traverse.core %~ (^.symbolPoint)
               pts' = NonEmpty.fromList pts
               dt   = delaunayTriangulation $ pts'
               out  = [iO $ drawTriangulation dt]
           writeIpeFile outFile . singlePageFromContent $ out
```

See the [hgeometry-examples](https://github.com/noinia/hgeometry/tree/master/hgeometry-examples) package for more examples.
