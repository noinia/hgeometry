HGeometry-ipe
=============

This package provides an API for reading and writing Ipe
(http://ipe.otfried.org) files. This is all very work in
progress. Hence, the API is experimental and may change at any time!

Here is an example showing reading a set of points from an Ipe file,
computing the DelaunayTriangulation and the Euclidean Minimimum
Spanning tree, and writing the result again to an output file. Refer
to
[hgeometry-examples](https://github.com/noinia/hgeometry/tree/master/hgeometry-examples/src/Demo/Delaunay.hs)
for the full code.


```haskell
mainWith                          :: Options -> IO ()
mainWith (Options inFile outFile) = do
  pts <- readAllFrom @(Point 2 R) inFile
  let pts' = NonEmpty.fromList pts
      dt   = toPlanarSubdivision (Proxy @DTWorld) . delaunayTriangulation $ pts'
      emst = euclideanMST pts'
      out  = [ iO $ drawPlanarSubdivisionWith drawVtx drawEdge (drawInternalFace dt) drawOuterFace dt
                  ! attr SLayer "delaunayTriangulation"
             , iO $ drawTree' emst ! attr SLayer "emst"
             ]
      outputFile = singlePageFromContent out
  outputFile' <- addStyleSheetFrom "../hgeometry-ipe/resources/opacities.isy" outputFile
  writeIpeFile outFile outputFile'

-- | The world in which the delaunay triangulation "lives"
data DTWorld

-- | Draw vertices using their default representation; disk marks. For
-- the rest we keep their original attributes.
drawVtx                         :: IpeOut' Maybe (VertexId' s, VertexData r (IpeAttributes IpeSymbol r)) IpeSymbol r
drawVtx (_vi, VertexData p ats) = Just $ defIO p ! ats

-- | Draw edges using normal line segments
drawEdge              :: IpeOut' Maybe (Dart s,      LineSegment 2 v r :+ e)  Path r
drawEdge (_d, s :+ _) = Just $ defIO s

-- | Internal faces are filled polygons.
drawInternalFace                 :: PlanarSubdivision s v e f r
                                 -> IpeOut' Maybe (FaceId' s,   SomePolygon v r :+ f)    Path r
drawInternalFace s (fi, pg :+ _) = Just $ defIO pg ! attr SFill lightcyan

--
drawOuterFace :: (Ord r, Num r) => IpeOut' Maybe (FaceId' s,   MultiPolygon (Maybe v) r :+ f) Path r
drawOuterFace (_, pg :+ _) = Just $ defIO pg ! attr SOpacity "10%"
                                             ! attr SFill lightgray
```

See the [hgeometry-examples](https://github.com/noinia/hgeometry/tree/master/hgeometry-examples) package for more examples.
