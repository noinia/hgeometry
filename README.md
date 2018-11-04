HGeometry
=========

[![Build Status](https://travis-ci.org/noinia/hgeometry.svg?branch=master)](https://travis-ci.org/noinia/hgeometry)
[![Hackage](https://img.shields.io/hackage/v/hgeometry.svg)](https://hackage.haskell.org/package/hgeometry)

HGeometry provides some basic geometry types, and geometric algorithms and data
structures for them. The main two focusses are: (1) Strong type safety, and (2)
implementations of geometric algorithms and data structures with good
asymptotic running time guarantees. Design choices showing these aspects are
for example:

- we provide a data type `Point d r` parameterized by a
type-level natural number `d`, representing d-dimensional points (in all cases
our type parameter `r` represents the (numeric) type for the (real)-numbers):

```haskell
newtype Point (d :: Nat) (r :: *) = Point { toVec :: Vector d r }
```
- the vertices of a `PolyLine d p r` are stored in a `Data.Seq2` which enforces
that a polyline is a proper polyline, and thus has at least two vertices.

Please note that aspect (2), implementing good algorithms, is much work in
progress. Only a few algorithms have been implemented, some of which could use
some improvements. Currently, HGeometry provides the following algorithms:

* two \(O(n \log n)\) time algorithms for convex hull in
  $\mathbb{R}^2$: the typical Graham scan, and a divide and conquer algorithm,
* an \(O(n)\) expected time algorithm for smallest enclosing disk in $\mathbb{R}^$2,
* the well-known Douglas Peucker polyline line simplification algorithm,
* an \(O(n \log n)\) time algorithm for computing the Delaunay triangulation
(using divide and conquer).
* an \(O(n \log n)\) time algorithm for computing the Euclidean Minimum Spanning
Tree (EMST), based on computing the Delaunay Triangulation.
* an \(O(\log^2 n)\) time algorithm to find extremal points and tangents on/to a
  convex polygon.
* An optimal \(O(n+m)\) time algorithm to compute the Minkowski sum of two convex
polygons.
* An \(O(1/\varepsilon^dn\log n)\) time algorithm for constructing a Well-Separated pair
  decomposition.
* The classic (optimal) \(O(n\log n)\) time divide and conquer algorithm to
  compute the closest pair among a set of \(n\) points in \(\mathbb{R}^2\).

It also has some geometric data structures. In particular, HGeometry contans an
implementation of

* A one dimensional Segment Tree. The base tree is static.
* A one dimensional Interval Tree. The base tree is static.
* A KD-Tree. The base tree is static.

HGeometry also includes a datastructure/data type for planar graphs. In
particular, it has a `EdgeOracle` data structure, that can be built in \(O(n)\)
time that can test if the graph contains an edge in constant time.

Numeric Types
-------------

All geometry types are parameterized by a numerical type `r`. It is well known
that Floating-point arithmetic and Geometric algorithms don't go well together;
i.e. because of floating point errors one may get completely wrong
results. Hence, I *strongly* advise against using `Double` or `Float` for these
types. In several algorithms it is sufficient if the type `r` is
`Fractional`. Hence, you can use an exact number type such as `Rational`.

A Note on the Ext (:+) data type
---------------------------------

In many applications we do not just have geometric data, e.g. `Point d r`s or
`Polygon r`s, but instead, these types have some additional properties, like a
color, size, thickness, elevation, or whatever. Hence, we would like that our
library provides functions that also allow us to work with `ColoredPolygon r`s
etc. The typical Haskell approach would be to construct type-classes such as
`PolygonLike` and define functions that work with any type that is
`PolygonLike`. However, geometric algorithms are often hard enough by
themselves, and thus we would like all the help that the type-system/compiler
can give us. Hence, we choose to work with concrete types.

To still allow for some extensibility our types will use the Ext (:+) type. For
example, our `Polygon` data type, has an extra type parameter `p` that allows
the vertices of the polygon to cary some extra information of type `p` (for
example a color, a size, or whatever).

```haskell
data Polygon (t :: PolygonType) p r where
  SimplePolygon :: C.CSeq (Point 2 r :+ p)                         -> Polygon Simple p r
  MultiPolygon  :: C.CSeq (Point 2 r :+ p) -> [Polygon Simple p r] -> Polygon Multi  p r
```

In all places this extra data is accessable by the (:+) type in Data.Ext, which
is essentially just a pair.

Reading and Writing Ipe files
-----------------------------

Apart from geometric types, HGeometry provides some interface for reading and
writing Ipe (http://ipe.otfried.org). However, this is all very work in
progress. Hence, the API is experimental and may change at any time! Here is an
example showing reading a set of points from an Ipe file, computing the
DelaunayTriangulation, and writing the result again to an output file

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
               out  = [asIpe drawTriangulation dt]
           writeIpeFile outFile . singlePageFromContent $ out
```

See the examples directory for more examples.
