HGeometry
=========

[![Build Status](https://travis-ci.org/noinia/hgeometry.svg?branch=master)](https://travis-ci.org/noinia/hgeometry)
[![Hackage](https://img.shields.io/hackage/v/hgeometry.svg)](https://hackage.haskell.org/package/hgeometry)

HGeometry is a library for computing with geometric objects in
Haskell. It defines basic geometric types and primitives, and it
implements some geometric data structures and algorithms. The main two
focusses are:

- 1. Strong type safety, and
- 2. implementations of geometric algorithms and data structures that
have good asymptotic running time guarantees.

Design choices showing these aspects are for example:

- we provide a data type `Point d r` parameterized by a
type-level natural number `d`, representing d-dimensional points (in all cases
our type parameter `r` represents the (numeric) type for the (real)-numbers):

```haskell
newtype Point (d :: Nat) (r :: *) = Point { toVec :: Vector d r }
```
- the vertices of a `PolyLine d p r` are stored in a `Data.LSeq` which enforces
that a polyline is a proper polyline, and thus has at least two vertices.

Please note that aspect two, implementing good algorithms, is much work in
progress. Only a few algorithms have been implemented, some of which could use
some improvements.

HGeometry Packages
------------------

HGeometry is split into a few smaller packages. In particular:

- hgeometry-combinatorial : defines some non-geometric
  (i.e. combinatorial) data types, data structures, and algorithms.
- hgeometry-ipe : defines functions for working with [ipe](http://ipe.otfried.org) files.
- hgeometry-svg : defines functions for working with svg files.
- hgeometry-interactive : defines functions for building an
  interactive viewer using [miso](https://haskell-miso.org).
- hgeometry : defines the actual geometric data types, data
  structures, and algorithms.

In addition there is a [hgeometry-examples](hgeometry-examples)
package that defines some example applications, and a hgometry-test
package that contains all testcases. The latter is to work around a
bug in cabal.

Available Geometric Algorithms
------------------------------

Apart from some basic geometric primitives such as intersecting
line segments, testing if a point lies in a polygon etc, HGeometry
implements some more advanced geometric algorithms. In particuar, the
following algorithms are currently available:

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
* An \(O(nm)\) time algorithm to compute the discrete Fr\'echet
  distance of two sequences of points (curves) of length \(n\) and
  \(m\), respectively.

Available Geometric Data Structures
-----------------------------------

HGeometry also contains an implementation of some geometric data
structures. In particular,

* A one dimensional Segment Tree. The base tree is static.
* A one dimensional Interval Tree. The base tree is static.
* A KD-Tree. The base tree is static.

There is also support for working with planar subdivisions. As a
result, [hgeometry-combinatorial] also includes a data structure for
working with planar graphs. In particular, it has an `EdgeOracle` data
structure, that can be built in \(O(n)\) time that can test if the
planar graph contains an edge in constant time.


Avoiding Floating-point issues
-------------------------------

All geometry types are parameterized by a numerical type `r`. It is well known
that Floating-point arithmetic and Geometric algorithms don't go well together;
i.e. because of floating point errors one may get completely wrong
results. Hence, I *strongly* advise against using `Double` or `Float` for these
types. In several algorithms it is sufficient if the type `r` is
`Fractional`. Hence, you can use an exact number type such as `Rational`.


Working with additional data
----------------------------

In many applications we do not just have geometric data, e.g. `Point d r`s or
`Polygon r`s, but instead, these types have some additional properties, like a
color, size, thickness, elevation, or whatever. Hence, we would like that our
library provides functions that also allow us to work with `ColoredPolygon r`s
etc. The typical Haskell approach would be to construct type-classes such as
`PolygonLike` and define functions that work with any type that is
`PolygonLike`. However, geometric algorithms are often hard enough by
themselves, and thus we would like all the help that the type-system/compiler
can give us. Hence, we choose to work with concrete types.

To still allow for some extensibility our types will use the Ext (:+)
type, as defined in the hgeometry-combinatorial package. For example,
our `Polygon` data type, has an extra type parameter `p` that allows
the vertices of the polygon to cary some extra information of type `p`
(for example a color, a size, or whatever).

```haskell
data Polygon (t :: PolygonType) p r where
  SimplePolygon :: C.CSeq (Point 2 r :+ p)                         -> Polygon Simple p r
  MultiPolygon  :: C.CSeq (Point 2 r :+ p) -> [Polygon Simple p r] -> Polygon Multi  p r
```
In all places this extra data is accessable by the (:+) type in Data.Ext, which
is essentially just a pair.
