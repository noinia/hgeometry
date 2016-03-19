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
progress. HGeometry currently has only very basic types, and implements only
a few algorithms:

* two (optimal) $O(n \log n)$ time algorithms for convex hull in
  $\mathbb{R}^2$: the typical Graham scan, and a divide and conqueror algorithm,
* an $O(n)$ expected time algorithm for smallest enclosing disk in $\mathbb{R}^$2,
* the well-known Douglas Peucker polyline line simplification algorithm,
* an $O(n \log n)$ time Delanay triangulation algorithm (using divide and conqueror).



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
  SimplePolygon :: C.CList (Point 2 r :+ p)                         -> Polygon Simple p r
  MultiPolygon  :: C.CList (Point 2 r :+ p) -> [Polygon Simple p r] -> Polygon Multi  p r
  ```

In all places this extra data is accessable by the (:+) type in Data.Ext, which
is essentially just a pair.

Reading and Writing Ipe files
-----------------------------

Apart from geometric types, HGeometry provides some interface for reading and
writing Ipe (http://ipe7.sourceforge.net). However, this is all very work in
progress. Hence, the API is experimental and may change at any time!
