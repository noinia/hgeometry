# HGeometry

![GitHub Workflow Status](
https://img.shields.io/github/actions/workflow/status/noinia/hgeometry/haskell-ci.yml?branch=hgeom1_again)
[![Hackage](https://img.shields.io/hackage/v/hgeometry.svg?color=success)](https://hackage.haskell.org/package/hgeometry)
[![API docs coverage](https://img.shields.io/endpoint?url=https%3A%2F%2Fnoinia.github.io%2Fhgeometry%2Fhaddock_badge.json)](https://noinia.github.io/hgeometry/haddocks)

HGeometry is a library for computing with geometric objects such as
points, line segments, and polygons, in Haskell. It defines basic
geometric types and primitive operations on these types (for example
functions to test or compute the intersection of two line segments),
and it implements some geometric data structures and algorithms. The
main two focusses are:

1. To provide idiomatic implementations of geometric algorithms and
   data structures that have good asymptotic running time guarantees,
   and
2. Strong type safety.

Please note that first aspect --implementing good algorithms and data
structures-- is much of a work in progress. Only a few algorithms have
been implemented, and most likely they can use some improvements.

# HGeometry Packages

HGeometry currently consists of three packages:

* hgeometry-combinatorial,

  which defines the non-geometric (i.e. combinatorial) data types,
  data structures, and algorithms.

* hgeometry,

  which defines the actual geometric algorithms and data structures,
  and

* hgeometry-examples

  which defines some examples that showcase using hgeometry.

The hgeometry package itself actually consists of several libraries:

* hgeometry

  The main library that defines the actual algorithms and data
  structures.

* hgeometry:vector

  Defines length annotated Vector types and typeclasses. The
  hgeometry-point library depends on this.

* hgeometry:point

  Defines types and typeclasses representing points in space, and
  basic operations on points.

* hgeometry:kernel

  Defines other geometric "constant complexity" geometric types and
  primitives. For example lines, halfspaces, line segments, balls,
  circles, rectangles etc.

* hgeometry:ipe

  Defines functions for reading, writing, and manipulating
  [ipe](http://ipe.otfried.org) files and the geometric objects
  therein.

* hgeometry:svg

  Defines functions for writing the geometry types to svg files.

### Examples

The [hgeometry-examples](hgeometry-examples) provides some examples of
using the library.

# Available Geometric Algorithms and Data Structures

This is a brief overview of the main available algorithms in
HGeometry. Refer to the haddocks for more details.

* Convex hull algorithms for $n$ points in $\mathbb{R}^2$

  - worst case optimal $O(n\log n)$ time implementations of Graham
    scan, Divide and Conquer,
  - an output sensitive $O(nh)$ time Jarvis March, and
  - a QuickHull implementation (which has worst case complexity $O(n^2)$)

* Closest Pair algorithms for $n$ points in $\mathbb{R}^2$

    - $O(n\log n)$ time using divide and conquer

* Point in polygon tests (in linear time for simple polygons, and in
  $O(\log n)$ time for convex polygons.).

* Finding tangents and extremal points with respect to a polygon

  - $O(\log n)$ time for convex polygons, and
  - linear time in case of simple polygons.

* Tangent finding algorithms (linear time for simple polygons and
  $O(\log n)$ time for convex polygons).


HGeometry also contains an implementation of some geometric data
structures. In particular,

* A one dimensional Interval Tree. The base tree is static.
* A one dimensional Segment Tree. The base tree is static.


# Avoiding Floating-point issues

All geometry types are parameterized by a numerical type `r`. It is
well known that Floating-point arithmetic and Geometric algorithms
don't go well together; i.e. because of floating point errors one may
get completely wrong results. Hence, I *strongly* advise against using
`Double` or `Float` for these types.

In most algorithms it is sufficient if the type `r` is
`Fractional`. Hence, you can use an exact number type such as
`Data.Ratio.Rational` or `HGeometry.Number.Real.Rational` (which is
essentially just a `Rational` with a friendlier `Show` instance).

Interval Arithmetic (to speed up our computations) is one of the
things on the main things on the TODO list.


# Working with additional data

In many applications we do not just have geometric data, e.g. `Point d
r`s or `Polygon r`s, but instead, these types have some additional
properties, like a color, size, thickness, elevation, or whatever. We
use typeclasses to make sure it is easy to use the functions with
custom geometric types that store such additional fields. For example,
the 2d convex hull algorithms have type:

```haskell
convexHull :: (Ord r, Num r, Point_ point 2 r) => NonEmpty point -> ConvexPolygon point
```

In many cases you may not want to explicitly declare a new specific
point type, but just "attach" an additional value (e.g. a color) to a
point. You may want to use the `Ext` type (typically seen as `(:+)`
from `Heometry.Ext` in such cases.


# Build Instructions

HGeometry heavily relies on typeclasses to support polymorphic
inputs. Therefore, if you are using this package, it is recommended to
compile your package with GHC options `-fspecialise-aggressively
-fexpose-all-unfoldings` to make sure GHC sufficiently specializes the
calls. You can do so by adding the following to your
executable/library stanza in your cabal file:


```cabal
    ghc-options: -fspecialise-aggressively -fexpose-all-unfoldings
```

Not doing so may significantly impact the performance of your compiled
code.
