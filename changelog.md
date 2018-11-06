#### 0.8 ###

- Compatibility with GHC 8.6
- Added \(O(n\log n)\) time closest pair algorithm.
- Added arrangement data type
- Various Bugfixes
- Added Camera data type with some world to screen transformations.
- Additional read/show instances
- Updated some of the show instances for Ipe related types.

#### 0.7 ###

- Compatibility with GHC 8.0-8.4
- Implemented more Algorithms and Data Structures. This includes
  * Polygon triangulation
- A new implementation of PlanarSubdivision that now also supports disconnected
  subdivsions.
- Performance improvements by changing to a different Vector
  implementation. For low dimensional vectors (of dimension at most four) we
  now essentially use the types from
  [linear](https://hackage.haskell.org/package/linear), this gives significant
  speedups on several small benchmarks.
- bugfixes.

#### 0.6 ###

- Implemented more Algorithms and Data Structures. This includes
  * Bentley-Ottmannn line-segment intersection,
  * Well-Separated Pair decompositions,
  * extremal point/tangents for Convex hulls,
  * Minkowski sum for convex polygons,
  * one dimensional segment trees,
  * one dimensional interval trees, and a
  * KD-tree.
- Several bug fixes, including a very stupid bug in Box
- Separate ConvexPolygon type.
- More thorough testing for some of the algorithms.
- Started work on a proper representation for planar subdivsions. This includes
  a representation of planar graphs that support querying if two vertices are
  connected by an edge in $O(1)$ time.
- Dropped support for GHC 7.8

#### 0.5 ###

- Implemented several algorithms, including Delaunay Triangulation, EMST, and
Douglas Peucker.
- Revamped the data types for Intersections

#### 0.4 ###

- Major rewrite from scratch, providing much stronger type-level
  guarantees. Incompatible with older versions.
- Convex Hull and Smallest enclosing disk algorithms.
- HGeometry now includes some very experimental and preliminary support for
  reading and writing Ipe7 files.

#### 0.2 & 0.3 ###

- Internal releases.

### 0.1.1 ###

- Fixed a bug in point on n the line segment test
- Generalized the types of inCircle, inDisc, onCircle, onDisc etc. We now need
  only that the type representing precision model implements the typeclass
  `Num` instead of `Floating'.

### 0.1 ###

- Initial release.
