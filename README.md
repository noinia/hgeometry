[![Build Status](https://travis-ci.org/noinia/hgeometry.svg?branch=master)](https://travis-ci.org/noinia/hgeometry)

HGeometry
=========

HGeometry provides some basic geometry types, and geometric algorithms and data
structures for them. The main two focusses are: (1) Strong type safety, and (2)
asymptotically optimal implementations. Examples of (1) are: it provides a type
'Point d r' parameterized by the dimension d and the numerical type
representing real numbers r, and that it uses a Non-empty sequence type to make
sure that any `PolyLine d r' is proper: i.e. it has at least two end points. As
an example of (2): it implements an $O(n \log n)$ time algorithm for convex
hull in $R^2$.

HGeometry currently has only very basic types, and implements only two
algorithms: $O(n \log n)$ time convex hull and $O(n)$ expected time smallest
enclosing disk (both in $R^2$).

Current work is on implementing $O(n \log n + k)$ time red-blue line segment
intersection. This would also allow for efficient polygon intersection and map
overlay.

Appart from geometric types, HGeometry provides some interface for reading and
writing Ipe (http://ipe7.sourceforge.net). However, this is all very
rudementary and experimental. (As is the rest of the package by the way.)
