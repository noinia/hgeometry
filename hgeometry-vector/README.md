hgeometry-vector
================

Vectors with type level tags and an optimized representation. This
package defines a type 'Vector d r', which represents vectors of
length d, storing elements of type 'r'. We try to pick an optimized
representation of such vectors, depending on their length d, and the
type r. In particular, for primtive types such as Int, we unbox and
unpack the Ints directly into the vector to avoid indirection.

All of this is controlled through the 'VectorFamily' type family,
which lets you specify a particular vector implementation.

For "large" types, we also provide Boxed vectors. Furthermore, the
WrapVector type can be used to easily implement optimal vectors for
newtypes.
