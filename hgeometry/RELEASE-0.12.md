It is my great pleasure to announce version 0.12 of HGeometry!

HGeometry is a swiss army knife for computation geometry. Wondering if a point
is inside a polygon? HGeometry will tell you. Looking for the shortest path
inside a maze? HGeometry will light your way. Planning a museum heist? HGeometry
will show you the camera blindspots in O(n log n) time!

Check out our website for more: https://hgeometry.org/

HGeometry is open to contributors at all levels of Haskell experience and
we have a list of weekend-sized projects. Ask in the Discord server
for guidance and/or pair programming.

New in 0.12:
 - Algorithms:
   - Visibility polygon in O(n log n).
   - Earclip triangulation in O(n^2) worst case, O(n) expected case.
   - Single-source shortest path in O(n).
   - Planar point locator in O(log n).
   - Point set diameter in O(n log n).
   - Convex hull of a polygon in O(n).
   - Diameter of a convex polygon in O(n).
   - Check if a point lies inside a convex polygon in O(n).
 - Bug fixes and improved numerical robustness.
 - Property testing and several methods for generating random polygons.
 - See changelog.org for exhaustive list of changes.

Future developments:
 - Bring Haddock documentation coverage up to 100%. Currently we're in the low eighties.
 - Holistic approach to demonstrating correctness:
    - Proven algorithms.
    - Unit testing for hand-picked corner cases.
    - Property testing with randomly generated polygons / geometric data.
    - Exhaustive property testing with real-world datasets (osm).
 - Benchmarks to show the relative performance of HGeometry's many algorithms.

Contributors:
 - Frank Staals, @noita
 - David Himmelstrup, @Lemmih
 - Hunter DeMeyer, @1ndy

Website: https://hgeometry.org
GitHub:  https://github.com/nionia/hgeometry
Hackage: https://hackage.haskell.org/package/hgeometry
Discord: https://discord.gg/HQwbD9jWqg
