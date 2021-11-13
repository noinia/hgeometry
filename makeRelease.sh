#!/bin/sh

pkgs="hgeometry-combinatorial hgeometry hgeometry-ipe"

for pkg in ${pkgs} ; do
  cabal build ${pkg}
done

for pkg in ${pkgs} ; do
  cabal sdist ${pkg}
done
