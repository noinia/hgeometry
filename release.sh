#!/bin/sh

cabal build all

echo "--------------------------------------------------------------------------------"
echo "hgeometry-combinatorial"
echo "--------------------------------------------------------------------------------"
cd hgeometry-combinatorial
cabal check
cabal sdist
cd ..


echo "--------------------------------------------------------------------------------"
echo "hgeometry"
echo "--------------------------------------------------------------------------------"
cd hgeometry
cabal check
cabal sdist
cd ..
