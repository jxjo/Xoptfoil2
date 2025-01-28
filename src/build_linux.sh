#!/bin/bash

cd ..

BUILDDIR=build
INSTALLDIR=$(pwd)/linux
XOPTFOIL_VERSION=1.0.4 
 
export XOPTFOIL_VERSION

if [ -d "$BUILDDIR" ];   then rm -Rf $BUILDDIR; fi
if [ -d "$INSTALLDIR" ]; then rm -Rf $INSTALLDIR; fi

mkdir $BUILDDIR
cd $BUILDDIR

cmake \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALLDIR" \
-DCMAKE_BUILD_TYPE:STRING="Release"  \
..
make VERBOSE=1 || exit 1
make install   || exit 1

cd ../src

