#!/bin/bash

BUILDDIR=build
INSTALLDIR=$(pwd)/linux
XOPTFOIL_VERSION=1.70beta
TARGET_OS=UNIX

export XOPTFOIL_VERSION
export TARGET_OS

if [ -d "$BUILDDIR" ]; then rm -Rf $BUILDDIR; fi
if [ -d "$INSTALLDIR" ]; then rm -Rf $INSTALLDIR; fi

mkdir build
cd build

cmake \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALLDIR" \
-DCMAKE_BUILD_TYPE:STRING="Release"  \
..
make VERBOSE=1 || exit 1
make install || exit 1

cd ..
