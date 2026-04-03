#!/bin/bash

# Save original directory
ORIGINAL_DIR=$(pwd)

cd ..

BUILDDIR=build
INSTALLDIR=$(pwd)/linux
XOPTFOIL_VERSION=1.1.0 
 
export XOPTFOIL_VERSION

if [ -d "$BUILDDIR" ];   then rm -Rf $BUILDDIR; fi
if [ -d "$INSTALLDIR" ]; then rm -Rf $INSTALLDIR; fi

mkdir $BUILDDIR
cd $BUILDDIR

cmake \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALLDIR" \
-DCMAKE_BUILD_TYPE:STRING="Release"  \
..

if [ $? -ne 0 ]; then
  cd "$ORIGINAL_DIR"
  exit 1
fi

make VERBOSE=1
if [ $? -ne 0 ]; then
  cd "$ORIGINAL_DIR"
  exit 1
fi

make install
if [ $? -ne 0 ]; then
  cd "$ORIGINAL_DIR"
  exit 1
fi

cd "$ORIGINAL_DIR"

