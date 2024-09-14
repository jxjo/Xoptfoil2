#!/bin/bash

BUILDDIR=build
INSTALLDIR=$(pwd)/linux
# CURRENT_DATE=`date +"%Y.%m.%d"`
# XOPTFOIL_VERSION=beta_${CURRENT_DATE} 
XOPTFOIL_VERSION=1.0.2 
 
export XOPTFOIL_VERSION

cd ..
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

