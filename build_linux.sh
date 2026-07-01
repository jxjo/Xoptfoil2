#!/bin/bash

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
cd "$SCRIPT_DIR"

BUILDDIR=build

if [ -d "$BUILDDIR" ]; then rm -Rf "$BUILDDIR"; fi

mkdir "$BUILDDIR"

cmake -S . -B "$BUILDDIR" \
  -DCMAKE_BUILD_TYPE:STRING=Release
if [ $? -ne 0 ]; then
  exit 1
fi

cmake --build "$BUILDDIR" --target install
if [ $? -ne 0 ]; then
  exit 1
fi
