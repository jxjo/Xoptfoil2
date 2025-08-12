echo OFF

set XOPTFOIL_VERSION=1.0.9

rem build and install directory 

cd ..
set INSTALLDIR=%CD%\windows
if not exist build        mkdir build
if not exist %INSTALLDIR% mkdir %INSTALLDIR%

rem run cmake in build dir 

cd build
cmake -G "MinGW Makefiles" ^
  -DCMAKE_INSTALL_PREFIX:PATH=%INSTALLDIR% ^
  -DCMAKE_BUILD_TYPE:STRING="Release" ^
  ..

mingw32-make VERBOSE=1
mingw32-make install
cd ..\src
