echo OFF

rem - build beta version number out of date (local dependent!)

set year=%date:~6,4%
set month=%date:~3,2%
set day=%date:~0,2%
set XOPTFOIL_VERSION=beta_%year%.%month%.%day%
echo:
echo Build Xoptfoil2 %XOPTFOIL_VERSION% --------------------------
echo:

rem - os

set TARGET_OS=WIN

rem build and install directory 

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
cd ..
