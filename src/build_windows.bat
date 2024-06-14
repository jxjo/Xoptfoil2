echo OFF

rem - build beta version number out of date (local dependent!)
rem set year=%date:~6,4%
rem set month=%date:~3,2%
rem set day=%date:~0,2%
rem set XOPTFOIL_VERSION=beta_%year%.%month%.%day%

set XOPTFOIL_VERSION=1.0.3 beta

echo:
echo Build Xoptfoil2 %XOPTFOIL_VERSION% --------------------------
echo:

rem - os

set TARGET_OS=WIN

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
