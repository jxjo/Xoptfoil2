echo OFF

set XOPTFOIL_VERSION=2.0_dev

rem build and install directory 

pushd ..
set INSTALLDIR=%CD%\windows
if not exist build        mkdir build
if not exist %INSTALLDIR% mkdir %INSTALLDIR%

rem run cmake in build dir 

cd build
cmake -G "MinGW Makefiles" ^
  -DCMAKE_INSTALL_PREFIX:PATH=%INSTALLDIR% ^
  -DCMAKE_BUILD_TYPE:STRING="Release" ^
  ..
if %ERRORLEVEL% neq 0 goto :error

mingw32-make VERBOSE=1
if %ERRORLEVEL% neq 0 goto :error

mingw32-make install
if %ERRORLEVEL% neq 0 goto :error

popd
goto :end

:error
popd
exit /b %ERRORLEVEL%

:end
