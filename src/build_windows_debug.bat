ECHO OFF
SET XOPTFOIL_VERSION=2.0_dev


pushd ..
SET INSTALLDIR=%CD%\windows
IF NOT EXIST build        MKDIR build
IF NOT EXIST %INSTALLDIR% MKDIR %INSTALLDIR%

CD build
cmake -G "MinGW Makefiles" ^
  -DCMAKE_INSTALL_PREFIX:PATH=%INSTALLDIR% ^
  -DCMAKE_BUILD_TYPE:STRING="Debug" ^
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