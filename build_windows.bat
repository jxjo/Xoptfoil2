@echo OFF
setlocal

pushd "%~dp0"
if not exist build mkdir build

cmake -S . -B build -G "MinGW Makefiles"
if %ERRORLEVEL% neq 0 goto :error

cmake --build build --target install -- --no-print-directory
if %ERRORLEVEL% neq 0 goto :error

popd
endlocal
goto :end

:error
popd
endlocal
exit /b %ERRORLEVEL%

:end
