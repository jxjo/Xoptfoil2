@echo off
REM Generate .mod files for Fortran linter

echo Creating directories...
if not exist "%~dp0mods_from_linter" mkdir "%~dp0mods_from_linter"
if not exist "%~dp0linter_build" mkdir "%~dp0linter_build"

echo Configuring CMake...
cd "%~dp0linter_build"
E:\Fortran\bin\cmake.exe -G "MinGW Makefiles" -DCMAKE_Fortran_MODULE_DIRECTORY="%~dp0mods_from_linter" "%~dp0.."
if %errorlevel% neq 0 (
    echo CMake configuration failed!
    exit /b %errorlevel%
)

echo Building modules...
E:\Fortran\bin\cmake.exe --build . --target MYOBJLIB
if %errorlevel% neq 0 (
    echo Build failed!
	pause
    exit /b %errorlevel%
)

echo.
echo Success! Module files generated in .vscode\mods_from_linter
echo.
dir "%~dp0mods_from_linter\*.mod" /b
