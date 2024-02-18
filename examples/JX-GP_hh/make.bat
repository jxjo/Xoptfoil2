@echo off
set Airfoil=JX-GP_hicks

if exist ..\..\xoptfoil2.exe (
    ..\..\xoptfoil2  -i %Airfoil%.inp  -o %Airfoil% 
) else (
    xoptfoil2  -i %Airfoil%.inp  -o %Airfoil% 
)

pause
