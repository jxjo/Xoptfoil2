@echo off
set Airfoil=JX-GP_hicks

xoptfoil2 -i %Airfoil%.inp -o %Airfoil%

pause
