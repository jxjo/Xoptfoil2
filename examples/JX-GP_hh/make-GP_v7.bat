@echo off
set Airfoil=JX-GP_v7

xoptfoil2 -i %Airfoil%.inp -o %Airfoil%

pause
