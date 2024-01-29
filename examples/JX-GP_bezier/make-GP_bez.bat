@echo off
set Airfoil=JX-GP_bez

xoptfoil2 -i %Airfoil%.inp -o %Airfoil%

pause
