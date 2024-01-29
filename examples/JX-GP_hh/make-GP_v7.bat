@echo off
set Airfoil=JX-GP_v7

xoptfoil-jx -i %Airfoil%.inp -o %Airfoil%

pause
