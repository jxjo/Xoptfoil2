@echo off
set airfoil=F3B_bezier

rem is xoptfoil2 in the parent directory? 
 
set localPath=..\..\
if not exist %localPath%xoptfoil2.exe set localPath=

%localPath%xoptfoil2 -i %airfoil%.xo2

pause
