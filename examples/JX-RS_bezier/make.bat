@echo off
set Airfoil=JX-RS_bez

rem is xoptfoil2 in the parent directory? 
 
set localPath=..\..\
if not exist %localPath%xoptfoil2.exe set LocalPath=

%LocalPath%xoptfoil2 -i %Airfoil%.inp -o %Airfoil%

pause


