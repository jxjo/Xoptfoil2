@echo off

rem is Xoptfoil2 in the parent directory? 
 
set localPath=..\..\
if not exist %localPath%Worker.exe set localPath=

%localPath%Worker -w norm          -a HD45.dat -i worker.inp
%localPath%Worker -w flap          -a HD45.dat -i worker.inp
%localPath%Worker -w check         -a HD45.dat  
%localPath%Worker -w set t=10      -a HD45.dat  
%localPath%Worker -w set c=3       -a HD45.dat 
%localPath%Worker -w set te=0.1    -a HD45.dat 
%localPath%Worker -w polar-flapped -a HD45.dat -i worker.inp

pause