@echo off
del *-norm.dat
del *bezier.dat

dir *.dat /B /ON > temp.txt
for /f "delims=§" %%f in (temp.txt) do Worker -i bezier.inp -a "%%f" -w bezier 
del temp.txt

pause
