@echo off
echo .
echo "Test all worker functions"
echo .


worker -w norm   -a hd45.dat -i inorm.inp

worker -w smooth -a hd45.dat -i iSmooth.txt

worker -w flap   -a hd45.dat -i iflap.txt 

worker -w check  -a hd45.dat  

worker -w set t=10 -a hd45.dat  

worker -w set c=3 -a M1779B.dat  

worker -w blend  60 -a hd45.dat  -a2 M1779B.dat

worker -w polar  -a hd45.dat -i iPolars_T2.txt -r 150000 
worker -w polar  -a hd45.dat -i iPolars_T1_set.txt

pause 
