@echo off
echo .
echo "Test all worker functions"
echo .

del HD45_*.*
del HD45-*.*
del M1779B_*.*
del M1779B-*.*

worker -w norm   -a HD45.dat -i inorm.inp

worker -w flap   -a HD45.dat -i iflap.txt 

worker -w check  -a HD45.dat  
worker -w check  -a M1779B.dat  

worker -w set  t=10  -a HD45.dat  
worker -w set xt=20  -a HD45.dat  
worker -w set  c=2   -a HD45.dat  
worker -w set xc=40  -a HD45.dat 

worker -w set xc=60  -a JX-Seed-Rear-Thick.dat  

worker -w set c=3 -a M1779B.dat  

worker -w blend  60 -a HD45.dat  -a2 M1779B.dat

worker -w polar  -a HD45.dat -i iPolars_T2.txt -r 150000 
worker -w polar  -a HD45.dat -i iPolars_T1_set.txt

pause 
