
localWorker="../../linux/bin/Worker"

$localWorker -w norm        -a HD45.dat -i worker.inp
$localWorker -w flap        -a HD45.dat -i worker.inp
$localWorker -w check       -a HD45.dat  
$localWorker -w set t=10    -a HD45.dat  
$localWorker -w set c=3     -a HD45.dat 
$localWorker -w set te=0.1  -a HD45.dat 
$localWorker -w polar       -a HD45.dat -i worker.inp