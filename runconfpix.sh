 #1st argument: pixel number
#2nd argument: address of LPJmL 
#!/bin/bash

export LPJROOT=${2}
cd $LPJROOT
echo ${1}
sed -i "s/.*mono grid cell start.*/${1}\/\*mono grid cell start\*\//g" lpjml.conf
sed -i "s/.*mono grid cell end.*/${1}\/\*mono grid cell end\*\//g" lpjml.conf



