 
#!/bin/bash

export RDIR=$(pwd)
export LPJROOT=$RDIR/../LPJmL2013
cd $LPJROOT

sed -i "s/.*mono grid cell.*/${1}\/\*mono grid cell\*\//g" lpjml.conf