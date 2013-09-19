 
#!/bin/bash

export RDIR=$(pwd)
export LPJDIR=$(find ../  -name '--LPJ*')
export LPJROOT=$RDIR/$LPJDIR
cd $LPJROOT

sed -i "s/.*mono grid cell.*/${1}\/\*mono grid cell\*\//g" lpjml.conf
