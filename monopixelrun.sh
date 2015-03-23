#!/bin/bash

export RDIR=$(pwd)
export LPJDIR=$(find ../  -name '--LPJ*')
export LPJROOT=$RDIR/$LPJDIR
cd $LPJROOT

#Change simulation grid
sed -i "s/.*mono grid cell start.*/${1}\/\*mono grid cell start\*\//g" lpjml.conf
sed -i "s/.*mono grid cell end.*/${1}\/\*mono grid cell end\*\//g" lpjml.conf


./bin/lpjml  lpjml.conf
./bin/lpjml -DFROM_RESTART lpjml.conf
