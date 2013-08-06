#!/bin/bash

export RDIR=$(pwd)
export LPJROOT=$RDIR/../LPJmL2013
cd $LPJROOT

./bin/lpjml  lpjml.conf
./bin/lpjml -DFROM_RESTART lpjml.conf
