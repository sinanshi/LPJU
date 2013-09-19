#!/bin/bash

export RDIR=$(pwd)
export LPJDIR=$(find ../  -name '--LPJ*')
export LPJROOT=$RDIR/$LPJDIR
cd $LPJROOT

./bin/lpjml  lpjml.conf
./bin/lpjml -DFROM_RESTART lpjml.conf
