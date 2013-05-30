#!/bin/bash

export LPJROOT=/home/sinan/workspace/R/LPJmL2013
cd $LPJROOT

#Change simulation grid
sed -e "222c $1" $LPJROOT/lpjml.conf > $LPJROOT/tempconf
cp tempconf lpjml.conf
sed -e "223c $1" $LPJROOT/lpjml.conf > $LPJROOT/tempconf
cp tempconf lpjml.conf

#Change spinup year
sed -e "228c $2" $LPJROOT/lpjml.conf > $LPJROOT/tempconf
cp tempconf lpjml.conf
sed -e "240c $2" $LPJROOT/lpjml.conf > $LPJROOT/tempconf
cp tempconf lpjml.conf

rm tempconf

./bin/lpjml  lpjml.conf
./bin/lpjml -DFROM_RESTART lpjml.conf
