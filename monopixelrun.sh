#!/bin/bash

export LPJROOT=/home/sinan/workspace/R/LPJmL2013
cd $LPJROOT

#Change simulation grid
sed -e "273c $1" $LPJROOT/lpjml.conf > $LPJROOT/tempconf
cp tempconf lpjml.conf
sed -e "274c $1" $LPJROOT/lpjml.conf > $LPJROOT/tempconf
cp tempconf lpjml.conf

#Change spinup year
sed -e "279c $2" $LPJROOT/lpjml.conf > $LPJROOT/tempconf
cp tempconf lpjml.conf
sed -e "291c $2" $LPJROOT/lpjml.conf > $LPJROOT/tempconf
cp tempconf lpjml.conf

rm tempconf

./bin/lpjml  lpjml.conf
./bin/lpjml -DFROM_RESTART lpjml.conf
