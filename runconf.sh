#Run LPJmL 
#1st argument: with 3 digits, 1st digits indicate create first restart file, 
#                                                      2nd digits for creating second restart file with land
#                                                      3rd  digits to run from restart file. 
#                                                      e.g. only run 1st: 100
#                                                              run 1 and 2:   110
#2nd argument: address of LPJmL 
#3rd argument: address to write LPJmL screen output

#!/bin/bash


export LPJROOT=${2}
echo $LPJROOT
cd $LPJROOT


#detect the grid number of LPJmL file
gridnum=`grep "mono\ grid\ cell\ start" lpjml.conf|grep -o [0-9]*`


#setting for the first run
export spinup1=50
export cycle1=30
export startyear1=1901
export endyear1=1901
export restartend1=1900

#setting for the second run
export spinup2=390
export cycle2=30
export startyear2=1901
export endyear2=2009
export restartend2=1950


#setting for the second run
export spinup3=0
export cycle3=0
export startyear3=1951
export endyear3=2009
export restartend3=2000

#---------------------
#1st Run: without Landuse
#--------------------
if [ "${1:0:1}" == 1 ]; then
  echo "1st run..."
  
  withland=`grep -c //#define\ WITH_LANDUSE  lpjml.conf`
  if [ "$withland" == 0 ]; then #remove landuse
       sed -i "s/#define WITH_LANDUSE 1/\/\/#define WITH_LANDUSE 1/g" lpjml.conf
  fi

  sed -i 's/.*spinup1.*/'$spinup1'\/\*spinup1\*\//g' lpjml.conf
  sed -i 's/.*cycle1.*/'$cycle1'\/\*cycle1\*\//g' lpjml.conf
  sed -i 's/.*startyear1.*/'$startyear1'\/\*startyear1\*\//g' lpjml.conf
  sed -i 's/.*endyear1.*/'$endyear1'\/\*endyear1\*\//g' lpjml.conf
  #make the name of restart file
  sed -i 's/.*restartfile1.*/restart\/s5000nv_p'$gridnum'_'$restartend1'.lpj\/\*restartfile1\*\//g' lpjml.conf 
  sed -i 's/.*restartend1.*/'$restartend1'\/\*restartend1\*\//g' lpjml.conf
  read -p "press enter to start"
  ./bin/lpjml  lpjml.conf >${3}.run_1.out
 cp lpjml.conf ${3}.run_1.conf
fi



#----------------------
#2nd Run: Create Landuse restart file
#         with Landuse
#         From_Restart
#       
#----------------------
if [ "${1:1:1}" == 1 ]; then
  echo "2nd run..."
  withland=`grep -c //#define\ WITH_LANDUSE lpjml.conf`
  if [ "$withland" == 1 ]; then #add landuse
      sed -i "s/\/\/#define WITH_LANDUSE 1/#define WITH_LANDUSE 1/g" lpjml.conf
  fi

  sed -i 's/.*spinup2.*/'$spinup2'\/\*spinup2\*\//g' lpjml.conf
  sed -i 's/.*cycle2.*/'$cycle2'\/\*cycle2\*\//g' lpjml.conf
  sed -i 's/.*startyear2.*/'$startyear2'\/\*startyear2\*\//g' lpjml.conf
  sed -i 's/.*endyear2.*/'$endyear2'\/\*endyear2\*\//g' lpjml.conf
  sed -i 's/.*restartfile2.*/restart\/s5000nv_p'$gridnum'_'$restartend1'.lpj\/\*restartfile2\*\//g' lpjml.conf
  sed -i 's/.*restartfile3.*/restart\/s5000LU_p'$gridnum'_'$restartend2'.lpj\/\*restartfile3\*\//g' lpjml.conf
  sed -i 's/.*restartend2.*/'$restartend2'\/\*restartend2\*\//g' lpjml.conf
  read -p "press enter to start"
  ./bin/lpjml -DFROM_RESTART lpjml.conf >${3}.run_2.out
  cp lpjml.conf ${3}.run_2.conf
fi
#----------------------
#3rd Run: Create Landuse restart file
#         with Landuse
#         From_Restart
#       
#----------------------

if [ "${1:2:2}" == 1 ]; then
  echo "3rd run..."
  withland=`grep -c //#define\ WITH_LANDUSE lpjml.conf`
  if [ "$withland" == 1 ]; then
     sed -i "s/\/\/#define WITH_LANDUSE 1/#define WITH_LANDUSE 1/g" lpjml.conf
  fi

  sed -i 's/.*spinup2.*/'$spinup3'\/\*spinup2\*\//g' lpjml.conf
  sed -i 's/.*cycle2.*/\/\/'$cycle3'\/\*cycle2\*\//g' lpjml.conf
  sed -i 's/.*startyear2.*/'$startyear3'\/\*startyear2\*\//g' lpjml.conf
  sed -i 's/.*endyear2.*/'$endyear3'\/\*endyear2\*\//g' lpjml.conf
  sed -i 's/.*restartfile2.*/restart\/s5000LU_p'$gridnum'_'$restartend2'.lpj\/\*restartfile2\*\//g' lpjml.conf
  sed -i 's/.*restartfile3.*/restart\/s5000LU_p'$gridnum'_'$restartend3'.lpj\/\*restartfile3\*\//g' lpjml.conf
  sed -i 's/.*restartend2.*/'$restartend3'\/\*restartend2\*\//g' lpjml.conf
  read -p "press enter to start"
  ./bin/lpjml -DFROM_RESTART lpjml.conf >${3}.run_3.out
  cp lpjml.conf ${3}.run_3.conf
fi
