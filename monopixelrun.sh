
#!/bin/bash

sed -e "281c $1" /home/sinan/Desktop/R/LPJmL_OTMed/lpjml.OTMed_LAM.conf > /home/sinan/Desktop/R/LPJmL_OTMed/temp 
sed -e "282c $1" /home/sinan/Desktop/R/LPJmL_OTMed/temp > /home/sinan/Desktop/R/LPJmL_OTMed/lpjml.OTMed_LAM.conf
rm /home/sinan/Desktop/R/LPJmL_OTMed/temp


echo $1
export LPJROOT=/home/sinan/Desktop/R/LPJmL_OTMed/
#./../LPJmL_OTMed/bin/lpjml  ./../LPJmL_OTMed/lpjml.OTMed_LAM.conf
./../LPJmL_OTMed/bin/lpjml -DFROM_RESTART ./../LPJmL_OTMed/lpjml.OTMed_LAM.conf
