#!/bin/bash 

download_list=(grid.bin cftfrac.bin pft_harvest.pft.bin cft_nir.pft.bin pft_irrig.pft.bin cft_npp.pft.bin cft_consump_water_b.pft.bin cft_consump_water_g.pft.bin gcgp.pft.bin)
# irrigsc_=(irrig1 irrig1 irrig1 irrig1 IMP IMP DRIP DRIP DRIP DRIP IMP)
# co2sc_=(const dyn red sat const red dyn red sat const sat)


size=(23176 109668832 109668832 109668832 109668832 128649976 109668832 109668832 128649976 1831 14121)
# irrigsc_=(irrig1 irrig1 irrig1 irrig1 IMP IMP IMP DRIP DRIP DRIP DRIP IMP)
# co2sc_=(const dyn red sat const dyn red dyn red sat const sat)
irrigsc_=(IMP)
co2sc_=(sat)


run=${#irrigsc_[@]}
for (( r=0; r<$run; r++ ))
do
	irrigsc=${irrigsc_[r]}
	co2sc=${co2sc_[r]}
	online_path="cluster-mib/../mfader/outputs/OUTPUT_GCMs_NEW/$irrigsc/$co2sc"
	#local_path="/var/run/media/sinan/Elements/GCM_OUTPUT"
	local_path="/var/run/media/sinan/TOURO/workspace/GCM"
	GCM=($(ssh sshi@hermes.oamp.fr "ls $online_path"))
	num_run=${#GCM[@]}

	
# 	while [ $num_run -ne 133 ]
# 	do
# 		num_run=${#GCM[@]}
# 		echo $num_run
# 		sleep 1
# 	done

	
	if [ ! -d $local_path/irrigsc ];then
		mkdir $local_path/$irrigsc
	fi
	if [ ! -d $local_path/irrigsc/$co2sc ];then
		mkdir $local_path/$irrigsc/$co2sc
	fi
	for i in ${GCM[*]}
	do
		localdir=$local_path/$irrigsc/$co2sc/$i
		if [ ! -d $localdir ]; then
			mkdir $localdir
		fi
# 		echo "===="
# 		echo "$i"
# 		echo "===="
		for (( j=0; j<${#download_list[@]}; j++))
		do
			fname=$localdir/${download_list[j]}
# 			if [ ! -f $fname ]; then
# 			echo $fname do not exsit
			#scp sshi@hermes.oamp.fr:$online_path/$i/${download_list[j]} $localdir
			
			if [ -f $fname ]; then
				a=$(du -b $localdir/${download_list[j]}|awk '{print $1}')
				if [ $a -ne ${size[j]} -a $a -gt 15000 ]; then
					echo $localdir/${download_list[j]}
					echo $a ${size[j]}
					scp sshi@hermes.oamp.fr:$online_path/$i/${download_list[j]} $localdir
				fi
			else
				echo "$localdir/${download_list[j]} not found"
				scp sshi@hermes.oamp.fr:$online_path/$i/${download_list[j]} $localdir
			fi
#  			echo $online_path/$i/${download_list[j]}
		done
	done
done



