d1=(irrig1 irrig1 irrig1 irrig1 DRIP DRIP DRIP DRIP IMP IMP IMP IMP)
d2=(const dyn red sat const dyn red sat const dyn red sat)
files=( grid.bin cftfrac.bin pft_harvest.pft.bin cft_nir.pft.bin pft_irrig.pft.bin cft_npp.pft.bin cft_consump_water_b.pft.bin cft_consump_water_g.pft.bin gcgp.pft.bin input* screen*)

files=(cft_consump_water_b.pft.bin cft_consump_water_g.pft.bin cftfrac.bin cft_nir.pft.bin  cft_npp.pft.bin gcgp.pft.bin pft_harvest.pft.bin pft_irrig.pft.bin)
size=(107100 107100  107100  107100  125636  125636  107100  107100)

OUT="/var/run/media/sinan/Elements/GCM_OUTPUT_NEW"
hermes="cluster-mib/../mfader/outputs/OUTPUT_GCMs_NEW"

for (( i=0; i<${#d1[@]}; i++))
do
	localdir=$OUT/${d1[i]}/${d2[i]}
	remotedir=$hermes/${d1[i]}/${d2[i]}
	GCM=($(ssh sshi@hermes.oamp.fr ls $remotedir))
	for((j=0; j<${#GCM[@]};j++))
	do
		if [ ! -d $localdir/${GCM[j]} ]; then
			mkdir $localdir/${GCM[j]}
		fi
		echo ${GCM[j]}
		echo "=========="
		for((k=0; k<${#files[@]};k++))
		do
			if [ ! -f $localdir/${GCM[j]}/${files[k]} ]; then
				scp sshi@hermes.oamp.fr:$remotedir/${GCM[j]}/${files[k]} $localdir/${GCM[j]}
			else
				size_=$(du $localdir/${GCM[j]}/${files[k]}|cut -f1)
				if [ $size_ != ${size[k]} ]; then
					echo ${files[k]} $size $size_
					scp sshi@hermes.oamp.fr:$remotedir/${GCM[j]}/${files[k]} $localdir/${GCM[j]}
					
				fi
			fi
		done
	done
done
