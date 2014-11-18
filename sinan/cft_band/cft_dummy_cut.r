source("~/workspace/LPJ_Utilities/src/map.r")
source("~/workspace/LPJ_Utilities/src/read.input.r")
npix<-67420
title<-as.character("LPJLUSE")
version<-as.integer(2)
order<-as.integer(1)
firstyear<-as.integer(1700)
nyear<-as.integer(311)
firstcell<-as.integer(0)
ncell<-as.integer(67420)
nbands<-as.integer(52)
cellsize<-as.numeric(0.5)
scalar<-as.numeric(0.001)



read.input.grid("/home/sinan/workspace/LPJmL/LPJmL_Global/inputs2013/grid.bin")

input_1<-rep(0,67420)
input_1[which(lon> -30 & lon<80)]<-1
input_band<-array(0,npix*52)
for(i in 1:npix){
	if(input_1[i]==1){
		input_band[(52*(i-1)+1):(52*i)]<-1
		}
	}
input_band<-as.vector(as.integer(input_band))
zz <- file("cft.bin", "wb")
writeChar(title,zz,eos=NULL)
writeBin(version,zz,size=4)
writeBin(order,zz,size=4)
writeBin(firstyear,zz,size=4)
writeBin(nyear,zz,size=4)
writeBin(firstcell,zz,size=4)
writeBin(ncell,zz,size=4)
writeBin(nbands,zz,size=4)
writeBin(cellsize,zz,size=4)
writeBin(scalar,zz,size=4)
for(i in 1:311){
	writeBin(input_band,zz,size=2)
	}
close(zz)

image(map.build(read.input.yearband("cft.bin",2,2010,52)),col=rainbow(2))

