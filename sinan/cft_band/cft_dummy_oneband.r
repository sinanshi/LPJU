# -One including rainfed and irrigated olives, each 0.3
# -One including annual crops only: bands 1-13 and 27-39. Each 0.03
# -One including trees only: bands 14-19,21,40-45,47. Each 0.03
# -And 26 files, containing each file, only two bands of each crop. Each frac 0.3. (so the same that ou did for olives, but for the rest of the crops).



rm(list=ls(all=TRUE))
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
scalar<-as.numeric(0.1)

read.input.grid("/home/sinan/workspace/LPJmL/LPJmL_Global/inputs2013/grid.bin")


# -One including rainfed and irrigated olives, each 0.3
input_bands<-rep(0,52)
input_bands[17]<-3
input_bands[43]<-3
input_pix_bands<-rep(input_bands,67420)
zz <- file("cft_dummy_oneband.bin", "wb")
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
for(i in 1:311)
writeBin(as.integer(input_pix_bands),zz,size=2)
close(zz)

#read.input.yearband("cft_dummy_oneband.bin",2,1901,43)
	

