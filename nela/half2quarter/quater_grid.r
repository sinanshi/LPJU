#Nasty implementation of upscaling the half degree grid to a quarter degree
#From half degree to quarter degree
#e.g.  (1,1)-> (0.75,0.75);(0.75,1.25);(1.25,0.75);(1.25,1.25)
#1) get diagonal values: i.e.  (1,1)-> (0.75,0.75);(1.25,1.25)
#2) compensate another diagonal
#Notice: the output was written in integer, which is 4 byte, while the LPJmL requires 2 bytes short binary,
#for the reason, one has to use C code to make it. 
# Sinan Shi 18/12/2012


rm(list = ls())

source("header")

header<-read.input.header("../../NelasInputs/grid.bin")
read.input.grid("../../NelasInputs/grid.bin")
pix<-length(lon)

lon<-round(lon,2)*1000
lat<-round(lat,2)*1000
lon_diag<-array(NA, dim=2*pix)
lat_diag<-array(NA, dim=2*pix)

#one more grid in diagonal 
for(i in 1:pix){
    lon_diag[2*i-1]<-lon[i]-125
    lon_diag[2*i]<-lon[i]+125
}

for(i in 1:pix){
    lat_diag[2*i-1]<-lat[i]-125
    lat_diag[2*i]<-lat[i]+125
}
newpix<-4*pix
lon_new<-array(NA,dim=newpix)
lat_new<-array(NA, dim=newpix)

ind_lon<-array(NA,dim=newpix)
ind_lat<-array(NA, dim=newpix)
#1122334455667788
ind_lon[seq(1,newpix,2)]<-seq(1,newpix/2)
ind_lon[seq(2,newpix+1,2)]<-seq(1,newpix/2)

ind_lat[seq(1,newpix,4)]<-seq(1,(newpix/2),2)
ind_lat[seq(2,newpix,4)]<-seq(2,(newpix/2),2)
ind_lat[seq(3,newpix,4)]<-seq(2,(newpix/2),2)
ind_lat[seq(4,newpix,4)]<-seq(1,(newpix/2),2)
#1221344356657887

lon_new<-lon_diag[ind_lon]
lat_new<-lat_diag[ind_lat]
# plot(lon_new,lat_new,xlim=c(-20000,10000),ylim=c(30000,60000))

lon_new<-as.integer(lon_new)
lat_new<-as.integer(lat_new)

lonlat<-array(NA,dim=2*newpix)
lonlat[seq(1,newpix*2,2)]<-lon_new[]
lonlat[seq(2,newpix*2,2)]<-lat_new[]
lonlat<-as.vector(lonlat)
lonlat<-floor(lonlat/10)
#lonlat<-c(lon_new,lat_new)
zz <- file("/home/sinan/workspace/LPJ_Utilities/src/nela/half2quarter/out/grid_quater.bin", "wb")
title<-"LPJGRID"

ncell<-as.integer(newpix)
cellsize<-0.25
scalar<-as.numeric(0.01)
writeChar(title,zz,eos=NULL) 
writeBin(header$version,zz,size=4)
writeBin(header$order,zz,size=4)
writeBin(header$firstyear,zz,size=4)
writeBin(header$nyear,zz,size=4)
writeBin(header$firstcell,zz,size=4)
writeBin(ncell,zz,size=4)
writeBin(header$nbands,zz,size=4)
writeBin(cellsize,zz,size=4)
writeBin(scalar,zz,size=4)
seek(zz,43,origin="start")
writeBin(as.integer(lonlat),zz,size=2)
close(zz)


