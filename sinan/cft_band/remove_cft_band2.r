#source("/home/mfader/_R/Preprocessing/new remove_cft_band.r")

woVF<-T # wo veg and fodder, put them in others
viejos<-F#only old crops + potatoes, put new trees in others
bioenergy<-F# put trees and cotton in bioenergy tree and vegis and fodder grass in bioenergy grasss 


if(woVF) {
  index<-c(1:19,21,23:45,47,49:52)
  #outfilename<-"/home/mfader/_R/PlotInputs/in/Mediterranean/inputs_basins/cft1700_2010_new_woVeFo.bin"
}
if(viejos) {
  index<-c(1:13,23,24,27:39,49) 
  outfilename<-"/home/mfader/_R/PlotInputs/in/Mediterranean/inputs_basins/cft1700_2010_new_onlyOld.bin"

}
if(bioenergy) {
  index<-c(1:13,23:39,49:52)  #wo nuevos, put them in others
  outfilename<-"/home/mfader/_R/PlotInputs/in/Mediterranean/inputs_basins/cft1700_2010_new_bioenergy.bin"
}

inputfilename<-"/home/mfader/_R/PlotInputs/in/Mediterranean/inputs_basins/cft1700_2010_new.bin"

inputfilename<-"cft1700_2010_new.bin"
outfilename<-"cft1700_2010_new_nothing.bin"
NCELL<-5794
################
#Remove_cft_band.r:
#-Manipulating cft for debuging
#-To create a cft file which contains only the band that chosen.
#-variable index contains all the chosen cft band number.
#
#1 Temperate Cereals 27
#2 Rice 28
#3 Maize 29
#4 Tropical Cereals 30
#5 Pulses 31
#6 Temperate Roots 32
#7 Potatoes 33 n
#8 Tropical Roots 34
#9 Sunflower 35
#10 Soybeans 36
#11 Groundnuts 37
#12 Rapeseed 38
#13 Sugar Cane 39
#14 Citrus 40n
#15 Non Citrus Orchards 41n
#16 Date Palm 42n
#17 Olives 43n
#18 Nuts Trees 44n
#19 Grapes 45n
#20 Vegetables 46n
#21 Cotton 47n
#22 Fodder grass 48n
#23 Others 49
#24 Manage Grasslands 50
#25 Bioenergy Grass 51
#26 Bioenergy Tree 52


#test results:
#all zero <T>
#olives: <T>
#old c(1:6,): <T>
#old+potatos <T>
#old+olives:F
#old+olives+potatos:X

#new:F
#new without veg fod c(7,)X
#rainfed new: c(7,14:26) <T>
#irrigated only new:c(33,40:48) F
#old+new_rainfed:c(1:32,34:39,49:52) <T> !WARNING013: in cell 1003 at year 1903: sum of cropfraction greater 1: 1.002000
#old+new_rainfed+potatos_irri:c(1:39,49:52)<T>
#old+new_rainfed+potatos_irri+citrus_irri: c(1:40,49:52)F
#old+new_rainfed+potatos(i)+citrus(i)+Orchards(i): c(1:41,49:52)X
#old+new_rainfed+potatos(i)+vegetable(i)+fodder(i):c(1:39,49:52,46,48)F

#only trees(i)+fodder(i)+vegetable(i): c(40:48) <T>
#only exclude potatos(i) F
#all irrigated without potatos(i) c(27:32,34:52) F


#All vegetations: F
###following test put energy tree/grass values as citrus
#1. energy tree (i) : <T>
#2. energy grass(i) : <T> 2 bands=0.004

#3. Old vegetations(r+i), with bioenergy part as citrus:
#c(1:6,8:13,23:26,27:32,34:39,49:52) <T>
#-extra performence information: 32 bands=0.0083s/year

#-excluding the new irrigated classes (potatoes and trees) and putting a value to the bioenergy grass class. 45bands=0.12s/year <T>

#-excluding the new irrigated classes (potatoes and trees) and putting a value to the bioenergy tree class. <T>

#-excluding the new irrigated classes (potatoes and trees) and putting a value to the bioenergy grass AND to the bioenergy tree class. <T>
#c(1:32,34:39,49:52)




read.input.header<-function(filename){
    file.in<-file(filename,"rb")
    
   # seek(file.in,7, origin = "start")
    name<-readChar(file.in,nchar=7)
    version<-readBin(file.in,integer(),n=1,size=4)
    order<-readBin(file.in,integer(),n=1,size=4)
    firstyear<-readBin(file.in,integer(),n=1,size=4)
    nyears<-readBin(file.in,integer(),n=1,size=4)
    firstcell<-readBin(file.in,integer(),n=1,size=4)
    ncells<-readBin(file.in,integer(),n=1,size=4)
    nbands<-readBin(file.in,integer(),n=1,size=4)
    cellsize<-readBin(file.in,numeric(),n=1,size=4)
    scalar<-readBin(file.in,numeric(),n=1,size=4)
    header<-data.frame(name,version,order,firstyear,nyears,firstcell,ncells,nbands,cellsize,scalar)
    close(file.in)
    return(header)
     
}
 
read.input.grid<-function(path.in){
     input.list<-dir(path.in)
     grid.name<-paste(path.in,input.list[grep("grid",input.list)],sep="")
     grid.header<-read.input.header(grid.name)
     
     prec<-abs(log(grid.header$scalar)/log(10))
     gridfile<- file(grid.name,"rb")
     seek(gridfile,HEADER_SIZE, origin = "start")
    grid.temp<<-readBin(gridfile,integer(),n=2*grid.header$ncells,size=2)
    grid.data<<-round(trunc(grid.temp,digits=0)*grid.header$scalar,digits=2)
    lon<<-grid.data[c(1:grid.header$ncells)*2-1]
    lat<<-grid.data[c(1:grid.header$ncells)*2]
    EAST<<-round(max(lon),prec)
    SOUTH<<-round(min(lat),prec)
    WEST<<-round(min(lon),prec)
    NORTH<<-round(max(lat),prec)
    RES<<-grid.header$cellsize
    NC<<-(NORTH-SOUTH)/RES+1
    NR<<-(EAST-WEST)/RES+1

    
    ind_lon<<-ceiling(lon/RES-min(lon)/RES+1)
    ind_lat<<-ceiling(lat/RES-min(lat)/RES+1)
    
    close(gridfile)
}

Lindex<-function(NPIX,NBANDS,year,pix,band) {
    temp<-(year-1)*NPIX*NBANDS+(pix-1)*NBANDS+band
    return(temp)
}

read.input.yearband<-function(filename,data.size,year,band){#year,band, start from 1
fileHeader<-read.input.header(filename)
data.year<-year-fileHeader$firstyear+1
file.in <- file(sprintf(filename),"rb")
data.in<-array(NA,dim=c(fileHeader$ncells))
seek(file.in,where=HEADER_SIZE+data.size*((data.year-1)*fileHeader$nband*fileHeader$ncells+(band-1)),origin="start")
for(i in 1:fileHeader$ncells){
data.in[i]<-readBin(file.in, integer(), n=1, size=data.size)*fileHeader$scalar
seek(file.in,where=(fileHeader$nbands-1)*2,origin="current")
         }
close(file.in)
return(data.in)
}

read.input.files<-function(filename,data.size){
    fileHeader<-read.input.header(filename)
    file.in <- file(sprintf(filename),"rb")
    data.in<-array(NA,dim=c(fileHeader$nbands,fileHeader$nyears,fileHeader$ncells))
    seek(file.in,where=HEADER_SIZE,origin="start")
   for(i in 1:fileHeader$nyears){
        for(j in 1:fileHeader$ncells){
               data.in[,i,j]<-readBin(file.in, integer(), n=fileHeader$nbands, size=data.size)*fileHeader$scalar
        }
     }
    close(file.in)
    return(data.in)
}

map.build<-function(raw_){
map<-array(NA, dim=c(NR,NC))
for(i in 1:length(raw_))
    map[ind_lon[i],ind_lat[i]]<-raw_[i]
    return(map)
}



#only old popo
bp<-rep(0,52)


index<-sort(index)
bp[index]<-1


filein<-file(inputfilename,"rb")
fileout<-file(outfilename,"wb")
seek(filein,43,"start")

writeChar(as.character("LPJLUSE"),fileout,eos=NULL)
writeBin(as.integer(2),fileout,size=4,endian=.Platform$endian) # CLIMATE VERSION
writeBin(as.integer(1),fileout,size=4,endian=.Platform$endian) # ORDER
writeBin(as.integer(1700),fileout,size=4,endian=.Platform$endian) # FIRSTYEAR
writeBin(as.integer(311),fileout,size=4,endian=.Platform$endian) # NYEAR
writeBin(as.integer(0),fileout,size=4,endian=.Platform$endian) # FIRSTCELL
writeBin(as.integer(NCELL),fileout,size=4,endian=.Platform$endian) # NCELL
writeBin(as.integer(52),fileout,size=4,endian=.Platform$endian) # NBAND
writeBin(0.5,fileout,size=4,endian=.Platform$endian) # CELLSIZE
writeBin(0.001,fileout,size=4,endian=.Platform$endian) # SCALAR

control1<-0
control2<-0
control0<-0

for(i in 1: (311*NCELL)){
   data<-readBin(filein,integer(),52,2)

   control1<-control1+sum(data)
   control0<-control0+sum(data*bp)
   #cat(sum(data),sum(data*bp),"\n")
   ##put irrigated citrus in irrigated bioenergy tree and grass.
   #data[51]<-data[40]
   #data[52]<-data[40]
   if(woVF){
    ##put vegis and fodder in others
    data[23]<-data[23]+data[20]+data[22] 
    data[49]<-data[49]+data[46]+data[48]
    control2<-control2+sum(data*bp)
   }
   if(viejos){
   ##put new classes in others
    data[23]<-data[23]+data[14]+data[15]+data[16]+data[17]+data[18]+data[19]+data[20]+data[21]+data[22] 
    data[49]<-data[49]+data[40]+data[41]+data[42]+data[43]+data[44]+data[45]+data[46]+data[47]+data[48]
    control2<-control2+sum(data*bp)
   }
   if(bioenergy){
   ##put new trees and cotton in bioenergy trees, and vegis and fodder in bioenergy grass
    data[26]<-(data[14]+data[15]+data[16]+data[17]+data[18]+data[19]+data[21]) #put trees and cotton in bioen trees
    data[52]<-(data[40]+data[41]+data[42]+data[43]+data[44]+data[45]+data[47]) 
    data[25]<-(data[20]+data[22]) #put vegis and fodder grass in bioener grass
    data[51]<-(data[46]+data[48])
    control2<-control2+sum(data*bp)
   }
   writeBin(as.integer(data*bp), fileout, size=2)
   #cat(control0,control1,control2,"\n")
}
print(control0)
print(control1)
print(control2)


close(filein)
close(fileout)