rm(list=ls(all=TRUE))
gc()

#source("/home/mfader/_AndereProjekte/Trendy4/LUData/progsnew/_out/landuse_modify.r")
#source("/home/mfader/inputs_longheader/inputs_Trendy/landuse_modify.r")

#source("/home/mfader/_AndereProjekte/Trendy4/LUData/progsnew/_out/read.input.r")
#source("/home/mfader/_AndereProjekte/Trendy4/LUData/progsnew/_out/map.r")
library("fields")

landusefile<-"/home/mfader/_AndereProjekte/Trendy4/LUData/progsnew/_out/cft1700_2015_HYDE_short_corrected_iformat.bin"
#landusefile<-"/home/mfader/inputs_longheader/inputs_Trendy/cft1700_2015_HYDE_short_corrected_iformat.bin"
#landusefile<-"/home/mfader/inputs_longheader/inputs_Trendy/cft1700_2015_HYDE_Crops2Grass_RfIr_short_corrected_iformat.clm"
#landusefile<-"/home/mfader/inputs_longheader/inputs_Trendy/cft1700_2015_HYDE_Crops2Grass_allRf_short_corrected_iformat.clm"

newlanduse<-"/home/mfader/_AndereProjekte/Trendy4/LUData/progsnew/_out/cft1700_2015_HYDE_short_corrected_iformat_1860_dummy.bin"
#newlanduse<-"/home/mfader/inputs_longheader/inputs_Trendy/cft1700_2015_HYDE_short_corrected_iformat_1860_dummy.bin"
#newlanduse<-"/home/mfader/inputs_longheader/inputs_Trendy/cft1700_2015_HYDE_Crops2Grass_RfIr_short_corrected_iformat_1860_dummy.bin"
#newlanduse<-"/home/mfader/inputs_longheader/inputs_Trendy/cft1700_2015_HYDE_Crops2Grass_allRf_short_corrected_iformat_1860_dummy.bin"

read.input.header<-function(filename){
    file.in<-file(filename,"rb")

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

read.input.yearband<-function(filename,year,band, data.size){#year,band, start from 1 
    fileHeader<-read.input.header(filename)
    data.year<-year-fileHeader$firstyear+1
    file.in <- file(sprintf(filename),"rb")
    data.in<-array(NA,dim=c(fileHeader$ncells))
    seek(file.in,where=43+data.size*((data.year-1)*fileHeader$nband*fileHeader$ncells+(band-1)),origin="start")
    for(i in 1:fileHeader$ncells){
        data.in[i]<-readBin(file.in, integer(), n=1, size=data.size)
        seek(file.in,where=(fileHeader$nbands-1)*2,origin="current")
    }
    close(file.in)
    return(data.in)
}


header<-read.input.header(landusefile)

firstyear<-header$firstyear
nyear<-header$nyears
npix<-header$ncells
nbands<-header$nbands
cellsize<-header$cellsize
scalar<-header$scalar

print(paste("version",header$version,"order",header$order,"firstyear",header$firstyear,"nyears",header$nyears,"firstcell",
header$firstcell,"ncells",header$ncells,"nbands",header$nbands,"cellsize",header$cellsize,"scalar",header$scalar,sep=" "))



d1860_array<-array(NA,c(nbands,npix))
for(i in 1:nbands){
    d1860_array[i,]<-read.input.yearband(landusefile,data.size=2,year=1860,band=i)
    cat("\b\b\b\b\b\b\b\b\b\b\b",round(i/nbands*100),"%")
}
cat("\n")

d1860<-as.integer(as.vector(d1860_array))

#--------
#writing LPJ output
#--------
outname<-paste(newlanduse,sep="")
cat("writing LPJ input",outname,"...")

output<-file(outname, "w+b")
writeChar("LPJLUSE",output,eos=NULL)               #header name
writeBin(as.integer(2),output,size=4)              #header version
writeBin(as.integer(1),output,size=4)              #order
writeBin(as.integer(firstyear),output,size=4)      #firstyear
writeBin(as.integer(nyear),output,size=4)          #nyear
writeBin(as.integer(0),output,size=4)              #firstcell
writeBin(as.integer(npix),output,size=4)           #ncell
writeBin(as.integer(nbands),output,size=4)             #nbands
writeBin(as.numeric(cellsize),output,size=4) #cellsize
writeBin(as.numeric(scalar),output,size=4) #scalar
for(i in 1:nyear){
    writeBin(d1860,output,size=2)
}
close(output)
cat("[done]\n")

for(i in 1:32){

may_old<-read.input.yearband(landusefile,data.size=2,year=1860,band=i)

may_new<-read.input.yearband(newlanduse,data.size=2,year=1700,band=i)
if(!all((may_old-may_new)==0))
   cat("warning: [1]")


may_new<-read.input.yearband(newlanduse,data.size=2,year=2015,band=i)
if(!all((may_old-may_new)==0))
   cat("warning: [2]")


may_new<-read.input.yearband(newlanduse,data.size=2,year=1750,band=i)
if(!all((may_old-may_new)==0))
   cat("warning: [3]")

may_new<-read.input.yearband(newlanduse,data.size=2,year=1900,band=i)
if(!all((may_old-may_new)==0))
   cat("warning: [4]")

may_new<-read.input.yearband(newlanduse,data.size=2,year=2000,band=i)
if(!all((may_old-may_new)==0))
   cat("warning: [5]")
   
}

