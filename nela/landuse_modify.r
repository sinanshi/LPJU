source("LPJU/read.input.r")
source("LPJU/map.r")
library("fields")

landusefile<-"landuse/cft1700_2015_HYDE_short_corrected_iformat.bin"
newlanduse<-"output/cft1700_2015_HYDE_short_corrected_iformat_1860_dummy.bin"
header<-read.input.header(landusefile)

firstyear<-header$firstyear
nyear<-header$nyears
npix<-header$ncells
nbands<-header$nbands
cellsize<-header$cellsize
scalar<-header$scalar

d1860<-array(NA,c(nbands,npix))
for(i in 1:nbands){
    d1860[i,]<-read.input.yearband(landusefile,data.size=2,year=1860,band=i)
    cat("\b\b\b\b\b\b\b\b\b\b\b",round(i/nbands*100),"%")
}
cat("\n")

d1860<-as.integer(as.vector(d1860/scalar))

#--------
#writing LPJ output
#--------
outname<-paste(newlanduse,sep="")
cat("writting LPJ input",outname,"...")

output<-file(outname, "wb")
writeChar("LPJCLIM",output,eos=NULL)            #header name
writeBin(as.integer(1),output,size=4)              #header version
writeBin(as.integer(1),output,size=4)              #order
writeBin(as.integer(firstyear),output,size=4)           #firstyear
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


may_old<-read.input.yearband(landusefile,data.size=2,year=1860,band=5)
may_new<-read.input.yearband(newlanduse,data.size=2,year=1700,band=5)
if(!all((may_old-may_new)==0))
   cat("warning: [1]")

may_new<-read.input.yearband(newlanduse,data.size=2,year=2015,band=32)
may_old<-read.input.yearband(landusefile,data.size=2,year=1860,band=32)
if(!all((may_old-may_new)==0))
   cat("warning: [2]")




