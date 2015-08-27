source("LPJU/read.input.r")
source("LPJU/map.r")
library("fields")
TEST<-TRUE
files<-paste("inputs/",dir("inputs"),sep="")
outfile<-paste("output/",dir("input_r"),sep="")

npix<-67420
nyear<-114+40
firstyear<-1861
nbands<-12
cellsize<-0.5
scalar<-0.1



for(i in 1:length(files)){
    data<-read.input.files2(files[i],2)

    data_period<-round(data[,,1:20]/scalar)
    data_period_vec<-as.integer(as.vector(data_period))
    data_period_vec<-c(data_period_vec,data_period_vec)

    data_real_vec<-as.integer(as.vector(round(data/scalar)))


    #--------
    #writing LPJ output
    #--------
    outname<-paste(outfile[i],"_recycle20y.clm",sep="")
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

    writeBin(data_period_vec,output,size=2)
    writeBin(data_real_vec,output,size=2)
    close(output)
    cat("[done]\n")



    if(TEST==TRUE){
        data_1861<-read.input.yearband(outname[i],data.size=2,band=1,year=1861)
        data_1901<-read.input.yearband(outname[i],data.size=2,band=1,year=1901)
        if(!all((data_1860-data_1901)==0))
            stop("testing failed: 1861 != 1901")

        data_1881<-read.input.yearband(outname[i],data.size=2,band=1,year=1881)
        if(!all((data_1881-data_1901)==0))
            stop("testing failed: 1881 != 1901 band 1")

        data_1881<-read.input.yearband(outname[i],data.size=2,band=5,year=1881)
        data_1901<-read.input.yearband(outname[i],data.size=2,band=5,year=1901)
        if(!all((data_1881-data_1901)==0))
            stop("testing failed: 1881 != 1901 band 5")

        data_2014<-read.input.yearband(outname[i],data.size=2,band=5,year=2014)
        if(!all((data_2014-data[5,,114])<1e-5))
            stop("testing failed: 1881 != 1901")

    }
}



