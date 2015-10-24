rm(list=ls(all=TRUE))
gc()

#source("/home/mfader/_AndereProjekte/Trendy4/ClimateData/period_clim_dummy.r")

#source("LPJU/read.input.r")
#source("LPJU/map.r")
library("fields")
#require(lpjutil)

TEST<-TRUE
files<-paste("/home/mfader/_AndereProjekte/Trendy4/ClimateData/inputs/",dir("/home/mfader/_AndereProjekte/Trendy4/ClimateData/inputs"),sep="")
outfile<-paste("/home/mfader/_AndereProjekte/Trendy4/ClimateData/outputs/",dir("/home/mfader/_AndereProjekte/Trendy4/ClimateData/inputs"),sep="")

nyear<-114+40
firstyear<-1861


read.input.header<-function(filename){
    file.in<-file(filename,"rb")
    
   # seek(file.in,7, origin = "start")
    name<-readChar(file.in,nchar=7)
    version<-readBin(file.in,integer(),n=1,size=4)
    order<-readBin(file.in,integer(),n=1,size=4)
    firstyear<-readBin(file.in,integer(),n=1,size=4)
    nyears<-readBin(file.in,integer(),n=1,size=4)
    firstcell<-readBin(file.in,integer(),n=1,size=4)   
    ncells<<-readBin(file.in,integer(),n=1,size=4)
    nbands<<-readBin(file.in,integer(),n=1,size=4)
    cellsize<<-readBin(file.in,numeric(),n=1,size=4)
    scalar<<-readBin(file.in,numeric(),n=1,size=4)
    header<-data.frame(name,version,order,firstyear,nyears,firstcell,ncells,nbands,cellsize,scalar)
    close(file.in)
    return(header)
     
}

read.input.files2<-function(filename,data.size){
    cat("reading LPJ input:",filename,"\n")
    fileHeader<-read.input.header(filename)
    file.in <- file(sprintf(filename),"rb")
    data.in<-array(NA,dim=c(fileHeader$nbands,fileHeader$ncells,fileHeader$nyears))
    seek(file.in,where=43,origin="start")
    for(i in 1:fileHeader$nyears){
        for(j in 1:fileHeader$ncells){
               data.in[,j,i]<-readBin(file.in, integer(), n=fileHeader$nbands, size=data.size)
        }
        cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b",round(i/fileHeader$nyears*100),"%")
     }
    cat("...[done]\n")
    close(file.in)
    return(data.in)
}

read.input.yearband<-function(filename,data.size,band,year){#year,band, start from 1 
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

#Functions done
#---------------------------------

for(i in 1:length(files)){
    data<-read.input.files2(files[i],2)

    data_period<-data[,,1:20]
    data_period_vec<-as.integer(as.vector(data_period))
    data_period_vec<-c(data_period_vec,data_period_vec)

    data_real_vec<-as.integer(as.vector(data))


    #--------
    #writing LPJ output
    #--------
    outname<-paste(outfile[i],"_recycle20y.clm",sep="")
    cat("writing LPJ input",outname,"...")

    output<-file(outname, "wb")
    writeChar("LPJCLIM",output,eos=NULL)               #header name
    writeBin(as.integer(2),output,size=4)              #header version
    writeBin(as.integer(1),output,size=4)              #order
    writeBin(as.integer(firstyear),output,size=4)      #firstyear
    writeBin(as.integer(nyear),output,size=4)          #nyear
    writeBin(as.integer(0),output,size=4)              #firstcell
    writeBin(as.integer(ncells),output,size=4)           #ncell
    writeBin(as.integer(nbands),output,size=4)         #nbands
    writeBin(as.numeric(cellsize),output,size=4) #cellsize
    writeBin(as.numeric(scalar),output,size=4) #scalar

    writeBin(data_period_vec,output,size=2)
    writeBin(data_real_vec,output,size=2)
    close(output)
    cat("[done]\n")



    if(TEST==TRUE){
        data_1861<-read.input.yearband(outname,data.size=2,band=1,year=1861)
        data_1901<-read.input.yearband(outname,data.size=2,band=1,year=1901)
        if(!all((data_1861-data_1901)==0))
            stop("testing failed: 1861 != 1901 band 1")

        data_1881<-read.input.yearband(outname,data.size=2,band=1,year=1881)
        if(!all((data_1881-data_1901)==0))
            stop("testing failed: 1881 != 1901 band 1")

        data_1881<-read.input.yearband(outname,data.size=2,band=5,year=1881)
        data_1901<-read.input.yearband(outname,data.size=2,band=5,year=1901)
        if(!all((data_1881-data_1901)==0))
            stop("testing failed: 1881 != 1901 band 5")
            
        data_1871<-read.input.yearband(outname,data.size=2,band=5,year=1871)
        data_1911<-read.input.yearband(outname,data.size=2,band=5,year=1911)
        if(!all((data_1871-data_1911)==0))
            stop("testing failed: 1871 != 1911 band 5")
            
        #--------------------
        data_1861<-read.input.yearband(outname,data.size=2,band=2,year=1861)
        if(!all((data_1861-data[2,,1])==0))
            stop("testing failed ori: 1861 != 1901")

        data_1881<-read.input.yearband(outname,data.size=2,band=3,year=1881)
        if(!all((data_1881-data[3,,1])==0))
            stop("testing failed: 1881 != 1901 band 3")

        data_1971<-read.input.yearband(outname,data.size=2,band=7,year=1871)
        if(!all((data_1971-data[7,,11])==0))
            stop("testing failed: 1871 != 1911")
            
        data_2014<-read.input.yearband(outname,data.size=2,band=12,year=2014)
        if(!all((data_2014-data[12,,114])==0))
            stop("testing failed: 2014 != 2014")

    }
}
