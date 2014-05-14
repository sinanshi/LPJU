################
#read inputs 
#
#
################

#setting path of input
path.in<-"../../NelasInputs/OUTPUT/"
HEADER_SIZE<-43

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
#temporary read function for testing
read.input.files.one<-function(filename,data.size,year,nband){#short 2; long 8; int 4; char 1; on 64 bit OS
    fileHeader<-read.input.header(filename)
    file.in <- file(sprintf(filename),"rb")
    data.in<-array(NA,dim=c(fileHeader$ncells))
    year.check<-1+fileHeader$firstyear-year

    seek(file.in,where=HEADER_SIZE,origin="start")
          for(p in 1:fileHeader$ncells){
                 seek(file.in,where=(HEADER_SIZE+Lindex(fileHeader$ncells,fileHeader$nbands,year.check,p,nband)),
                              origin="start")
               data.in[p]<-readBin(file.in, integer(), n=1, size=data.size)*fileHeader$scalar
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

# read.input.soil<-function(path.in){
#   input.list<-dir(path.in)
#   file.name<-paste(path.in,input.list[grep("soil",input.list)],sep="")
#   filesoil<-file(sprintf(file.name),"rb")
#   soilpar<-readBin(filesoil,integer(),n=npixel,size=1)
#   
#   soilpar
#   
#}
