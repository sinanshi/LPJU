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
    
    seek(file.in,7, origin = "start")
    version<-readBin(file.in,integer(),n=1,size=4)
    order<-readBin(file.in,integer(),n=1,size=4)
    firstyear<-readBin(file.in,integer(),n=1,size=4)
    nyears<-readBin(file.in,integer(),n=1,size=4)
    firstcell<-readBin(file.in,integer(),n=1,size=4)   
    ncells<-readBin(file.in,integer(),n=1,size=4)
    nbands<-readBin(file.in,integer(),n=1,size=4)
    cellsize<-readBin(file.in,numeric(),n=1,size=4)
    scalar<-readBin(file.in,numeric(),n=1,size=4)
    header<-data.frame(version,order,firstyear,nyears,firstcell,ncells,nbands,cellsize,scalar)
    close(file.in)
    return(header)
     
}
 
read.input.grid<-function(path.in){
     input.list<-dir(path.in)
     grid.name<-paste(path.in,input.list[grep("grid.bin",input.list)],sep="")
     grid.header<-read.input.header(grid.name)
     
     gridfile<- file(grid.name,"rb")
     seek(gridfile,HEADER_SIZE, origin = "start")
    grid.data<<-readBin(gridfile,integer(),n=2*grid.header$ncells,size=2)*grid.header$scalar
    lon<<-grid.data[c(1:grid.header$ncells)*2-1]
    lat<<-grid.data[c(1:grid.header$ncells)*2]
    ind_lon<<- as.integer((grid.data[c(1:grid.header$ncells)*2-1]+20)/res + 1.01)
    ind_lat<<- as.integer((grid.data[c(1:grid.header$ncells)*2]-25)/res + 1.01)
    close(gridfile)
}

#temporary read function for testing
read.input.files<-function(filename,data.size){#short 2; long 8; int 4; char 1; on 64 bit OS
    fileHeader<-read.input.header(filename)
    file.in <- file(sprintf(filename),"rb")
    data.in<-array(NA,dim=c(fileHeader$nyears,fileHeader$ncells,fileHeader$nbands))
    temp<-readBin(file.in,integer(),n=fileHeader$nbands*fileHeader$nyears*fileHeader$ncells,size=data.size)*fileHeader$scalar
    temp2<-array(NA,c(fileHeader$nyears*fileHeader$ncells , fileHeader$nbands))
    seek(file.in,where=HEADER_SIZE,origin="start")
    for(i in 1:(fileHeader$nyears*fileHeader$ncells))  temp2[i,] <-temp[c(((fileHeader$nbands*(i-1))+1):(fileHeader$nbands*(i)))]
    for(i in 1:(fileHeader$nyears))                              data.in[i,,]<-temp2[c(((fileHeader$ncells*(i-1))+1):(fileHeader$ncells*(i))),]
    close(file.in)
    return(data.in)
}




# 
# read.input.soil<-function(path.in){
#   input.list<-dir(path.in)
#   file.name<-paste(path.in,input.list[grep("soil",input.list)],sep="")
#   filesoil<-file(sprintf(file.name),"rb")
#   soilpar<-readBin(filesoil,integer(),n=npixel,size=1)
#   
#   soilpar
#   
#   }
