################################################################
#read all yearly output data including 
#1. carbon data
#2. flux data
################################################################


files.out <- c("vegc","soilc","litc","firec","flux_estab","mnpp","mrh","mevap","mtransp","mrunoff","fpc")

#notice that it includes monthly output and yearly output.


read.output.carbon<-function(path,npixel.out,nyear.out){
 # define output date array
 vegc.data.out<<-array(NA,c(npixel.out,nyear.out))
 soilc.data.out<<-array(NA,c(npixel.out,nyear.out))
 litc.data.out<<-array(NA,c(npixel.out,nyear.out))
 firec.data.out<<-array(NA,c(npixel.out,nyear.out))
 fluxestab.data.out<<-array(NA,c(npixel.out,nyear.out))

 vegc.fn.out<- file(paste(path,"vegc.bin",sep=""),"rb")
 vegc.data.out[,]<<-readBin(vegc.fn.out,double(),npixel.out*nyear.out,size=4)
 

 soilc.fn.out<- file(paste(path,"soilc.bin",sep=""),"rb")
 soilc.data.out[,]<<-readBin(soilc.fn.out,double(),npixel.out*nyear.out,size=4)

 litc.fn.out<- file(paste(path,"litc.bin",sep=""),"rb")
 litc.data.out[,]<<-readBin(litc.fn.out,double(),npixel.out*nyear.out,size=4)

 firec.fn.out<- file(paste(path,"firec.bin",sep=""),"rb")
 firec.data.out[,]<<-readBin(firec.fn.out,double(),npixel.out*nyear.out,size=4)

 fluxestab.fn.out<- file(paste(path,"flux_estab.bin",sep=""),"rb")
 fluxestab.data.out[,]<<-readBin(fluxestab.fn.out,double(),npixel.out*nyear.out,size=4)
 
 closeAllConnections()
 
}

read.output.flux<-function(path,npixel.out,nyear.out){

 #define output data array
 mnpp.data.out<<-array(NA,c(npixel.out,12,nyear.out))
 mrh.data.out<<-array(NA,c(npixel.out,12,nyear.out))
 mevap.data.out<<-array(NA,c(npixel.out,12,nyear.out))
 mtransp.data.out<<-array(NA,c(npixel.out,12,nyear.out))
 mrunoff.data.out<<-array(NA,c(npixel.out,12,nyear.out))

 mnpp.fn.out<- file(paste(path,"mnpp.bin",sep=""),"rb")
 mnpp.data.out[,,]<<-readBin(mnpp.fn.out,double(),npixel.out*nyear.out*12,size=4)

 mrh.fn.out<- file(paste(path,"mrh.bin",sep=""),"rb")
 mrh.data.out[,,]<<-readBin(mrh.fn.out,double(),npixel.out*nyear.out*12,size=4)

 mevap.fn.out<- file(paste(path,"mevap.bin",sep=""),"rb")
 mevap.data.out[,,]<<-readBin(mevap.fn.out,double(),npixel.out*nyear.out*12,size=4)

 mtransp.fn.out<- file(paste(path,"mtransp.bin",sep=""),"rb")
 mtransp.data.out[,,]<<-readBin(mtransp.fn.out,double(),npixel.out*nyear.out*12,size=4)

 mrunoff.fn.out<- file(paste(path,"mrunoff.bin",sep=""),"rb")
 mrunoff.data.out[,,]<<-readBin(mrunoff.fn.out,double(),npixel.out*nyear.out*12,size=4)
 
 closeAllConnections()

}


read.output.all<-function(path){
  pixel_year<-output.info(path)
  cat("=======================================================\n")
  cat("Path=",path,"\n")
  cat("pixel number=",pixel_year[1],"simulation years=",pixel_year[2],"\n")
  cat("Reading the output data...\n")
  read.output.carbon(path,pixel_year[1],pixel_year[2])
  read.output.flux(path,pixel_year[1],pixel_year[2])
  cat("read: <sucessful>\n")
  cat("=======================================================\n")
}
 


read.grid<-function(){
  pixel_year<-output.info(path.out)
  npixel.out<-pixel_year[1]
  res=0.5
  grid.fn.out<- file(paste(path.out,"grid.bin",sep=""),"rb")
  grid.data<<-readBin(grid.fn.out,integer(),n=2*npixel.out,size=2)/100
  lon<<-grid.data[c(1:npixel.out)*2-1]
  lat<<-grid.data[c(1:npixel.out)*2]
  ind_lon<<- as.integer((grid.data[c(1:npixel.out)*2-1]+20)/res + 1.01)
  ind_lat<<- as.integer((grid.data[c(1:npixel.out)*2]-25)/res + 1.01)
  close(grid.fn.out)
}

