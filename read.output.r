################################################################
#read all yearly output data including 
#1. carbon data
#2. flux data
################################################################


files.out <- c("vegc","soilc","litc","firec","flux_estab","mnpp","mrh","mevap","mtransp","mrunoff","fpc")

#notice that it includes monthly output and yearly output.


read.output.carbon<-function(){
 # define output date array
 vegc.data.out<<-array(NA,c(npixel.out,simyears))
 soilc.data.out<<-array(NA,c(npixel.out,simyears))
 litc.data.out<<-array(NA,c(npixel.out,simyears))
 firec.data.out<<-array(NA,c(npixel.out,simyears))
 fluxestab.data.out<<-array(NA,c(npixel.out,simyears))

 vegc.fn.out<- file(paste(path.out,"vegc.bin",sep=""),"rb")
 vegc.data.out[,]<<-readBin(vegc.fn.out,double(),npixel.out*nyear.out,size=4)
 

 soilc.fn.out<- file(paste(path.out,"soilc.bin",sep=""),"rb")
 soilc.data.out[,]<<-readBin(soilc.fn.out,double(),npixel.out*nyear.out,size=4)

 litc.fn.out<- file(paste(path.out,"litc.bin",sep=""),"rb")
 litc.data.out[,]<<-readBin(litc.fn.out,double(),npixel.out*nyear.out,size=4)

 firec.fn.out<- file(paste(path.out,"firec.bin",sep=""),"rb")
 firec.data.out[,]<<-readBin(firec.fn.out,double(),npixel.out*nyear.out,size=4)


 fluxestab.fn.out<- file(paste(path.out,"flux_estab.bin",sep=""),"rb")
 fluxestab.data.out[,]<<-readBin(fluxestab.fn.out,double(),npixel.out*nyear.out,size=4)
 
 closeAllConnections()
 
}

read.output.flux<-function(){

 #define output data array
 mnpp.data.out<<-array(NA,c(npixel.out,12,simyears))
 mrh.data.out<<-array(NA,c(npixel.out,12,simyears))
 mevap.data.out<<-array(NA,c(npixel.out,12,simyears))
 mtransp.data.out<<-array(NA,c(npixel.out,12,simyears))
 mrunoff.data.out<<-array(NA,c(npixel.out,12,simyears))

 mnpp.fn.out<- file(paste(path.out,"mnpp.bin",sep=""),"rb")
 mnpp.data.out[,,]<<-readBin(mnpp.fn.out,double(),npixel.out*nyear.out*12,size=4)

 mrh.fn.out<- file(paste(path.out,"mrh.bin",sep=""),"rb")
 mrh.data.out[,,]<<-readBin(mrh.fn.out,double(),npixel.out*nyear.out*12,size=4)

 mevap.fn.out<- file(paste(path.out,"mevap.bin",sep=""),"rb")
 mevap.data.out[,,]<<-readBin(mevap.fn.out,double(),npixel.out*nyear.out*12,size=4)

 mtransp.fn.out<- file(paste(path.out,"mtransp.bin",sep=""),"rb")
 mtransp.data.out[,,]<<-readBin(mtransp.fn.out,double(),npixel.out*nyear.out*12,size=4)

 mrunoff.fn.out<- file(paste(path.out,"mrunoff.bin",sep=""),"rb")
 mrunoff.data.out[,,]<<-readBin(mrunoff.fn.out,double(),npixel.out*nyear.out*12,size=4)
 
 closeAllConnections()

}


read.output.all<-function(){
  read.output.carbon()
  read.output.flux()
}
 


read.grid<-function(){

  res=0.5
  grid.fn.out<- file(paste(path.out,"grid.bin",sep=""),"rb")
  grid.data<<-readBin(grid.fn.out,integer(),n=2*npixel.out,size=2)/100
  lon<<-grid.data[c(1:npixel.out)*2-1]
  lat<<-grid.data[c(1:npixel.out)*2]
  ind_lon<<- as.integer((grid.data[c(1:npixel.out)*2-1]+20)/res + 1.01)
  ind_lat<<- as.integer((grid.data[c(1:npixel.out)*2]-25)/res + 1.01)
  close(grid.fn.out)
}

