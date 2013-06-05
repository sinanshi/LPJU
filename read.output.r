################################################################
#read all yearly output data including 
#1. carbon data
#2. flux data
################################################################


files.out <<- c("vegc","soilc","litc","firec","flux_estab","mnpp","mrh","mevap","mtransp","mrunoff","fpc")
daily.files.list<<-c("d_cleaf","d_cpool","d_croot","d_cso","d_daylength","d_evap","d_fhiopt","d_fphu","d_froot","d_gpp","d_gresp","d_growingday","d_hi",
"d_himind","d_husum","d_irrig","d_lai","d_laimax_adjusted","d_laimaxnppdeficit","d_npp","d_par","d_perc","d_pet","d_phen","d_phu","d_prec","d_pvd","d_rd",
"d_rpool","d_rroot","d_rso","d_sun","d_temp","d_trans","d_vdsum","d_w0","d_w1","d_wdf","d_wevap","d_wscal")

#d_fphu? d_phu? d_so? d_leaf?

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


read.daily.output<-function(path){
  nyear<-output.daily.info(paste(path,daily.files.list[1],".bin",sep=""))
  temp<-array(NA,dim=nyear*365)
  daily.data.frame<<-data.frame(d_cleaf=temp,d_cpool="",d_croot="",d_cso="",d_daylength="",d_evap="",d_fhiopt="",d_fphu="",d_froot="",d_gpp="",d_gresp="",d_growingday="",d_hi="",
d_himind="",d_husum="",d_irrig="",d_lai="",d_laimax_adjusted="",d_laimaxnppdeficit="",d_npp="",d_par="",d_perc="",d_pet="",d_phen="",d_phu="",d_prec="",d_pvd="",d_rd="",
d_rpool="",d_rroot="",d_rso="",d_sun="",d_temp="",d_trans="",d_vdsum="",d_w0="",d_w1="",d_wdf="",d_wevap="",d_wscal="")
  
  for(i in 1:length(daily.files.list)){
    
    daily.fn<-file(paste(path,daily.files.list[i],".bin",sep=""),"rb")
    nyear<-output.daily.info(paste(path,daily.files.list[i],".bin",sep=""))
    cat("Reading",daily.files.list[i],"...")
    temp<-readBin(daily.fn,double(),365*nyear,size=sizeof.data)
    daily.data.frame[,daily.files.list[i]]<-temp
    cat("done\n")
    closeAllConnections()
  }
  
  return(daily.data.frame)
  
}  
    
    

