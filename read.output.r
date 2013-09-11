################################################################
#read all yearly output data including 
#1. carbon data
#2. flux data
################################################################


files.out <<- c("vegc","soilc","litc","firec","flux_estab","mnpp","mrh","mevap","mtransp","mrunoff","fpc")

  

#daily output file list
#notice:once it has been changed daily.data.frame should also be changed correspondingly
daily.files.list<<-c("d_cleaf","d_cpool","d_croot","d_cso","d_daylength","d_evap","d_fhiopt","d_fphu","d_froot","d_gpp","d_gresp","d_growingday","d_hi",
"d_himind","d_husum","d_irrig","d_lai","d_laimax_adjusted","d_laimaxnppdeficit","d_npp","d_par","d_perc","d_pet","d_phen","d_phu","d_prec","d_pvd","d_rd",
"d_rpool","d_rroot","d_rso","d_sun","d_temp","d_trans","d_vdsum","d_w0","d_w1","d_wdf","d_wevap","d_wscal")
monthly.files.list<<-c("mevap","mfpar","mgpp","minterc","mnpp","mrh","mrunoff","mswc1","mswc2","mtransp")
yearly.files.list<<-c("firec","firef","flux_estab","flux_harvest","litc","soilc","vegc")

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


#==========================
#read the grid and give the lat,lon and other information 
#==========================
read.grid<-function(path){
  pixel_year<-get.output.info(path)
  npixel.out<-pixel_year[1]
  #res=0.5
  grid.fn.out<- file(paste(path,"grid.bin",sep=""),"rb")
  grid.data<<-readBin(grid.fn.out,integer(),n=2*npixel.out,size=2)/100
  lon<<-grid.data[c(1:npixel.out)*2-1]
  lat<<-grid.data[c(1:npixel.out)*2]
  ind_lon<<- as.integer((grid.data[c(1:npixel.out)*2-1]-bound_west)/res + 1.01)
  ind_lat<<- as.integer((grid.data[c(1:npixel.out)*2]-bound_south)/res + 1.01)
  close(grid.fn.out)
}



#=========================
#read country files
#=========================
read.cow<-function(path){
 npixel.out<-get.output.info(path)[1]
 country.fn.out<-file(paste(path,"country.bin",sep=""),"rb")
 country.data<<-readBin(country.fn.out,integer(),n=npixel.out,size=2)
 close(country.fn.out)
}

#==========================
#read harvest data from pft_harvest
#==========================
read.harvest.data<-function(path,harvest.name.seclect){

 cat("reading harvest output...") 
 npixel.out<-get.output.info(path)[1]
 nyear.out<-get.output.info(path)[2]
 
 harvest.fn.out<-file(paste(path,"pft_harvest.pft.bin",sep=""),"rb")
 harvest.temp<-array(NA,c(npixel.out,nft,nyear.out))#the harvest file data were aligned as [pixels,plant funtional type,years]
 harvest.temp[,,]<-readBin(harvest.fn.out,double(),npixel.out*nyear.out*32,size=4)
 
 harvest.data<-array(NA,c(npixel.out,nyear.out,32))
 #change data alignment 
 for(i in 1:length(harvest.name)) harvest.data[,,i]<-harvest.temp[,i,]
 

 close(harvest.fn.out)
 cat("done\n")
 return(harvest.data)
}

#=======================
#daily output reading and put all daily output data into 
# a data frame
#=======================

read.daily.output<-function(path){
  nyear<-get.output.daily.info(paste(path,daily.files.list[1],".bin",sep=""))
  temp<-array(NA,dim=nyear*365)
#this should be the same list as daily.files.list
  daily.data.frame<<-data.frame(d_cleaf=temp,d_cpool="",d_croot="",d_cso="",d_daylength="",d_evap="",d_fhiopt="",d_fphu="",d_froot="",d_gpp="",d_gresp="",d_growingday="",d_hi="",
d_himind="",d_husum="",d_irrig="",d_lai="",d_laimax_adjusted="",d_laimaxnppdeficit="",d_npp="",d_par="",d_perc="",d_pet="",d_phen="",d_phu="",d_prec="",d_pvd="",d_rd="",
d_rpool="",d_rroot="",d_rso="",d_sun="",d_temp="",d_trans="",d_vdsum="",d_w0="",d_w1="",d_wdf="",d_wevap="",d_wscal="")
  
  for(i in 1:length(daily.files.list)){
    daily.fn<-file(paste(path,daily.files.list[i],".bin",sep=""),"rb")
    nyear<-get.output.daily.info(paste(path,daily.files.list[i],".bin",sep=""))
    cat("Reading",daily.files.list[i],"...")
    temp<-readBin(daily.fn,double(),365*nyear,size=sizeof.data)
    daily.data.frame[,daily.files.list[i]]<-temp
    cat("done\n")
    closeAllConnections()
  }
  
  return(daily.data.frame)
  
}  
    

#=======================
#daily output reading and put all daily output data into 
# a data frame
#=======================

read.yearly.output<-function(path){
  ncell<-get.output.info(path)[1]
  nyear<-get.output.info(path)[2]
  temp<-array(NA,dim=nyear)
#this should be the same list as daily.files.list
 yearly.data.frame<-data.frame(firec=temp,firef="",flux_estab="",flux_harvest="",litc="",soilc="",vegc="")
  
  for(i in 1:length(yearly.files.list)){
     yearly.fn<-file(paste(path,yearly.files.list[i],".bin",sep=""),"rb")
     cat("Reading",yearly.files.list[i],"...")
     temp<-readBin(yearly.fn,double(),nyear,size=sizeof.data)
     yearly.data.frame[,yearly.files.list[i]]<-temp
     cat("done\n")
     closeAllConnections()
  }
  
  return(yearly.data.frame)
 
}  
   

   
#=======================
#daily output reading and put all daily output data into 
# a data frame
#=======================

read.monthly.output<-function(path){
  ncell<-get.output.info(path)[1]
  nyear<-get.output.info(path)[2]
  temp<-array(NA,dim=nyear*12)
#this should be the same list as daily.files.list
 monthly.data.frame<-data.frame(mevap=temp,mfpar="",mgpp="",minterc="",mnpp="",mrh="",mrunoff="",mswc1="",mswc2="",mtransp="") 
  for(i in 1:length(monthly.files.list)){
     monthly.fn<-file(paste(path,monthly.files.list[i],".bin",sep=""),"rb")
     cat("Reading",monthly.files.list[i],"...")
     temp<-readBin(monthly.fn,double(),nyear*12,size=sizeof.data)
     monthly.data.frame[,monthly.files.list[i]]<-temp
     cat("done\n")
     closeAllConnections()
  }
  
  return(monthly.data.frame)
 
}  
   

    
read.output.all<-function(path){
  pixel_year<-get.output.info(path)
  cat("=======================================================\n")
  cat("Path=",path,"\n")
  cat("pixel number=",pixel_year[1],"simulation years=",pixel_year[2],"\n")
  cat("Reading the output data...\n")
  
  read.grid(path)
#  read.cow(path)
  read.output.carbon(path,pixel_year[1],pixel_year[2])
  read.output.flux(path,pixel_year[1],pixel_year[2])
  
  cat("read: <sucessful>\n")
  cat("=======================================================\n")
}

