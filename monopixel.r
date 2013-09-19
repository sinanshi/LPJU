#############################################
#Monopixel
#
#####################################

library('ggmap')
####################################
#Find an proper (lat,lon) from values returned by selecting on the map 
####################################
round.grid<-function(num,res){
 compar<-seq(floor(num)-0.25,(ceiling(num)+1.25),res)
 min=1000#arbitrary
 for(i in 1:length(compar)){
   if(min>abs(num-compar[i])){
      min=abs(num-compar[i])
      round.value=compar[i]
   }
 }
 round.value
}

####################################
#run monopixel LPJmL with lontitude latitude and spinup year
###################################
monop.lrun<-function(lon.mono,lat.mono,spinup.mono){
  pos<-lonlat2grid(lon.mono,lat.mono)-1#NOTICE: in C grid number starts from 0
  cat("grid number=",pos,"\n")
 arg<-paste(pos,spinup.mono)
 system2('./monopixelrun.sh',args=arg,wait=TRUE)
}

#####################################
#run monopixel LPJmL with pixel number
#####################################
monop.prun<-function(pos.mono,spinup.mono){
 arg<-paste(pos.mono,spinup.mono)
 system2('./monopixelrun.sh',args=arg,wait=TRUE)
}

######################################
#text run
######################################
monop.trun<-function(position.name){
 
 pos<-array(NA,c(1,2))
 pos[1,1]<-geocode(position.name)$lon#Get the position input text from google lontitude
 pos[1,2]<-geocode(position.name)$lat#latitude
 pos[1,1]<-round.grid(pos[1,1])
 pos[1,2]<-round.grid(pos[1,2])
 coor.array<<-pos#Notice:global varible for ploting!
 cat(position.name,"\n")
 cat("---------------------\n")
 
 daily.frame.out<<-lpjml.dailyrun(pos,1)#for just one location
}

#-------------------------------------
#run monopixel LPJmL by clicking on map
#-------------------------------------
monop.graph<-function(spinup.mono=50){

 point<-1
 coor.array<<-array(NA,dim=c(30,2))
 
while(TRUE){
 #fetch latitude lontitude information by clicking on the map
 cat("click on the map to choose the coordinator(",point,")\n")
 temp<-locator(1)
 coor<-array(NA,dim=2)

 #To get the closest (lon,lat) where clicked
 coor[1]=round.grid(temp$x,res)
 coor[2]=round.grid(temp$y,res)
 coor.array[point,1]<<-coor[1]  #coor.array is a global varaible of indicating which location will be run on LPJmL
 coor.array[point,2]<<-coor[2] 
 
 lines(coor[1],coor[2],"o")#mark on the map
 text(coor[1],(coor[2]+1),labels=point,cex=5)#test on the map
 cat("lontitude=",coor[1],"latitude=",coor[2],"\n")
 chose<-readline("Press 'enter' to continue and 'q' to rechoose and type 'ok' for finishing:")
 if(chose=="q"){
   cat("Please rechoose the coordinator by clicking on the map")
   next
 } 

 else if(chose=="ok"){
   cat("end\n")
   break
 }
 point<-point+1
}

#run lpjml can return coupled data
daily.frame.out<<-lpjml.dailyrun(coor.array,point)#spinup year was set as 50

}



#--------------------------------------
#Run mono pixel by input lontitude and latitude
#-------------------------------------
monop.lonlat<-function(){
  coor.array<<-array(NA,dim=c(30,2))
  location.num<-readline("How many locations you want to compare?")#number of location
  for(i in 1:location.num){
    coor.array[i,1]<<-readline(paste("Lontitude of location",i,":"))
    coor.array[i,2]<<-readline(paste("Latitude of location",i,":"))
 }

daily.frame.out<<-lpjml.dailyrun(coor.array=coor.array,test.num=location.num)#spinup ear was set as 50
}


#-----------------------------
#daily run and bind multiple data in data frames
#-----------------------------
lpjml.dailyrun<-function(coor.array,test.num){
 cat("Aligning daily output data...")
 for(i in 1:test.num){ 
  monop.lrun(coor.array[i,1],coor.array[i,2],spinup.mono=50)
  daily.temp<-read.daily.output(path.mono)
  daily.temp$ID<-rep(i,length(daily.temp[,1]))
 if(i==1){
   daily.frame<-daily.temp
 }
 else{
 daily.frame<-rbind(daily.frame,daily.temp)
 }
}
 daily.frame$row <- with(daily.frame, ave(ID==ID, ID, FUN = cumsum))
 m <- melt(daily.frame, id.vars = c("row", "ID"))
 daily.frame.out <- acast(m, row ~ variable ~ ID)
 cat("done!\n")
 return(daily.frame.out)
}



