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
 
 if(exists("lon.mono")==FALSE||exists("lat.mono")==FALSE||exists("spinup.mono")==FALSE){
   cat("ERROR:To run mono-pixel LPJmL, Both position and spinup year have to be configured!")
   stop()
 }
 pos_lat_count<-0
 pos_lon=NA
 Have_lon<-FALSE
 Have_lat<-FALSE

 
 for(i in 1:(length(grid.data)/2)){
   if(lon[i]==lon.mono){
     pos_lon<-i
     Have_lon<-TRUE
     if(lat[i]==lat.mono){
       pos<-i
       Have_lat<-TRUE
       break
     }
   } 
 }
 
 
 if(Have_lat==FALSE|Have_lon==FALSE){
   print("Error:LPJ do not provide any data on the sea!")
   print("      Please choose a land pixel and try again.")
   stop()  
 }
  cat("grid number=",pos,"\n")
  cat("(lontitude,latitude)=",lon[pos],lat[pos],"\n")
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

#####################################
#run monopixel LPJmL by clicking on map
#####################################
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
 coor.array[point,1]<<-coor[1]
 coor.array[point,2]<<-coor[2] 
 
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





