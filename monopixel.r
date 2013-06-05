#############################################
#Monopixel
#
#####################################


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
   if(grid.data[2*i-1]==lon.mono){
     pos_lat_count<-(pos_lat_count+1)
     pos_lon<-2*i-1
     Have_lon<-TRUE
   } 
 }

 for(i in 1:pos_lat_count){
   if(grid.data[pos_lon+2*i-1]==lat.mono){
     pos=pos_lon+2*i-1
     Have_lat<-TRUE
   }
 }

 if(Have_lat==FALSE|Have_lon==FALSE){
   print("Error:LPJ do not provide any data on the sea!")
   print("      Please choose a land pixel and try again.")
   stop()  
 }

 pos<-pos/2-1
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

#####################################
#run monopixel LPJmL by clicking on map
#####################################
monop.graph<-function(spinup.mono){
 read.grid()
 plot(lon,lat)
 temp<-locator(1)
 coor<-array(NA,dim=2)
 coor[1]=round.grid(temp$x,res)
 coor[2]=round.grid(temp$y,res)

 monop.lrun(coor[1],coor[2],spinup.mono)
}





