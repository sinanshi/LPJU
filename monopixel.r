#############################################
#
#
#####################################

source('header')


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


monopixel<-function(){
read.grid()
plot(lon,lat)

#while(TRUE){
temp<-locator(1)
coor<-array(NA,dim=2)
coor[1]=round.grid(temp$x,res)
coor[2]=round.grid(temp$y,res)


pos_lat_count<-0
pos_lon=NA
Have_lon<-FALSE
Have_lat<-FALSE

for(i in 1:(length(grid.data)/2)){
  if(grid.data[2*i-1]==coor[1]){
    pos_lat_count<-(pos_lat_count+1)
    pos_lon<-2*i-1
    Have_lon<-TRUE
  }
}

for(i in 1:pos_lat_count){
  if(grid.data[pos_lon+2*i-1]==coor[2]){
    pos=pos_lon+2*i-1
    Have_lat<-TRUE
  }
}

if(Have_lat==FALSE|Have_lon==FALSE){
  print("Error:LPJ do not provide any data on the sea!")
  print("      Please choose a land pixel and try again.")
  stop()  
}

pos=pos/2-1
system2('./monopixelrun.sh',args=pos,wait=TRUE)
}





