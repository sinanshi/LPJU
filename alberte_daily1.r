#Compare daily outputs
#
#-----------------------------------



rm (list=ls(all=TRUE))
#load required libraries
  require(fields)
  require(maps)
  library(reshape2)
#load header
  source("header")
  

  

#------------------------------------------------
#run lpjml and get data
#------------------------------------------------
#read grid to get latitude lontitude information
 read.grid()
 plot(lon,lat)
 point<-1
 coor.array<-array(NA,dim=c(30,2))
 
while(TRUE){
 #fetch latitude lontitude information by clicking on the map
 cat("click on the map to choose the coordinator(",point,")\n")
 temp<-locator(1)
 coor<-array(NA,dim=2)

 #To get the closest (lon,lat) where clicked
 coor[1]=round.grid(temp$x,res)
 coor[2]=round.grid(temp$y,res)
 coor.array[point,1]=coor[1]
 coor.array[point,2]=coor[2] 
 
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
daily.frame.out<-lpjml.dailyrun(coor.array,point)#spinup year was set as 50



#------------------------------------------
#plot data:can be personalised
#------------------------------------------
  #plot years
  begin<-1920
  end<-1922
  
  #data to plot
  plot.list<-c("d_cleaf","d_gpp","d_prec","d_temp")
    
  pos.begin<-(begin-simstartyear)*365+1
  pos.end<-(end-simstartyear)*365
  time<-c(1:(pos.end-pos.begin+1))
  
  par(mfrow=c(length(plot.list),point))
    
    for(j in 1:length(plot.list)){
      for(i in 1:point){
        info.o<-vars.check(plot.list[j])#get the information of output e.g. description, unit
        title<-paste(info.o$id,"---",info.o$des," at ","(",coor.array[i,1],"E,",coor.array[i,2],"N)",sep="")
        yl<-info.o$unit#ylabl=unit
        xl<-paste("Year","(",begin,"-",end,")",sep="")#xlablel
        dataplot<-daily.frame.out[c(pos.begin:pos.end),plot.list[j],i]
      plot(time,dataplot,main=title,xlab=xl,ylab=yl,"l")
     }
   }  
   

