 
#Compare daily outputs
#
#-----------------------------------



#rm (list=ls(all=TRUE))
#load required libraries
  require(fields)
  require(maps)
  library(reshape2)
#load header
  source("header")
  
  
  #global data scale (years)
  global.year<-c(1901,1902)
  
  #location input type setting
  #input.type<-0  #The locations input by clicking on the map
  input.type<-1  #The locations input by exact lontitude latitude
  
  #------------------------------------------
#plot data:can be personalised
#------------------------------------------
daily.plot<-function(){ 
 #plot years
  begin<-1988; end<-2004

  loc.d<-dim(daily.frame.out)[3]
  loc.m<-dim(monthly.frame.out)[3]
  loc.y<-dim(yearly.frame.out)[3]
  #data to plot
  plot.list.d<-c("d_temp","d_gpp")
  plot.list.m<-c("mnpp")
  plot.list.y<-c("soilc","vegc")
  

  
  
  plot.total<-length(plot.list.d)+length(plot.list.m)+length(plot.list.y)
  if(plot.list.d[1]=="NA") plot.total<-plot.total-1
  if(plot.list.m[1]=="NA") plot.total<-plot.total-1
  if(plot.list.y[1]=="NA") plot.total<-plot.total-1
  
  
  par(mfrow=c(plot.total,1))
  #plot daily data
  
  pos.begin.d<-(begin-simstartyear)*365+1
  pos.end.d<-(end-simstartyear)*365
  time.d<-c(1:(pos.end.d-pos.begin.d+1))
  if(plot.list.d[1]!="NA"){
  for(j in 1:length(plot.list.d)){
      maxdata<-max(daily.frame.out[c(pos.begin.d:pos.end.d),plot.list.d[j],])
      mindata<-min(daily.frame.out[c(pos.begin.d:pos.end.d),plot.list.d[j],])
      for(i in 1:loc.d){
        info.o<-vars.check(plot.list.d[j])#get the information of output e.g. description, unit
        title<-paste(info.o$id,"---",info.o$des,sep="")
        yl<-info.o$unit#ylabl=unit
        xl<-paste("Year","(",begin,"-",end,")",sep="")#xlablel
        dataplot<-daily.frame.out[c(pos.begin.d:pos.end.d),plot.list.d[j],i]
        if(i==1) 
          plot(ylim=c(mindata,maxdata),time.d,dataplot,main=title,xlab=xl,ylab=yl,"l",col=i)
        else
          lines(time.d,dataplot,col=i)
     }
     legend(0.95*max(time.d),0.95*maxdata,as.character(1:loc.d),lty = 1,col=c(1:loc.d))
   }  
  } 
   
  if(plot.list.m[1]!="NA"){
  pos.begin.m<-(begin-simstartyear)*12+1
  pos.end.m<-(end-simstartyear)*12 
  time.m<-c(1:(pos.end.m-pos.begin.m+1))

   for(j in 1:length(plot.list.m)){
      maxdata<-max(monthly.frame.out[c(pos.begin.m:pos.end.m),plot.list.m[j],])
      mindata<-min(monthly.frame.out[c(pos.begin.m:pos.end.m),plot.list.m[j],])
      for(i in 1:loc.m){
        info.o<-vars.check(plot.list.m[j])#get the information of output e.g. description, unit
        title<-paste(info.o$id,"---",info.o$des,sep="")
        yl<-info.o$unit#ylabl=unit
        xl<-paste("Year","(",begin,"-",end,")",sep="")#xlablel
        dataplot<-monthly.frame.out[c(pos.begin.m:pos.end.m),plot.list.m[j],i]
        if(i==1) 
         plot(ylim=c(mindata,maxdata),time.m,dataplot,main=title,xlab=xl,ylab=yl,"l",col=i)
        else
         lines(time.m,dataplot,col=i)
     }
     legend(0.95*max(time.m),0.95*maxdata,as.character(1:loc.m),lty = 1,col=c(1:loc.m))
   }  
  }
  
  if(plot.list.y[1]!="NA"){
  pos.begin.y<-(begin-simstartyear)+1
  pos.end.y<-(end-simstartyear) 
  time.y<-c(1:(pos.end.y-pos.begin.y+1))

   for(j in 1:length(plot.list.y)){
      maxdata<-max(yearly.frame.out[c(pos.begin.y:pos.end.y),plot.list.y[j],])
      mindata<-min(yearly.frame.out[c(pos.begin.y:pos.end.y),plot.list.y[j],])
      for(i in 1:loc.y){
        info.o<-vars.check(plot.list.y[j])#get the information of output e.g. description, unit
        title<-paste(info.o$id,"---",info.o$des,sep="")
        yl<-info.o$unit#ylabl=unit
        xl<-paste("Year","(",begin,"-",end,")",sep="")#xlablel
        dataplot<-yearly.frame.out[c(pos.begin.y:pos.end.y),plot.list.y[j],i]
        if(i==1) 
         plot(ylim=c(mindata,maxdata),time.y,dataplot,main=title,xlab=xl,ylab=yl,"l",col=i)
        else
         lines(time.y,dataplot,col=i)
     }
     legend(0.95*max(time.y),0.95*maxdata,as.character(1:loc.y),lty = 1,col=c(1:loc.y))
   }  
  } 
   
} 
  

 

#------------------------------------------------
#run lpjml and plot data
#------------------------------------------------
#read grid to get latitude lontitude information
read.grid(path.out)
multirun.gui()





