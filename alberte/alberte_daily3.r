 
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
  begin<-2000
  end<-2004
  point<-dim(daily.frame.out)[3]
  #data to plot
  plot.list<-c("d_temp","d_gpp","d_prec")
  pos.begin<-(begin-simstartyear)*365+1
  pos.end<-(end-simstartyear)*365
  time<-c(1:(pos.end-pos.begin+1))
  #:wX11()
  par(mfrow=c(length(plot.list),1))
  
    
   for(j in 1:length(plot.list)){
      maxdata<-max(daily.frame.out[c(pos.begin:pos.end),plot.list[j],])
      mindata<-min(daily.frame.out[c(pos.begin:pos.end),plot.list[j],])
      for(i in 1:point){
        info.o<-vars.check(plot.list[j])#get the information of output e.g. description, unit
        title<-paste(info.o$id,"---",info.o$des,sep="")
        yl<-info.o$unit#ylabl=unit
        xl<-paste("Year","(",begin,"-",end,")",sep="")#xlablel
        dataplot<-daily.frame.out[c(pos.begin:pos.end),plot.list[j],i]
        
        
        if(i==1) 
          plot(ylim=c(mindata,maxdata),time,dataplot,main=title,xlab=xl,ylab=yl,"l",col=i)
        else
          lines(time,dataplot,col=i)
     }
     legend(0.95*max(time),0.95*maxdata,as.character(1:point),lty = 1,col=c(1:point))
   }  
   

} 
  

 

#------------------------------------------------
#run lpjml and plot data
#------------------------------------------------
#read grid to get latitude lontitude information
read.grid(path.out)
multirun.gui()





