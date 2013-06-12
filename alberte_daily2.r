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
  
  #change the name below to change the global maps
  global.maps.data<-"mnpp.data.out"
  
  #global data scale (years)
  global.year<-c(1901,1905)
  
  
  #------------------------------------------
#plot data:can be personalised
#------------------------------------------
 daily.plot<-function(){ 
 #plot years
  begin<-1920
  end<-1922
  point<-dim(daily.frame.out)[3]
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
   
}
  
  
  
read.output.all(path.out)
#------------------------------------------------
#run lpjml and get data
#------------------------------------------------
#read grid to get latitude lontitude information
 read.grid()
 map.temp<-map.create(eval(parse(text=global.maps.data)),global.year[1],global.year[2])
 map.interact(map.temp,global.year[1],global.year[2],colour)





