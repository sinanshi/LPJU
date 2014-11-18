#source('~/R/Sinan/local130919/alberte/alberte_daily6.r')
#!!! to call this script: need to be on ~/R/Sinan/local130919/, where all the functions are defined  

# alb 131022: for comparing two LPJmL codes versions

rm(list=ls(all=TRUE))
 
#Compare daily outputs
#
#-----------------------------------



#rm (list=ls(all=TRUE))
#load required libraries
  require(fields)
  require(maps)
  library(reshape2)
  print("OK0\n")

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
  begin<-2000; end<-2004

  loc.d<-dim(daily.frame.out)[3]
  loc.m<-dim(monthly.frame.out)[3]
  loc.y<-dim(yearly.frame.out)[3]
  #data to plot
  plot.list.d<-c("NA")
  plot.list.m<-c("mnpp")
  plot.list.y<-c("vegc")
  
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

} # end of function daily.plot 
  

 

#------------------------------------------------
#run lpjml and plot data
#------------------------------------------------
#read grid to get latitude lontitude information
print("OK1\n")
read.grid(path.out)       #function defined within read.output.r
                          #path.out was defined within benchmark.settings:
                          #path.out<-paste(getwd(),"/../outputs/output_2013_test/",sep="") 
                          # these are outputs of a OT-Med run (6089 pixels)

print("OK2\n")
#multirun.gui()            #function defined within MultiRun_GUI.r, for the interactive box. 

runs <- 3
lonlat.array<<-array(NA,dim=c(runs,2))

#lonlat.array <- matrix(c(2.25,48.25,
#                         4.25,44.25,
#                         8.25,45.25),ncol=2,byrow=T) 

lonlat.array <- matrix(c(4.25,44.25,
                         4.25,44.25,
                         8.25,45.25),ncol=2,byrow=T) 
                         
                         
                         
                         
theParameter.array <- "Location"   # "Location" or "Soil_Par"

 #make the directory according the current time, all the results will be
  #stored in VisualResults/
  current.day<-format(Sys.time(), "%Y-%b-%d")
  current.time<-format(Sys.time(), "%H:%M:%S")
  current.name<<-paste("../VisualResults/",current.day,"[",current.time,"]","/",sep="")
  system2("mkdir",args=current.name)
  

for(r in 1:2){
  
  # compute the pixel number in the grid (starts with 0), lonlat2grid  defined in benchmark_fuctions.r
  grid<-lonlat2grid(lonlat.array[r,1],lonlat.array[r,2])-1     
  
  #put the correct grid number within lpjml.conf
  system2("./monorunconf.sh",args=grid)  
  
   
  #run LPJmL!!
 # 
  if(r==1) {
      system("./runonepixel.sh 111")
    }
  if(r==2) {
      system("./runonepixel_test.sh")
    }

  browser()
  
  
  
  # path.mono defined in benchmark.settings. The outputs in the ~/R/Sinan/--LPJmL.v3.5.003/output are 
  # rewritten each time. If the outputs need to be saved: 
  
  # creation of different directories for the different outputs:
  
   
  testname <<- paste("~/R/Sinan/VisualResults/",current.day,sprintf("_p%d",grid),"/",sep="")
  system2("mkdir",args=testname)
  
   if(r==1) {
      args1 <- paste("~/R/Sinan/--LPJmL.v3.5.003/output/*.*")
   }
   if(r==2) {
      args1 <- paste("~/R/Sinan/0_LPJmL.in.progress/output/*.*")
   }

  args2 <- paste(testname,"/.",sep="")
  command <- paste("cp ", args1,args2)
  cat(command, "\n")
  s <- system(command, FALSE)

  daily.temp<-read.daily.output(path.mono)
  daily.temp$ID<-rep(r,length(daily.temp[,1]))
  
  
  monthly.temp<-read.monthly.output(path.mono)
  monthly.temp$ID<-rep(r,length(monthly.temp[,1]))
   
  yearly.temp<-read.yearly.output(path.mono)
  yearly.temp$ID<-rep(r,length(yearly.temp[,1])) #give ID label to identify the data of runs
 
  if(r==1){
    daily.frame<-daily.temp
    yearly.frame<-yearly.temp
    monthly.frame<-monthly.temp
  }
  else{
    daily.frame<-rbind(daily.frame,daily.temp)
    yearly.frame<-rbind(yearly.frame,yearly.temp)
    monthly.frame<-rbind(monthly.frame,monthly.temp)
  }
  
  
  print(sprintf("r: %d done",r))
  #browser()
  
  
 } # end of  for(r in 1:runs)


   #realign data into a good form    
    daily.frame$row <- with(daily.frame, ave(ID==ID, ID, FUN = cumsum))
    m <- melt(daily.frame, id.vars = c("row", "ID"))
    daily.frame.out <<- acast(m, row ~ variable ~ ID)
    daily.frame.out.y <- array(NA, c(40,1,109,365))
  #  for(y in 1:109) {
    #daily.frame.out[c(pos.begin.d:pos.end.d),plot.list.d[j],])
    #     daily.frame.out.y[
    
    #realign data in a good form    
    monthly.frame$row <- with(monthly.frame, ave(ID==ID, ID, FUN = cumsum))
    m <- melt(monthly.frame, id.vars = c("row", "ID"))
    monthly.frame.out <<- acast(m, row ~ variable ~ ID)
    
    #realign data in a good form    
    yearly.frame$row <- with(yearly.frame, ave(ID==ID, ID, FUN = cumsum))
    m <- melt(yearly.frame, id.vars = c("row", "ID"))
    yearly.frame.out <<- acast(m, row ~ variable ~ ID)
   
    cat("done!\n")
    
    
    #daily.frame.out[1:730,"d_lai",]
    # j'ai bien 2 colonnes (2 pixels) de 730 valeurs chacune. 
    # Il faudrait que je fasse une 4ème dim avec l'année
    
 
#####################################################################"
# plot results:

daily.plot()
 
 
 