#-----------------------------------------
#fuctions
#-----------------------------------------


#-------------------------------
#return [gridsize,simulation years]
#-------------------------------
output.info<-function(path){
  grid.fn.out <- paste(path,"grid.bin",sep="")
  npixel.out <- file.info(grid.fn.out)$size/sizeof.data
  nyear.out <-  file.info(paste(path,"vegc.bin",sep=""))$size/sizeof.data/npixel.out
  p_y<-c(npixel.out,nyear.out)

  p_y
}

#-------------------------------------
#return simulation year for daily output
#-------------------------------------
output.daily.info<-function(daily.fn){
 nyear.out<-file.info(daily.fn)$size/sizeof.data/365 
 return(nyear.out) 
}
#-------------------------------------
#Global.area.mean:return value of all pixels on a global map
#-------------------------------------
global.area.mean<-function(data){
  dimension<-dim(data)
  
  #yearly data 
  if(length(dimension)==2){
    data.mean<-array(NA,dim=dimension[2])
    for(i in 1:dimension[2]){
     data.mean[i]<-mean(data[,i])
    }
  }
  #monthly data
  else if(length(dimension)==3){
    data.mean<-array(NA,dim=c(dimension[2],dimension[3]))
    for(i in 1:dimension[2]){
      for(j in 1:dimension[3]){
      data.mean[i,j]<-mean(data[,i,j])
      }
    }
  }
  else{
   cat("ERROR in global.area.mean(): Do not recognize the input data. Input data can only be in 1 or 2 dimensions to represent yearly and monthly data respectively.  ")
  stop()
  }
  data.mean

}
 

#--------------------------------------
#month2year:return monthly average of each pixel
#-------------------------------------
month2year<-function(data.monthly){

 dimension<-dim(data.monthly)
 #one pixel case
 if(length(dimension)==2){
   cat("Function month2year:generating monthly average data for one pixel...")
   data.yearly<-array(NA,dim=dimension[2])
   for(i in 1:dimension[2])
    data.yearly[i]<-mean(data.monthly[,i])
 }
 if(length(dimension)==3){
   cat("Function month2year:generating monthly average data for global data...")
   data.yearly<-array(NA,dim=c(dimension[1],dimension[3]))
   for(i in 1:dimension[1]){
    for(j in 1:dimension[3]){
      data.yearly[i,j]<-mean(data.monthly[i,,j])
    }
   }
 }
 cat("done!\n")
 data.yearly   
}


#------------------------------------
#
#------------------------------------
get.onepixel.data<-function(data,pixel){
  dimension<-dim(data)
  if(length(dimension)==2){
   onepixel<-array(NA,dim=dimension[2])
   onepixel<-data[pixel,]
  }
  if(length(dimension)==3){
   onepixel<-array(NA,dim=c(dimension[2],dimension[3]))
   onepixel<-data[pixel,,]
 }
 onepixel 
}

#-----------------------------------------
# finds pixels in grid closest to a given array of longitudes/latitudes 
#-----------------------------------------
find.nearest.pixel <- function(grid,lon,lat,res=0.5){
  la <- (grid$ilat-1)*res+grid$ext.lat[1]
  lo <- (grid$ilon-1)*res+grid$ext.lon[1]
  ind <- slice.index(la,1)
  index <- rep(0,length(lon))
  #for each point
  for(p in 1:length(lon)){
    latdiff <- abs(la-lat[p])
    londiff <- abs(lo-lon[p])
    index[p]=ind[londiff==min(londiff[ind[latdiff==min(latdiff)]]) & latdiff==min(latdiff)][1]
  }
  index
}





#-------------------------------
#plot multiple data seperately
#-------------------------------

plot.data.seperate<-function(dim_init,data.num,syear,eyear,datalist){
if(dim_init==2){
      time<-c(syear:eyear)
      par(mfrow=c(data.num,1))
      for(i in 1:data.num){
         plot.data<-global.area.mean(eval(parse(text=datalist[i])))
         plot(time,plot.data[(syear-simstartyear+1):(eyear-simstartyear+1)],"l",main=datalist[i],xlab="year",ylab=datalist[i])
        }
    }
    if(dim_init==3){
      time<-as.vector(t(replicate(12,syear:eyear)))
      time<-paste(as.character(time),label.month)
      par(mfrow=c(data.num,1))
      for(i in 1:data.num){
         plot.data<-as.vector(global.area.mean(eval(parse(text=datalist[i]))))
         plot(c(1:length(time)),plot.data[c(((syear-simstartyear)*12+1):((eyear-simstartyear+1)*12))],main=datalist[i],xlab=paste("Month-[",syear,",",eyear,"]",sep=""),"l",ylab="")
        # axis(1,at=1:length(time),lab=time)
      } 
    }
}

#-----------------------------
#bind multiple data frames
#-----------------------------
lpjml.dailyrun<-function(coor.array,test.num){
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
 return(daily.frame.out)
}



