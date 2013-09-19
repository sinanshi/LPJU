require(reshape)
require(reshape2)

#-----------------------------------------
#fuctions
#-----------------------------------------


#-------------------------------
#return [gridsize,simulation years]
#-------------------------------
get.output.info<-function(path){
  grid.fn.out <- paste(path,"grid.bin",sep="")
  npixel.out <- file.info(grid.fn.out)$size/sizeof.data
  nyear.out <-  file.info(paste(path,"vegc.bin",sep=""))$size/sizeof.data/npixel.out
  p_y<-c(npixel.out,nyear.out)

  p_y
}

#-------------------------------------
#return simulation year for daily output (NOT USEFUL!!!)
#-------------------------------------
get.output.daily.info<-function(daily.fn){
 nyear.out<-file.info(daily.fn)$size/sizeof.data/365 
 return(nyear.out) 
}


#----------------------------------
#read output parameter information
#: id, description, unit 
#----------------------------------
read.vars.info<-function(){
  var.raw<-read.csv("outputvars.par",sep=" ",header=TRUE)
  id<-array(var.raw$id)
  name<-array(var.raw$name)  
  des<-array(var.raw$description) 
  unit<-array(var.raw$unit)         
  var.frame<-data.frame(id,name,des,unit)
return(var.frame)
}


#----------------------------------
#vars.check
#---------------------------------
vars.check<-function(output.name){
  output.name<-gsub(".data.out",replacement="",x=output.name)
 for(i in 1:dim(vars.info)[1]){#number of vars list
  if(output.name==vars.info$name[i]||output.name==vars.info$id[i]){
    output.info<-data.frame(id=vars.info$id[i],
                            name=vars.info$name[i],
                            des=vars.info$des[i],
                            unit=vars.info$unit[i]) 
    
    break
  }
 }
  if(exists("output.info")==FALSE){
   stop("output name not found in the 'outputvars.par' table.")
 }

 return(output.info)

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
#
#-------------------------------
lonlat2grid<-function(longitude,latitude){
  if(exists("longitude")==FALSE||exists("latitude")==FALSE){
   cat("ERROR:To run mono-pixel LPJmL, longitude latitude have not been configured!")
   stop()
 }
 pos<-which(lon==longitude&lat==latitude)
 
 if(length(pos)!=1){
   cat("Error:The pixel you chosen is not available on LPJmL grid\n")
   stop()  
 }
 else   return(pos)
 
}

#-------------------------------
#plot multiple data seperately
#-------------------------------

plot.data.seperate<-function(dim_init,data.num,syear,eyear,datalist){
if(dim_init==2){
      time<-c(syear:eyear)
      par(mfrow=c(data.num,1))
      for(i in 1:data.num){
         data<-global.area.mean(eval(parse(text=datalist[i])))
         plotdata<-data[(syear-simstartyear+1):(eyear-simstartyear+1)]
         name<-gsub(".data.out","",datalist[i])
         info.o<-vars.check(name)
         title<-paste(info.o$des,sep="")
         unit<-paste(info.o$id,info.o$unit)
         xl<-paste("Year","(",syear,"-",eyear,")",sep="")
         plot(time,plotdata,"l",main=title,xlab=xl,ylab=unit)
        }
    }
    if(dim_init==3){
      #time<-as.vector(t(replicate(12,syear:eyear)))
      #time<-paste(as.character(time),label.month)
      par(mfrow=c(data.num,1))
      for(i in 1:data.num){
         data<-as.vector(global.area.mean(eval(parse(text=datalist[i]))))
         plotdata<-data[c(((syear-simstartyear)*12+1):((eyear-simstartyear+1)*12))]
         name<-gsub(".data.out","",datalist[i])
         info.o<-vars.check(name)
         title<-paste(info.o$des,sep="")
         unit<-paste(info.o$id,info.o$unit)
         xl<-paste("Month","(",syear,"-",eyear,")",sep="")
         plot(c(1:length(plotdata)),plotdata,"l",main=title,xlab=xl,ylab=unit)
        
      } 
    }
}




#----------------------------------
#which country:
#----------------------------------
which.country<-function(country.name){
 

}






