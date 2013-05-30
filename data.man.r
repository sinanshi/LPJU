#=============================
#Data manupilation
#============================






#============================
#plot data on same map
#
#============================
 plot.query<-function(path){
   py<-output.info(path)
   npixel<-py[1]
   year<-py[2]

   cat("List of Methods\n")
   cat("[1]:Compare global grid average outputs(yearly)\n")
   cat("[2]:Compare monthly average of one pixel(p)\n")
   cat("[3]:Compare maps\n")
   cat("Method of comparasion=?")
   method<-readline()
   cat("Number of data to be compared=?")
   data.num<-as.numeric(readline())
   cat("List of output files:")
   print(list.files(path))
   cat("\n")
   datalist<-array(NA,dim=data.num)
   dim_init<-3
   
   #set data variables and find the lowest dimension
   for(i in 1:data.num){
     cat(i,".","Name of output data=")
     datalist[i]<-readline()
     if(length(dim(eval(parse(text=datalist[i]))))<dim_init)
       dim_init<-length(dim(eval(parse(text=datalist[i]))))
  }  
   
   cat("start year=?")
   syear<-as.numeric(readline())
   cat("end year=?")
   eyear<-as.numeric(readline())

 
  #set all data as same dimension
  for(i in 1:data.num){
    if(length(dim(eval(parse(text=datalist[i]))))!=dim_init){
       cat(datalist[i],":Convert the monthly data to yearly data?")
       if(readline()=="y"){
         assign(datalist[i],month2year(eval(parse(text=datalist[i]))))
       }
       else{
         cat("ERROR:Dimension must agree")
         stop()
      }
    }
  }
     

  plot_colors <- c(rgb(r=0.0,g=0.0,b=0.9), "red", "forestgreen") 

  if(method==1){
    if(dim_init==2){
      time<-c(syear:eyear)
      par(mfrow=c(data.num,1))
      for(i in 1:data.num){
         plot.data<-global.area.mean(eval(parse(text=datalist[i])))
         plot(time,plot.data[(syear-simstartyear+1):(eyear-simstartyear+1)],"l")
      }
    }
    if(dim_init==3){
      time<-as.vector(t(replicate(12,syear:eyear)))
      par(mfrow=c(data.num,1))
      for(i in 1:data.num){
         plot.data<-as.vector(global.area.mean(eval(parse(text=datalist[i]))))
         plot(plot.data[c(((syear-simstartyear)*12+1):((eyear-simstartyear+1)*12))])
      }
    }
  }
} 
  

