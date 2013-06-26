#============================
#Data manupilation
#============================






#============================
#plot data on same map
#
#============================
 query<-function(path){
   py<-get.output.info(path)
   npixel<-py[1]
   year<-py[2]
   label.month<<-c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
   g.o<-readline("Global data (g) or One pixel data(o)?")

#Global data manipulation
 if(g.o=="g"){
   p.m<-readline("plot(p) or map(m)=?")

#Monthly global data plot
    if(p.m=="p"){
     cat("List of Methods\n")
     cat("[1]:Compare global grid average outputs\n")
     cat("[2]:UNDER DEVELOPMENT\n")
     method<-readline("Method of comparasion=?")
     data.num<-as.numeric(readline("Number of data to be compared=?"))
     cat("List of output files:")
     print(list.files(path))
     cat("\n")
     datalist<-array(NA,dim=data.num)
     dim_init<-3
   
     #set data variables and find the lowest dimension
     for(i in 1:data.num){
       datalist[i]<-readline(paste(i,".","Name of output data=",sep=""))
       if(length(dim(eval(parse(text=datalist[i]))))<dim_init)
         dim_init<-length(dim(eval(parse(text=datalist[i]))))
    }  
   
     
     syear<-as.numeric(readline("start year=?"))
     eyear<-as.numeric(readline("end year=?"))

 
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
    plot.data.seperate(dim_init,data.num,syear,eyear,datalist) 
  }
     
 
 }
  
  #Global data map
  if(p.m=="m"){
  map.data.name<-readline("Data you need=?")
  syear<-as.numeric(readline("start year=?"))
  eyear<-as.numeric(readline("end year=?"))
  
  map.data<-map.create(eval(parse(text=map.data.name)),syear,eyear)
  map.interact(map.data,syear,eyear,data.info=vars.check(map.data.name))

 
  
  }  
}

  if(g.o=="o"){
    cat("List of Comparing Methods(UNDER DEVELOPMENT)\n")
    cat("[1]:Compare daily of one pixel(p)\n")
    cat("[2]:Compare monthly of one pixel data\n")
    method<-readline("Method of comparasion=?")
    data.num<-as.numeric(readline("Number of data to be compared=?"))
    cat("List of output files:")
    print(list.files(path))
    cat("\n")
    datalist<-array(NA,dim=data.num)
  } 
  
}
