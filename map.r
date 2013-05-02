######################
#input map name and grid name
#and then give the map values
######################

####################################
#create the map data for display
#data.raw<-the output data you would like to use,e.g. mnpp.data.out
#startyear<-the year you want to display,[simstartyear,simendyear] (e.g. [1901,2006])
#endyear<-[simstartyear,simendyear]
#Notice: Extracting map for a single year, set both startyear and endyear the same value 
####################################
require(tcltk)
library(tkrplot)
map.create<-function(data.raw,startyear,endyear,type){

 startyear<-startyear-simstartyear+1
 endyear<-endyear-simstartyear+1

 if(startyear<0){
  print("map.create: the chosen start year should be later than simulation start year.")
  stop()
 }


 if(type=="y"){# if yearly data
 map.data<-array(NA,dim=c(NCOLS,NROWS,simyears))   #correct carlibration
  for(y in startyear:endyear){
    for(i in 1:npixel.out)
      map.data[ind_lon[i],ind_lat[i],y] <- data.raw[i,y]
    
   }
 }


 if(type=="m"){# if monthly data
 map.data<-array(NA,dim=c(NCOLS,NROWS,12,simyears))   #correct carlibration
  for(y in startyear:endyear){
   for(m in 1:12)  {
    for(i in 1:npixel.out)
      map.data[ind_lon[i],ind_lat[i],m,y] <- data.raw[i,m,y]
    } 
  }
 }

map.data
}

map.show<-function(map.data,y,m){
  
 yg.palette <- colorRampPalette(c("yellow3","yellow","greenyellow","green","green3","darkgreen"))
 colo=yg.palette(101)
 colo.cm = cm.colors(32)
 image(x=seq(-19.75,49.75,len=140),y=seq(25.25,49.75,len=50),map.data[,,m,1],col=colo,xlab="",ylab="",axes=T)
  map(add=T,boundary=T)
}



map.interact<-function(map.data,startyear,endyear,type,colour){
 
 startyear<-startyear-simstartyear+1
 endyear<-endyear-simstartyear+1
#####################################
#####################################
 yhscale <- 1.5    # Horizontal scaling
 Myvscale <- 1.5    # Vertical scaling

 tt <- tktoplevel()
 tkwm.title(tt,date)

 CopyToClip <- function(){
   png(file="myplot.png", bg="white")
   #display a map
   dev.off()
}

 copy.but <- tkbutton(tt,text="Copy to Clipboard",command=CopyToClip)

 tkgrid(copy.but)
 

 switch.year<<-startyear
 switch.month<<-1
 ############################
 #
 #########################
 map.switch<-function(xlim=NULL, ylim=NULL, xaxs="r", yaxs="r"){
   
   
   
   map.show(map.data,switch.year,switch.month)   
   devset <- function()
      if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)


   keydown <- function(key) {
             if (key == "q") return(invisible(1))
             eventEnv$onMouseMove <- NULL
             if(key=="6"){
               if(switch.month==12){
                  switch.year<<-switch.year+1
                  switch.month<<-1
               }
               else{
               switch.month<<-switch.month+1
               }
               map.show(map.data,switch.year,switch.month)
               Sys.sleep(0.1) 
               print(switch.month)
              } 
            if(key=="4"){
              plot(rnorm(1000))
              Sys.sleep(0.1)
            }
            NULL 
        }

    setGraphicsEventHandlers(prompt="Click and drag, hit q to quit",
                          onKeybd = keydown)
    eventEnv <- getGraphicsEventEnv()


}


     X11(type = "Xlib")
     map.switch(startyear,endyear)
     # This currently only works on the Windows
     # and X11(type = "Xlib") screen devices...
     getGraphicsEvent()

}

