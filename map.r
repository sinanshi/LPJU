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
#library(tkrplot)
map.create<-function(data.raw,startyear,endyear){
 
 npixel.out<-get.output.info(path.out)[1] 
 simyears<-get.output.info(path.out)[2]
 startyear<-startyear-simstartyear+1
 endyear<-endyear-simstartyear+1
 
 if(startyear<0){
  print("map.create: the chosen start year should be later than simulation start year.")
  stop()
 }


 if(length(dim(data.raw))==2){# if yearly data
 map.data<-array(NA,dim=c(NCOLS,NROWS,simyears))   #correct carlibration
  for(y in startyear:endyear){
    for(i in 1:npixel.out)
      map.data[ind_lon[i],ind_lat[i],y] <- data.raw[i,y]
    
   }
 }


 if(length(dim(data.raw))==3){# if monthly data
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

map.show<-function(map.data,y,m,data.info){#data.info is created by vars.check()
  
 yg.palette <- colorRampPalette(c("yellow3","yellow","greenyellow","green","green3","darkgreen"))
 colo=yg.palette(101)
 colo.cm = cm.colors(32)
 #image configuration
 tit<-paste(data.info$des,"(",data.info$name,")")  
 if(length(dim(map.data))==4){
  ylab<-paste(paste(as.character(y+simstartyear-1),"-",as.character(m)))
  
  image(x=seq(west,east,len=((east-west+0.5)*2)),y=seq(south,north,len=((north-south+0.5)*2)),
             map.data[,,m,y],
             col=colo,
             xlab="",
             ylab=ylab,
             axes=T)
  map(add=T,boundary=T)
 title(main=tit)
   image.plot(x=seq(west,east,len=((east-west+0.5)*2)),y=seq(south,north,len=((north-south+0.5)*2)),
             map.data[,,m,y],
             col=colo,
             legend.only=T,
             horizontal=T,
             legend.args=list( text=data.info$unit,col="black", cex=1, side=1, line=0),
             legend.width=0.3)
 
}

 if(length(dim(map.data))==3){
  xlab<-as.character(y+simstartyear-1)
  image(x=seq(west,east,len=((east-west+0.5)*2)),y=seq(south,north,len=((north-south+0.5)*2)),map.data[,,y],col=colo,xlab=xlab,ylab="",axes=T)
  map(add=T,boundary=T)
  title(main=tit)
 }
}


 
############################################################
############################################################
map.switch<-function(map.data,startyear,endyear,xlim=NULL, ylim=NULL, xaxs="r", yaxs="r",data.info){
   
   map.show(map.data,switch.year,switch.month,data.info=data.info)   
   devset <- function()
      if (dev.cur() != eventEnv$which) dev.set(eventEnv$which)
   
      
      keydown <- function(key) {
             if (key == "q") return(invisible(1))
                 eventEnv$onMouseMove <- NULL
             if(key=="6"){
               #monthly output
               if(length(dim(map.data))==4){
                  if(switch.year==endyear&&switch.month==12){
                     switch.year<<-startyear
                     switch.month<<-1
                  }
                  else{
                     if(switch.month==12){
                        switch.year<<-switch.year+1
                        switch.month<<-1
                     }
                     else
                        switch.month<<-switch.month+1
                  }
               }
               #yearly output
               if(length(dim(map.data))==3){
                 if(switch.year==endyear) 
                    switch.year<<-startyear
                 else 
                    switch.year<<-switch.year+1      
               }
                map.show(map.data,switch.year,switch.month,data.info)
                Sys.sleep(0.1) 
             } 
             if(key=="4"){
               if(length(dim(map.data))==4){
                 if(switch.year==startyear&&switch.month==1){
                    switch.year<<-endyear
                    switch.month<<-12
                 }
                 else{
                    if(switch.month==1){
                       switch.year<<-switch.year-1
                       switch.month<<-12
                    }
                    else
                     switch.month<<-switch.month-1 
                }
               }
               if(length(dim(map.data))==3){
                 if(switch.year==startyear)
                  switch.year<<-endyear
                 else
                  switch.year<<-switch.year-1
               }
              map.show(map.data,switch.year,switch.month,data.info)
              Sys.sleep(0.1)
             
            NULL 
        }
     }
    setGraphicsEventHandlers(prompt="hit q to quit",
                             onKeybd = keydown)
    eventEnv <- getGraphicsEventEnv()


}




##################################
# Map.interact
##################################
map.interact<-function(map.data,startyear,endyear,colour,data.info){#colour not in use

 map.itc.data<<-array(NA,dim=dim(map.data))#Global data for interact map only
 map.itc.data<<-map.data
 
 startyear<-startyear-simstartyear+1
 endyear<-endyear-simstartyear+1

 switch.year<<-startyear
 switch.month<<-1

 yhscale <- 5.5    # Horizontal scaling
 Myvscale <- 5.5    # Vertical scaling

#widget functions

CopyToClip <- function(){
   png(file=paste(as.character(switch.year+simstartyear-1),"-",as.character(switch.month)), bg="white")
   map.show(map.itc.data,switch.year,switch.month,data.info=data.info)
   dev.off()
}

NewWin<- function(){
   X11(type = "Xlib")#X11, remove this line if the system is not X11
   map.show(map.itc.data,switch.year,switch.month,data.info=data.info)
}

NewSwitchWin<-function(){
  X11(type = "Xlib")#X11, remove this line if the system is not X11
  map.switch(map.itc.data,startyear,endyear,data.info=data.info)
  getGraphicsEvent()
}
     tt <- tktoplevel()
     tkwm.title(tt,"LPJmL Data")
     copy.but <- tkbutton(tt,text="Copy to Clipboard",command=CopyToClip)
     newwins.but <- tkbutton(tt,text="New Window(Static)",command=NewWin)
     newwind.but<-tkbutton(tt, text="New Window(Dynamic)",command=NewSwitchWin)
     monorun.but<-tkbutton(tt,text="Mono Pixel run",command=monop.graph)
     l.general<- tklabel(tt,text="You may choose the functions here")
     l.ym <- tklabel(tt, text="Year/Month")
     e.year<-tkentry(tt, width=5)
     e.month<-tkentry(tt, width=3)
     tkgrid(l.general)
     tkgrid(copy.but,sticky="w")
     tkgrid(newwins.but,sticky="w")
     tkgrid(newwind.but,sticky="w")
     tkgrid(monorun.but,sticky="w")
     tkgrid(l.ym,e.year,e.month)
     cat("for dynamic windows, press 'q' to quit")
   
     

     #X11(type = "Xlib")#X11, remove this line if the system is not X11
     #map.switch(map.data,startyear,endyear)
     # This currently only works on the Windows
     # and X11(type = "Xlib") screen devices...
     getGraphicsEvent()

}

