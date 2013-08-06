

require(tcltk)
parameter<- c("Location","Soil.Par","parameter a","parameter b")



#-----------------------------------
#Pop out configuration windows
#-----------------------------------
configure.windows<-function(){
#----------------------------------------------------
#The function of the "RUN" button: to store the 
#parameters and write it into a configuration file
#for each configuration and run LPJmL. 
#---------------------------------------------------
config.done<-function(){
#define configuration variables
lonlat.array<<-array(NA,dim=c(runs,2))

 for(i in 1:runs){
   #case 1: if the location has been modified
   if(any(theParameter.array=="Location")){
      lonlat.array[i,1]<<-tclvalue(get(paste("tcl.lon",i,sep=""))) #coor.array is a global array for storing lon,lat with configurations
      lonlat.array[i,2]<<-tclvalue(get(paste("tcl.lat",i,sep="")))
      grid<-lonlat2grid(lonlat.array[i,1],lonlat.array[i,2])
      system2("./monorunconf.sh",args=grid)  
  

   }
   #case 2: if the soil.par has been modified
   if(any(theParameter.array=="Soil.Par")){
      arg<-array(NA,dim=2)
      arg[1]<-paste(current.name,"soilpar",i,sep="")#name of temp soil par
      arg[2]<-"../LPJmL2013/par/soil.par"
      system2("cp",args=arg)
   }    

     #run LPJmL!!
     system("./runonepixel.sh")
     # system("./../LPJmL2013/bin/lpjml ../LPJmL2013/lpjml.conf")
     # system("./../LPJmL2013/bin/lpjml -DFROM_RESTART ../LPJmL2013/lpjml.conf")

     #The following piece of code is exactely the same as lpjml.dailyrun in benchmark_functions
     #read one pixel run data
     daily.temp<-read.daily.output(path.mono)
     daily.temp$ID<-rep(i,length(daily.temp[,1]))
     if(i==1){
        daily.frame<-daily.temp
     }
     else{
       daily.frame<-rbind(daily.frame,daily.temp)
     }
    }
    #realign data into a good form    
    daily.frame$row <- with(daily.frame, ave(ID==ID, ID, FUN = cumsum))
    m <- melt(daily.frame, id.vars = c("row", "ID"))
    daily.frame.out <<- acast(m, row ~ variable ~ ID)
    cat("done!\n")
 
}

#--------------------------------------------------
#The function for the "Open Soil par" button:create soil.par_i with three configurations
#--------------------------------------------------
soilpar.config<-function(){
  sys.args<-array(NA,dim=2) #the arguments for copying soil.par file used by system2
  sys.args[1]<-"../LPJmL2013/par/soil.par"#directory of original soil.par
  for(i in 1:runs){
    soil.par.tempname<-paste(current.name,"soilpar",i,sep="")#name of temp soil par
    sys.args[2]<-soil.par.tempname
    #copy soil.par from LPJmL and create temprory files 
    #such as soil.par.temp[i] for each run
    system2("cp",args=sys.args)
    system2("kwrite",args=soil.par.tempname)
    different<-system2("diff",args=sys.args)
    if(different==0){
      decision<-readline("The soil par you configured is identical to the original configuration file.\nAre you sure you have saved the new configuration file properly?[y/n]")
     
    if(decision=="n")
       system2("kwrite",args=soil.par.tempname)
  }
 }
}



  #make the directory according the current time, all the results will be
  #stored in VisualResults/
  current.day<-format(Sys.time(), "%Y-%b-%d")
  current.time<-format(Sys.time(), "%H:%M:%S")
  current.name<-paste("../VisualResults/",current.day,"[",current.time,"]","/",sep="")
  system2("mkdir",args=current.name)
  tt.cw <- tktoplevel()

  tkgrid(tklabel(tt.cw,text="PARMETERS CONFIGURATIONS"))

  #case: if the location has been modified
  if(any(theParameter.array=="Location")){
    tkgrid(tklabel(tt.cw,text="====Configure Location===="))
    for(i in 1:runs){
        assign(paste("tcl.lon",i,sep=""),tclVar(2.25),envir = .GlobalEnv)#the temporory variable for tcltk text entry
        assign(paste("tcl.lat",i,sep=""),tclVar(48.25),envir = .GlobalEnv)
        lon.entry<-tkentry(tt.cw,textvariable=get(paste("tcl.lon",i,sep="")))
        lat.entry<-tkentry(tt.cw,textvariable=get(paste("tcl.lat",i,sep="")))
        tkgrid(tklabel(tt.cw,text=""))
        tkgrid(tklabel(tt.cw,text="Longitude"),tklabel(tt.cw,text="Latitude"))
        tkgrid(lon.entry,lat.entry)
        tkfocus(tt.cw)
    }
 }

  #case: if the soil par has been modified
  if(any(theParameter.array=="Soil.Par")){
    cat("Soil_Par Modifing...\n")
    soil.conf.but<-tkbutton(tt.cw,text="Open Soil.par file",command=soilpar.config)
    tkgrid(soil.conf.but)  
  }

   run.but<-tkbutton(tt.cw,text="RUN LPJmL",command=config.done)
   tkgrid(run.but)
   

tkfocus(tt.cw)

}



#-----------------------------
#MAIN: The initial window 
#-----------------------------

#configure botton: call the configure windows
multirun.gui<-function(){
configure.runs<-function(){
   theParameter.array<<-parameter[as.numeric(tkcurselection(tl))+1]
   runs<<-tclvalue(tclruns)
   tkdestroy(tt)
   configure.windows()
}


#setup
tclruns<- tclVar(1)#the temporory variable for tcltk text entry
tt <- tktoplevel(width=700,height=800)
tl<-tklistbox(tt,height=length(parameter),selectmode="multiple",background="white")
for (i in (1:length(parameter))){
    tkinsert(tl,"end",parameter[i])
}

runs.entry<-tkentry(tt,textvariable=tclruns)
configure.but<-tkbutton(tt,text="Configure",command=configure.runs)

#arrange widget
tkgrid(tklabel(tt,text="Parameters for LPJ RUN (one pixel)"))
tkgrid(runs.entry,tklabel(tt,text="Runs"),sticky='we')
tkgrid(tl)
tkgrid(configure.but,sticky='we')
tkfocus(tt)
}



