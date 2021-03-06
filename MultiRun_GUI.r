

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
             grid<-lonlat2grid(lonlat.array[i,1],lonlat.array[i,2])-1
             system2("./monorunconf.sh",args=grid)  
        }
        #case 2: if the soil.par has been modified
        if(any(theParameter.array=="Soil.Par")){
            arg<-array(NA,dim=2)
            arg[1]<-paste(current.name,"soilpar",i,sep="")#name of temp soil par
            arg[2]<-"../--LPJmL2013/par/soil_new.par"
            system2("cp",args=arg)
       }    
       #run LPJmL!!
       system("./runonepixel.sh 001")
#--------------------------------
#The following piece of code is exactely the same as lpjml.dailyrun in benchmark_functions
#read one pixel run data
#--------------------------------
       daily.temp<-read.daily.output(path.mono)
       daily.temp$ID<-rep(i,length(daily.temp[,1]))
       
       monthly.temp<-read.monthly.output(path.mono)
       monthly.temp$ID<-rep(i,length(monthly.temp[,1]))
      
     
       yearly.temp<-read.yearly.output(path.mono)
       yearly.temp$ID<-rep(i,length(yearly.temp[,1])) #give ID label to identify the data of runs
       if(i==1){
           daily.frame<-daily.temp
           yearly.frame<-yearly.temp
           monthly.frame<-monthly.temp
       }
       else{
           daily.frame<-rbind(daily.frame,daily.temp)
           yearly.frame<-rbind(yearly.frame,yearly.temp)
           monthly.frame<-rbind(monthly.frame,monthly.temp)
      }
    } 
   
   #realign data into a good form    
   daily.frame$row <- with(daily.frame, ave(ID==ID, ID, FUN = cumsum))
   m <- melt(daily.frame, id.vars = c("row", "ID"))
   daily.frame.out <<- acast(m, row ~ variable ~ ID)
   
   
   #realign data in a good form    
   monthly.frame$row <- with(monthly.frame, ave(ID==ID, ID, FUN = cumsum))
   m <- melt(monthly.frame, id.vars = c("row", "ID"))
   monthly.frame.out <<- acast(m, row ~ variable ~ ID)
     

   #realign data in a good form    
   yearly.frame$row <- with(yearly.frame, ave(ID==ID, ID, FUN = cumsum))
   m <- melt(yearly.frame, id.vars = c("row", "ID"))
   yearly.frame.out <<- acast(m, row ~ variable ~ ID)
     
 
    
    cat("done!\n")
 
}

#--------------------------------------------------
#The function for the "Open Soil par" button:create soil.par_i with three configurations
#--------------------------------------------------
soilpar.config<-function(){
  sys.args<-array(NA,dim=2) #the arguments for copying soil.par file used by system2
  sys.args[1]<-"../--LPJmL2013/par/soil_new.par"#directory of original soil.par
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
#--------------------------------------------------
#This function for making restart file
#--------------------------------------------------
make.restart<-function(){
         #define configuration variables
     lonlat.array<<-array(NA,dim=c(runs,2))
     for(i in 1:runs){
         #case 1: if the location has been modified
         if(any(theParameter.array=="Location")){
             lonlat.array[i,1]<<-tclvalue(get(paste("tcl.lon",i,sep=""))) #coor.array is a global array for storing lon,lat with configurations
             lonlat.array[i,2]<<-tclvalue(get(paste("tcl.lat",i,sep="")))
             grid<-lonlat2grid(lonlat.array[i,1],lonlat.array[i,2])-1
             system2("./monorunconf.sh",args=grid)  
        }
        #case 2: if the soil.par has been modified
        if(any(theParameter.array=="Soil.Par")){
            arg<-array(NA,dim=2)
            arg[1]<-paste(current.name,"soilpar",i,sep="")#name of temp soil par
            arg[2]<-"../--LPJmL2013/par/soil_new.par"
            system2("cp",args=arg)
       }    
       #run LPJmL!!
       system("./runonepixel.sh 110")
       cat("Restart files have been created.\n")
       cat("Please be very careful that the configurations of restart files are the same as the running configurations.")
         
    }
}

  #make the directory according the current time, all the results will be
  #stored in VisualResults/
  current.day<-format(Sys.time(), "%Y-%b-%d")
  current.time<-format(Sys.time(), "%H:%M:%S")
  current.name<<-paste("../VisualResults/",current.day,"[",current.time,"]","/",sep="")
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
   make.restart.but<-tkbutton(tt.cw,text="Make Restart Files",command=make.restart)
   plot.but<-tkbutton(tt.cw,text="Plot Daily Data",
                      command=function(){
                        x11()
                        daily.plot()
                     })

   store.image<-function(){ #for store button
     png(paste(current.name,"/results",sep=""))
     daily.plot()
     dev.off()
     cat("Result Graph Storation: Done!")
   }

   store.but<-tkbutton(tt.cw,text="Store Image",
                       command=function(){
                         png(paste(current.name,"/results",sep=""))
                         daily.plot()
                         dev.off()
                         cat("Result Graph Storation: Done!")
                       })
   tkgrid(make.restart.but,row=3,column=3)
   tkgrid(run.but,row=4,column=3)
   tkgrid(plot.but,row=5,column=3)   
   tkgrid(store.but,row=6,column=3)
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



