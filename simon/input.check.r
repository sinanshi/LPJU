 source("header")

 read<-0 #read data (1) or not (0)


 path.in<-"../inputs/OTMed_window_6089p_2013/"
 
 
 
 
 
 cft.name <- c("rainfed wheat","irri wheat",
 "rainfed rice","irri rice",
 "rainfed maize","irri maize",
 "rainfed millet","irri millet",
 "rainfed lentils","irri lentils",
 "rainfed sugarbeet", "irri sugarbeet",
 "rainfed yam", "irri yam",
 "rainfed sunflower", "irri sunflower",
 "rainfed soybean", "irri soybean",
 "rainfed groundnuts","irri groundnuts",
 "rainfed rapeseed", "irri rapeseed",
 "rainfed sugarcane","irri sugarcane",
 "rainfed others", "irri others",
 "rainfed mangrass",  "irri mangrass",
 "rainfed energytree","irri energytree",
 "rainfed energygrass","irri energygrass")
 
 
 
 
 
 
 
 
 
 
 read.grid(path.out)
 #temporary use it with output to calibrate the grid. If the grid has been changed, one should change this line. 
 if(read==1){
  cft1<-read.input.cft(path.in)
  soil1<-read.input.soil(path.in)
 }

 check.input.info<-function(lon.i,lat.i,year){
   grid_<-lonlat2grid(lon.i,lat.i)
   cat("CHECKING RESULTS:\n")
   cat("=======================\n")
   cat("Longitude=",lon.i,"Latitude=",lat.i,"\n")
   cat("Grid=",grid_,"     Year=",year,"\n")
   cat("=======================\n")
   cat("Soil Parameter=",soil1[grid_],"\n")
   cat("=======================\n")
   cat("CFT\n")
   for(i in 1:32){
     cat(cft.name[i],":",cft1[i,grid_,year-cftstartyear],"      ")
     if(i%%2==0)  cat("\n")
  }
 }
   
   

 
