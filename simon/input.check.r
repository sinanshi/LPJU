 source("header")

 read<-1 #read data (1) or not (0)


 path.in<-"../inputs/OTMed_window_6089p_2013/"
 
 
 
 
 
 cft.name <- c("rainfed wheat", "rainfed rice", "rainfed maize", "rainfed millet", "rainfed lentils", "rainfed sugarbeet", "rainfed yam", "rainfed sunflower", "rainfed soybean", "rainfed groundnuts",
 "rainfed rapeseed",
 "rainfed sugarcane",
 "rainfed others", 
 "rainfed mangrass",
 "rainfed energytree",
 "rainfed energygrass",
 "irri wheat",
 "irri rice",
 "irri maize",
 "irri millet",
 "irri lentils",
 "irri sugarbeet",
 "irri yam",
 "irri sunflower",
 "irri soybean",
 "irri groundnuts",
 "irri rapeseed",
 "irri sugarcane",
 "irri others",
 "irri mangrass",
 "irri energytree",
 "irri energygrass")
 
 
 
 
 
 
 
 
 
 
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
     cat(cft.name[i],":",cft1[year-cftstartyear+1,grid_,i],"      ")
     if(i%%2==0)  cat("\n")
  }
 }
   
   

 
