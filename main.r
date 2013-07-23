# lpj_benchmark.r
# source('~/Desktop/R/R_Sinan/lpj_benchmark.r')
#
#-------------------------------------------------------------------

  cat("Data Read: enter 'y' for reading data and 'n' for not reading data:")
  dataread<-readline()
  if(dataread=="y"){
    rm (list=ls(all=TRUE))
    dataread<-1
    cat("Reading all the LPJmL output data...\n")
    
  }
  
  #load required libraries
  require(fields)
  require(maps)

  #load settings
  source("header")
      
  
  #npixel.out <- file.info(grid.fn.out)$size/4
  #nyear.out <-  file.info(paste(path.out,"vegc.bin",sep=""))$size/sizeof.data/npixel.out
  date <- date()           # timestamp
  
  #read grid
  read.grid(path.out)
  
  #read output data
  if(dataread==1)
    read.output.all(path.out)
    
  #map.create()
