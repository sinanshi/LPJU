 read.input.grid<-function(){
   pixel_year<-get.output.info(path.out)
   npixel.out<-pixel_year[1]
   res=0.5
   grid.fn.out<- file(paste(path.input,"grid_OTMed_6089p_h43.bin",sep=""),"rb")
   grid.data<<-readBin(grid.fn.out,integer(),n=2*npixel.out,size=2)/100
   lon<<-grid.data[c(1:npixel.out)*2-1]
   lat<<-grid.data[c(1:npixel.out)*2]
   ind_lon<<- as.integer((grid.data[c(1:npixel.out)*2-1]+20)/res + 1.01)
   ind_lat<<- as.integer((grid.data[c(1:npixel.out)*2]-25)/res + 1.01)
   close(grid.fn.out)
}

#temporary read function for testing
read.input.tmp<-function(path.in){
  input.list<-dir(path.in)
  file.name<-paste(path.in,input.list[grep("tmp",input.list)],sep="")
  nyear<-(file.info(file.name)$size-input_header)/6089/2/12
  filetmp <- file(sprintf(file.name),"rb")
  tmp.in<<-array(NA,dim=c(12,nyear,6089))
  seek(filetmp,where=input_header,origin="start")
  for(i in 1:nyear){
    for(j in 1:6089){
      tmp.in[,i,j]<<-readBin(filetmp, integer(), n=12, size=2)/10
    }
  }
close(filetmp)
}

read.input.cft<-function(path.in){

  input.list<-dir(path.in)
  file.name<-paste(path.in,input.list[grep("cft",input.list)],sep="")
  nyear<-(file.info(file.name)$size-input_header)/6089/32/2
  
  ##check if the file
  if(nyear!=(cftendyear-cftstartyear+1))
    cat("ERROR: File size is not correct. Please reset cftstartyear and cftendyear.")
  else
    cat(paste("Reading",":",file.name,"..."))

  ##Read cft data
  filecft <- file(sprintf(file.name),"rb")
  seek(filecft,where=input_header,origin="start")
 
  cft.in<-array(NA,dim=c(32,6089,nyear))
  for(i in 1:32){
    for(j in 1:6089){
     cft.in[i,j,]<-readBin(filecft,integer(),n=nyear,size=2)    
    }
 }
 close(filecft)
 cft.in
}


read.input.soil<-function(path.in){
  input.list<-dir(path.in)
  file.name<-paste(path.in,input.list[grep("soil",input.list)],sep="")
  filesoil<-file(sprintf(file.name),"rb")
  soilpar<-readBin(filesoil,integer(),n=6089,size=1)
  
  soilpar
}

