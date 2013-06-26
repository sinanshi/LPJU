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



