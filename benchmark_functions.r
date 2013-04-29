#-----------------------------------------
#fuctions
#-----------------------------------------





global.mean<-function(data,npixel,sim.years){

 data.mean<-array(0,dim=sim.years)
 for(j in 1:simyears){
   data.mean[j]<-mean(data[((j-1)*npixel+1):(j*npixel)])
}
 data.mean
}



