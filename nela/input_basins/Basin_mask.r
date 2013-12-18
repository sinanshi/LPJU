#=====================
#basin_grid_mask.r
#Generate binary file "grid_basin" with selected basin grids.
#1) Select all basins inside the window, with longitude=(-18.80,45.50), latitude=(18.50,51.50).
#2) Remove basins that are not located in the selecting country list:
#      Albania 1, Algeria 2 ,Bosnia_and_Herzegovina 16, Bulgaria 19, Croatia 35, Cyprus 37, Egypt 43, France 54, Greece 63, 
#      Israel 78, Italy 79, Lebanon 91, Libya 94, Macedonia 97, Montenegro 109, Morocco 110 ,Portugal 133 ,Serbia 142,
#      Slovenia 145,,Spain 151,Syria 160,Tunisia 168,Turkey 169
#
#grid_basin: data type: integer, 4 byte.
#                      lon[1], lon[2], ...lon[n], lat[1], lat[2], ...lat[n]
#        Sinan Shi 18/12/2013
#=====================


 
 #=====================
#To run this R code you have to:
#1. sepcify loc.inputr=location of input original "grid.bin", "cow.bin"
#                    loc.inputbasins=location of basin data, can be derived from "make_basin.c"
#                    loc.outputr=location of where to put grid_basin and visualised basin map. 
#2. include  src/read.input.r, you can find it in the Git repository.
#3. specify the number of pixel of global grid
#
#=====================
rm(list=ls(all=TRUE))
require(fields)
#-------------
#you may include either this file or copy the functions here. 
#-------------
source("/home/sinan/workspace/LPJ_Utilities/src/read.input.r")
#-------------
#Locations
#-------------
loc.inputr <- paste(getwd(),"/",sep="")
#loc.inputr <- "/home/mfader/_R/InputR/"
loc.inputbasins<-paste(getwd(),"/",sep="")
#loc.inputbasins<-"/home/mfader/_R/Postprocessing/in/"
loc.outputr <- paste(getwd(),"/",sep="")
#loc.outputr <- "/home/mfader/_R/Postprocessing/out/"
#-------------
#Pixel number of global map
#-------------
npixel<-67420

    

#read cow
read.cow<-function(){
    zz <- file("cow_mg_2006_full.bin","rb")
    seek(zz,43, origin = "start")
    data.read<-readBin(zz,integer(),n=npixel*2,size=2)
    cow<-data.read[seq(1,npixel*2,2)]
    close(zz)
    return(cow)
}  

read.gbasin.asc<-function(){
       zz <- file(paste(loc.inputbasins,"basin_new_asc.dat",sep=""),"rb")
       data<-readBin(zz,integer(),npixel,size=4)
       data<-data+1
       close(zz)
       return(data)
}

#############
#convert 1 dimentional
#raw data to a map. raw[NPIX]
#############
map.build<-function(raw_){
map<-array(NA, dim=c(NR,NC))
for(i in 1:length(raw_))
    map[ind_lon[i],ind_lat[i]]<-raw_[i]
    return(map)
}

#==========
#Select all basins inside the window
#Longitude=(-18.80,45.50)
#Latitude    =(18.50,51.50)
#==========
window<-function(){
    map.w<-array(0,c(NR,NC))#result
    index.w<<-array(0,max(data))
    
    longitude.bound<-which(x>-18.0&x<45.50)
    latitude.bound<-which(y>18.50&y<51.50)
    
    for(i in min(longitude.bound):max(longitude.bound)){
      for(j in min(latitude.bound):max(latitude.bound)){
          if(is.na(map[i,j])==FALSE){
             index.w[map[i,j]]<<-1
         }
       }
    }
         
         
    
    for(i in 1:NR){
       for(j in 1:NC){
           if(is.na(map[i,j])==FALSE){
               if(index.w[map[i,j]]==1){
                  map.w[i,j]<-map[i,j]
               }
            }
         }
    }
    return(map.w)
}
               
 #==========
#Remove basins that are not located in the selecting country list
#==========             
  window.country<-function(){
    data.w<-window()
    cow<-read.cow()
    map.cow<-map.build(cow)
    
    for(i in which(index.w==1)){
        
           this.cow<-map.cow[which(data.w==i,arr.ind=2)]
           if(any(this.cow==24)){#cameroon, to exclude one basin across Algeria, Niger
               data.w[which(data.w==i,arr.ind=2)]<-0
           }else{
           #print(this.cow)
             if(any(this.cow==1) #Albania 1
                  ||any(this.cow==2) #Algeria 2
                  ||any(this.cow==16)#Bosnia_and_Herzegovina 16
                  ||any(this.cow==19)#Bulgaria 19
                  ||any(this.cow==35)#Croatia 35
                  ||any(this.cow==37)#Cyprus 37
                  ||any(this.cow==43)#Egypt 43
                  ||any(this.cow==54)#France 54
                  ||any(this.cow==63)#Greece 63
                  ||any(this.cow==78)#Israel 78
                  ||any(this.cow==79)#Italy 79
                  ||any(this.cow==91)#Lebanon 91
                  ||any(this.cow==94)#Libya 94
                  ||any(this.cow==97)#Macedonia 97
                  ||any(this.cow==109)#Montenegro 109
                  ||any(this.cow==110)#Morocco 110
                  ||any(this.cow==133)#Portugal 133
                  ||any(this.cow==142)#Serbia 142
                  ||any(this.cow==145)#Slovenia 145
                  ||any(this.cow==151)#Spain 151
                  ||any(this.cow==160)#Syria 160
                  ||any(this.cow==168)#Tunisia 168
                  ||any(this.cow==169)#Turkey 169
             )
             next
           else{
                data.w[which(data.w==i,arr.ind=2)]<-0
             }
        
         }
     }
          
            return(data.w)
}            

##########
#start main function
##########
 x=seq(-179.75,179.75,0.5)
 y=seq(-55.75,83.75,0.5)
 mycol<-rep(rainbow(100),500)
read.input.grid("/home/sinan/workspace/NelasInputs/")
cow<-read.cow()
data<-read.gbasin.asc()
map<-map.build(data)
map.window<-window()


map.show<-map.window
map.show[which(map.show==0)]<-NA
bitmap(paste("BASIN_WINDOW",".jpeg",sep=""),type="jpeg",onefile=FALSE,height=5,width=12,pointsize=24,res=300)
op <- par(mar=c(0,0,0,0),mgp=c(0,0,0))  
image(x,y,map.show,col=mycol,xlim=c(-20,60),ylim=c(-15,70))
map(add=T)
rect(-18.00, +18.50,+45.5,+51.50)
par(op)
dev.off()


map.wc<-window.country()
map.show<-map.wc
map.show[which(map.show==0)]<-NA
bitmap(paste("BASIN_WINDOW_COUNTRY",".jpeg",sep=""),type="jpeg",onefile=FALSE,height=5,width=12,pointsize=24,res=300)
op <- par(mar=c(0,0,0,0),mgp=c(0,0,0))  
image(x,y,map.show,col=mycol,xlim=c(-20,60),ylim=c(-15,70))
map(add=T)
rect(-18.00, +18.50,+45.5,+51.50)
par(op)
dev.off()


lon.out<-x[which(map.wc!=0,arr.ind=2)[,1]]
lat.out<-y[which(map.wc!=0,arr.ind=2)[,2]]

a<-lon.out[order(as.data.frame(lon.out))]
b<-lat.out[order(as.data.frame(lon.out))]

lonlat<-c(a,b)
lonlat<-as.integer(lonlat*100)

zz<-file("grid_basin","wb")
writeBin(lonlat,zz,size=4)

close(zz)
