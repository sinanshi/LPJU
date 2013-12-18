##############
#
##############

source("header")
  require(fields)
  require(maps)


#path.in<-"../../NelasInputs/OUTPUT/"
# path.in<-"/home/sinan/OT-Med/Hydrology/cut_output_5794/"
 path.in<-"../../NelasInputs/"
path.imout<-"../../NelasInputs/OUTPUT/visual/"
clmbands<-c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
read.input.grid(path.in)


 cft.name <- c("rainfed wheat", "rainfed rice", "rainfed maize", "rainfed millet", "rainfed lentils", "rainfed sugarbeet", "rainfed yam", "rainfed sunflower", "rainfed soybean", "rainfed groundnuts",
 "rainfed rapeseed", "rainfed sugarcane", "rainfed others",  "rainfed mangrass", "rainfed energytree", "rainfed energygrass",
 "irri wheat", "irri rice", "irri maize", "irri millet", "irri lentils", "irri sugarbeet", "irri yam", "irri sunflower", "irri soybean",
 "irri groundnuts", "irri rapeseed", "irri sugarcane", "irri others", "irri mangrass", "irri energytree", "irri energygrass")


Longitude<<-seq(WEST,EAST,RES)
Latitude<<-seq(SOUTH,NORTH,RES)


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
   
   
plot.clm<-function(name,bandnames,year,cols){
    cat(paste("visualising",name,"\n"))
    filename<-paste(path.in,name,sep="")
    header<-read.input.header(filename)
    raw<-read.input.files(filename,data.size=2)#2 indicates the size short
    if(header$nyear==1) {
        check.year<-1
     }else{
         check.year<-year-header$firstyear+1
    }
    for(i in 1:header$nbands){
           map<-map.build(raw[i,check.year,])

       bitmap(paste(path.imout,name,"_",i,".jpeg",sep=""),type="jpeg",onefile=FALSE,
                      height=5,width=12,pointsize=24,res=300)  
     
      op <- par(mar=c(0,0,0,0),mgp=c(0,0,0))
       image(x=Longitude,y=Latitude,map,xlim=c(-20,49.25),col=cols(100))
       image.plot(x=Longitude, y=Latitude, map,xlim=c(-20.00,49.25), axes=FALSE, col=cols(100),
               legend.only=FALSE, legend.shrink=0.5)
       map(add=T)
       text(-30,37,paste(name, bandnames[i]))
       text(-20,37,year)
       par(op)
       dev.off()
     }
}
       
plot.noheader<-function(name,cols,data.size){
    cat(paste("visualising",name,"\n"))
    filename<-paste(path.in,name,sep="")
    size<-file.info(filename)$size
    npix<-size/data.size
    file.in <- file(sprintf(filename),"rb")
    data.in<-array(NA,dim=c(npix))
    raw<-readBin(file.in, integer(), n=npix, size=data.size)
    map<-map.build(raw)
#     bitmap(paste(path.imout,name,".jpeg",sep=""),type="jpeg",onefile=FALSE,
#                       height=5,width=12,pointsize=24,res=300)  
#      
#      op <- par(mar=c(0,0,0,0),mgp=c(0,0,0))
       image(x=Longitude,y=Latitude,map,xlim=c(-20,49.25),col=cols(100))
       image.plot(x=Longitude, y=Latitude, map,xlim=c(-20.00,49.25), axes=FALSE, col=cols(100),
               legend.only=FALSE, legend.shrink=0.5)
       map(add=T)
#        par(op)
#        dev.off()
  
      close(file.in)
}
    
    
  #-------------------
  #plot basin: no header, long
  #-------------------
    plot.basin<-function(name,cols,data.size){
    cat(paste("visualising",name,"\n"))
    filename<-paste(path.in,name,sep="")
    size<-file.info(filename)$size
    npix<-size/data.size
    file.in <- file(sprintf(filename),"rb")
    data.in<-array(NA,dim=c(npix))
    raw<-readBin(file.in, integer(), n=npix, size=data.size)
    map<-map.build(raw)
#     bitmap(paste(path.imout,name,".jpeg",sep=""),type="jpeg",onefile=FALSE,
#                     height=5,width=12,pointsize=24,res=300)  
#      
#      op <- par(mar=c(0,0,0,0),mgp=c(0,0,0))
        image(x=Longitude,y=Latitude,map,col=cols)
     
     #image(x=Longitude,y=Latitude,map,xlim=c(-20,49.25),ylim=c(-10,50),col=cols)
       map(add=T)
       close(file.in)
#     par(op)
#     dev.off()
}
  #-------------------
  #plot drainage.bin
  #-------------------   
  plot.drain<-function(name){
       cat(paste("visualising",name,"\n"))
       filename<-paste(path.in,name,sep="")
       header<-read.input.header(filename)
       raw<-read.input.files(filename,data.size=4)#4 for long
       
       draingrid<-which(raw[1,1,]!=-1&raw[1,1,]!=-9)
       x1<-lon[draingrid]
       y1<-lat[draingrid]
       x2<-lon[raw[1,1,draingrid]+1]#+1 because in C, the grid index start from 0
       y2<-lat[raw[1,1,draingrid]+1]
#        
#     bitmap(paste(path.imout,name,".jpeg",sep=""),type="jpeg",onefile=FALSE,
#                      height=5,width=12,pointsize=24,res=300)  
#       op <- par(mar=c(0,0,0,0),mgp=c(0,0,0))
       plot(lon,lat,col=8)
       for(i in 1:length(draingrid)){
           lines(c(x1[i],x2[i]),c(y1[i],y2[i]),col=2)
       }
#       nodrain<-which(raw[1,1,]==-1|raw[1,1,]==-9)
#       points(lon[nodrain+1],lat[nodrain+1])
#       par(op)
#      dev.off()


 }
       
COLS3 <- colorRampPalette(c("darkgreen","chartreuse","lemonchiffon","goldenrod1","goldenrod2","goldenrod3","goldenrod"))
COLS2 <- colorRampPalette(c("grey93","lightcyan","skyblue","blue","navy"))
COLS <- colorRampPalette(c("navy","blue","skyblue","grey","orange","red","red4"))
# plot.clm("pre.clm",clmbands,2000,COLS2)
# plot.clm("tmp.clm",clmbands,2000,COLS)
# plot.clm("gpcc_cru09_prec_monthly_1901_2009.clm",clmbands,2000,COLS2)
# plot.clm("tmx.clm",clmbands,2000,COLS)
plot.clm("elevation.bin",clmbands,1901,COLS)
 #plot.noheader("lakes.bin",COLS2)
# plot.noheader("lakeswithoutreservoirs.bin",COLS2)
 #plot.clm("elevation",clmbands,1901,COLS3)
# plot.clm("cru_ts_3_10.1901.2009.tmp_6342p_h43.clm",clmbands,2000,COLS)#otmed/
#  mycol=rep(rainbow(100),6742)
#  plot.basin("global_basin.bin",mycol,4)
 #plot.drain("neighb_irrigation.bin")
#  plot.clm("cft1700_2005_16cfts_SR.bin",cft.name,2005, COLS3)
#  plot.clm("cft1700_2005_32bands.bin",cft.name,2005, COLS3)
#  plot.clm("cft1700_2005_bioenergy_sc.bin",cft.name,2005, COLS3)
