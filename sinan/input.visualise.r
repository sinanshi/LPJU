##############
#
##############

source("header")
  require(fields)
  require(maps)


path.in<-"../../NelasInputs/OUTPUT/"
#path.in<-"/home/sinan/OT-Med/Hydrology/bins/"
path.imout<-"../../NelasInputs/OUTPUT/visual/"
clmbands<-c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
read.input.grid(path.in)





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
       #text(-20,37,year)
       par(op)
       dev.off()
     }
}
       
COLS3 <- colorRampPalette(c("darkgreen","chartreuse","lemonchiffon","goldenrod1","goldenrod2","goldenrod3","goldenrod"))
COLS2 <- colorRampPalette(c("grey93","lightcyan","skyblue","blue","navy"))
COLS <- colorRampPalette(c("navy","blue","skyblue","grey","orange","red","red4"))
#plot.clm("pre.clm",clmbands,2000,COLS2)
#plot.clm("tmp.clm",clmbands,2000,COLS)
 plot.clm("gpcc_cru09_prec_monthly_1901_2009.clm",clmbands,2000,COLS2)
#plot.clm("tmx.clm",clmbands,2000,COLS)
#plot.clm("elevation.bin",clmbands,1901,COLS3)

#plot.clm("elevation_6342p_h43.bin",clmbands,1901,COLS3)
#plot.clm("cru_ts_3_10.1901.2009.tmp_6342p_h43.clm",clmbands,2000,COLS)#otmed/
 
 
 

