##############
#
##############

source("header")
  require(fields)
  require(maps)


path.in<-"../../NelasInputs/OUTPUT/"
filename<-"pre.clm"
path.imout<-"../../NelasInputs/OUTPUT/visual/"
clmbands<-c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
read.input.grid(path.in)
EAST<-max(lon)
SOUTH<-min(lat)
WEST<-min(lon)
NORTH<-max(lat)
RES<-0.5
NCOLS<<-(NORTH-SOUTH)/RES+1
NROWS<<-(EAST-WEST)/RES+1
Longitude<-seq(WEST,EAST,RES)
Latitude<-seq(SOUTH,NORTH,RES)




ind_lon<-as.integer(lon*2)-as.integer(min(lon[1])*2)+1
ind_lat<-as.integer(lat*2)-as.integer(min(lat)*2)+1


#############
#convert 1 dimentional
#raw data to a map. raw[NPIX]
#############
map.build<-function(raw){
map<-array(NA, c(NROWS, NCOLS))
for(i in 1:length(raw))
    map[ind_lon[i],ind_lat[i]]<-raw[i]
    return(map)
}
   
   
plot.clm<-function(name,bandnames,year,cols){
    cat(paste("visualising",name,"\n"))
    filename<-paste(path.in,name,sep="")
    header<-read.input.header(filename)
    raw<-read.input.files(filename,data.size=2)#2 indicates the size short
    check.year<-year-header$firstyear+1
    for(i in 1:header$nbands){
       
       
       map<-map.build(raw[i,check.year,])
       bitmap(paste(path.imout,name,"_",i,".jpeg",sep=""),type="jpeg",onefile=FALSE,
                        height=5,width=12,pointsize=24,res=300)  
       op <- par(mar=c(0,0,0,0),mgp=c(0,0,0))
       image(x=Longitude,y=Latitude,map,xlim=c(-40,49.25),col=cols(100))
       image.plot(x=Longitude, y=Latitude, map,xlim=c(-40.00,49.25), axes=FALSE, col=cols(100),
               legend.only=FALSE, legend.shrink=0.5)
       map(add=T)
       text(-30,37,paste(name, bandnames[i]))
       text(-20,37,year)
       par(op)
       dev.off()
     }
}
       
COLS3 <- colorRampPalette(c("darkgreen","chartreuse","lemonchiffon","goldenrod1","goldenrod2","goldenrod3","goldenrod"))
COLS2 <- colorRampPalette(c("grey93","lightcyan","skyblue","blue","navy"))
COLS <- colorRampPalette(c("navy","blue","skyblue","grey","orange","red","red4"))
#plot.clm("pre.clm",clmbands,2000,COLS2)
# plot.clm("tmp.clm",clmbands,2000,COLS)
# plot.clm("tmn.clm",clmbands,2000,COLS)
# plot.clm("tmx.clm",clmbands,2000,COLS)
plot.clm("elevation.bin",clmbands,1901,COLS3)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
# filecontry<-"ListCountries.dat"
#  filegrid<-c("grid.bin")
#  filesoil<-c("soil_new_67420.bin")
#  filecow<-c("cow_mg_2006.bin","cow_mg_2006_full.bin")
#  fileclim6<-c("tmp.clm","pre.clm","cld.clm","wet.clm","tmx.clm","tmn.clm")
#  fileclim9<-c("cru_ts_3_10.1901.2009.cld.clm","cru_ts_3_10.1901.2009.dtr.clm","cru_ts_3_10.1901.2009.pre.clm","cru_ts_3_10.1901.2009.tmn.clm","cru_ts_3_10.1901.2009.tmp.clm","cru_ts_3_10.1901.2009.tmx.clm","cru_ts_3_10.1901.2009.wet.clm")
#  fileclimera<-c("lwnet_erainterim_1901-2010.clm","swdown_erainterim_1901-2010.clm",
#   "gpcc_cru09_prec_monthly_1901_2009.clm","gpcc_cru09_wet_monthly_1901_2009.clm")
#  filelu<-c("cft1700_2005_16cfts_SR.bin", "cft1700_2005_32bands.bin","cft1700_2005_bioenergy_sc.bin")
#  filesdate<-c("sdate_combined.clm", "sdate_combined_filled.clm")
#  filedrain<-c("drainage.bin")
#  filepopdens<-c("popdens_1901_2003.clm","popdens_HYDE3_1901_2007_bi.clm","popdens_HYDE_1901_2010_bi.clm")
#  fileelev<-c("elevation.bin")
#  filenei<-c("neighb_irrigation.bin")
#  filewuse<-c("wateruse_1900_2000.bin","wateruse_1900_2100_A2.bin","wateruse_1900_2100_B1.bin")
#  filelakes<-c("lakes.bin","lakeswithoutreservoirs.bin")
#  fileres<-c("reservoir_info_grand5.bin")

