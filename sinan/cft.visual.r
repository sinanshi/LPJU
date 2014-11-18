 
library("fields")
library("maps")
#path of cft
#path.in<-"/home/sinan/workspace/input_downscale/LU_half2quarter/CFT/"
path.in<-"~/workspace/input_downscale/"
#path of output image
path.imout<-""
clmbands<-c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")

 cft.name <- c("rainfed_wheat", "rainfed_rice", "rainfed_maize", "rainfed_millet", "rainfed lentils", "rainfed sugarbeet", "rainfed yam", "rainfed sunflower", "rainfed soybean", "rainfed groundnuts",
 "rainfed rapeseed", "rainfed sugarcane", "rainfed others",  "rainfed mangrass", "rainfed energytree", "rainfed energygrass",
 "irri wheat", "irri rice", "irri maize", "irri millet", "irri lentils", "irri sugarbeet", "irri yam", "irri sunflower", "irri soybean",
 "irri groundnuts", "irri rapeseed", "irri sugarcane", "irri others", "irri mangrass", "irri energytree", "irri energygrass")

cft.name52<-c("Temperate_Cereals(r)","Rice(r)","Maize(r)","Tropical_Cereals(r)","Pulses(r)","Temperate_Roots(r)","Potatoes(r)","Tropical_Roots(r)","Sunflower(r)",
  "Soybeans(r)","Groundnuts(r)","Rapeseed(r)","Sugar_Cane(r)","Citrus(r)",
  "Non_Citrus_Orchards(r)","Date_Palm(r)","Olives(r)","Nuts_Trees(r)",
  "Grapes(r)","Vegetables(r)","Cotton(r)","Fodder_grass(r)","Others(r)",
  "Manage_Grasslands(r)","Bioenergy_Grass(r)","Bioenergy_Tree(r)",
  
"Temperate_Cereals(i)","Rice(i)","Maize(i)","Tropical_Cereals(i)","Pulses(i)",
"Temperate_Roots(i)","Potatoes(i)","Tropical_Roots(i)","Sunflower(i)",
  "Soybeans(i)","Groundnuts(i)","Rapeseed(i)","Sugar_Cane(i)","Citrus(i)",
  "Non_Citrus_Orchards(i)","Date_Palm(i)","Olives(i)","Nuts_Trees(i)",
  "Grapes(i)","Vegetables(i)","Cotton(i)","Fodder_grass(i)","Others(i)",
  "Manage_Grasslands(i)","Bioenergy_Grass(i)","Bioenergy_Tree")
COLS3 <- colorRampPalette(c("darkgreen","chartreuse","lemonchiffon","goldenrod1","goldenrod2","goldenrod3","goldenrod"))
HEADER_SIZE<-43
#--------------------------
# To read input header
read.input.header<-function(filename){
    file.in<-file(filename,"rb")
    
    seek(file.in,7, origin = "start")
    version<-readBin(file.in,integer(),n=1,size=4)
    order<-readBin(file.in,integer(),n=1,size=4)
    firstyear<-readBin(file.in,integer(),n=1,size=4)
    nyears<-readBin(file.in,integer(),n=1,size=4)
    firstcell<-readBin(file.in,integer(),n=1,size=4)   
    ncells<-readBin(file.in,integer(),n=1,size=4)
    nbands<-readBin(file.in,integer(),n=1,size=4)
    cellsize<-readBin(file.in,numeric(),n=1,size=4)
    scalar<-readBin(file.in,numeric(),n=1,size=4)
    header<-data.frame(version,order,firstyear,nyears,firstcell,ncells,nbands,cellsize,scalar)
    close(file.in)
    return(header)
     
}
#--------------------------
# To read input grid
read.input.grid<-function(path.in){
     input.list<-dir(path.in)
     grid.name<-paste(path.in,input.list[grep("grid",input.list)],sep="")
     grid.header<-read.input.header(grid.name)
     
     prec<-abs(log(grid.header$scalar)/log(10))
     gridfile<- file(grid.name,"rb")
     seek(gridfile,HEADER_SIZE, origin = "start")
    grid.temp<-readBin(gridfile,integer(),n=2*grid.header$ncells,size=2)
    grid.data<<-round(grid.temp,digits=0)*grid.header$scalar
    lon<<-grid.data[c(1:grid.header$ncells)*2-1]
    lat<<-grid.data[c(1:grid.header$ncells)*2]
    EAST<<-round(max(lon),prec)
    SOUTH<<-round(min(lat),prec)
    WEST<<-round(min(lon),prec)
    NORTH<<-round(max(lat),prec)
    RES<<-grid.header$cellsize
    NC<<-(NORTH-SOUTH)/RES+1
    NR<<-(EAST-WEST)/RES+1

    
    ind_lon<<-ceiling(lon/RES-min(lon)/RES+1)
    ind_lat<<-ceiling(lat/RES-min(lat)/RES+1)
    
    close(gridfile)
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

#--------------------------
# To read input
 read.input.yearband<-function(filename,data.size,year,band){#year,band, start from 1 
	 fileHeader<-read.input.header(filename)
	 data.year<-year-fileHeader$firstyear
	 file.in <- file(sprintf(filename),"rb")
	 data.in<-array(NA,dim=c(fileHeader$ncells))
	 seek(file.in,where=HEADER_SIZE+data.size*(data.year*fileHeader$nband*fileHeader$ncells+(band-1)),origin="start")
	 for(i in 1:fileHeader$ncells){
		   data.in[i]<-readBin(file.in, integer(), n=1, size=2)*fileHeader$scalar
		   seek(file.in,where=(fileHeader$nbands-1)*2,origin="current")
         }
	 close(file.in)
	 return(data.in)
	 
}
#--------------------------
# To read input
plot.cft<-function(name,year,cols,bandnames,data.size){
    cat(paste("visualising",name,"\n"))
    filename<-paste(path.in,name,sep="")
    header<-read.input.header(filename)
    
    for(i in 1:header$nband){
	    cat(paste("reading band",i,"-",bandnames[i],"...","\n"))
 	    raw<-read.input.yearband(filename,data.size,year,i)#2 indicates the size short
 	    map<-map.build(raw)
     
    bitmap(paste(path.imout,i,"_",year,"_",bandnames[i],".jpeg",sep=""),type="jpeg",onefile=FALSE,
	   height=5,width=12,pointsize=24,res=300)  
     
      op <- par(mar=c(0,0,0,0),mgp=c(0,0,0))
      image(x=Longitude,y=Latitude,map,col=cols(100))
       image.plot(x=Longitude, y=Latitude, map, axes=TRUE, col=cols(100),
               legend.only=FALSE, legend.shrink=0.5)
       map(add=T)
       text(20,-45,paste(year, bandnames[i]))
       par(op)
       dev.off()
    }
}



#main function       
read.input.grid(path.in)
Longitude<<-seq(WEST,EAST,RES)
Latitude<<-seq(SOUTH,NORTH,RES)       
plot.cft("cft1700_2010_new.bin",year=1700,col=COLS3,bandnames=cft.name52,data.size=2)