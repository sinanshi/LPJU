source("array3d2ncdf.r")
source("lpj2ncdf.r")


gridpath <- "~/workspace/OT-Med/LPJU/data/grid_global.bin"
output_csv<-read.csv("output_cft.csv")
time_interval <- "years"

crop_names<-c("temperate_cereals", "rice","maize","tropical_cereals","pulses" ,"temperate_roots" ,"tropical_roots", 
"sunflower","soybean" ,"groundnuts" ,"rapeseed","sugarcane" ,"others" ,"managed_grasslands",
"bio_energy_grass","bio_energy_tree")
lpj_band<- c(paste("rainfed_",crop_names,sep=""),paste("irrigated_",crop_names,sep=""))

band_names<-list()
band_names[[1]]<-lpj_band
band_names[[2]]<-lpj_band
band_names[[3]]<-lpj_band



run_nc <- function(){
    nfiles<-length(output_csv$File)
    for(i in 1:nfiles){
        filename <- paste(inpath,output_csv$File[i],".bin",sep="")
        units <- as.vector(output_csv$unit)[i]
        outfile <- paste(outpath, output_csv$Name.of.outfile[i], sep = "")
        nbands <- output_csv$nbands[i]
        units <- rep(units, nbands)
        var_name <- band_names[[i]]



        lpjoutput<-list(
                        filename = filename,
                        lpjgrid = gridpath,
                        ncfile = outfile,
                        var_name = var_name, 
                        start_year = start_year,
                        nyears = nyears, 
                        ncells = 67420,
                        nbands = nbands, # yearly data set 1
                        units = units,
                        time_start = time_start,
                        time_interval = time_interval,
                        time_dim = nyears,
                        longname = var_name,
                        missval = 1e32)

        cat("\n----------------\n")
        cat("writing ",lpjoutput[["filename"]], "-->", lpjoutput[["ncfile"]],"\n")
        cat("var:",lpjoutput[["var_name"]], "start_year:", lpjoutput[["start_year"]],
            "time_dim:",lpjoutput[["time_dim"]],"nyears:",lpjoutput[["nyears"]],
            "bands:",lpjoutput[["nbands"]],"\n")

        lpjoutput2ncdf(lpjoutput)
    }
}





#years 2
#2a
start_year <- 1861 # year should be one year less
time_start <- "1861"
nyears <- 40

inpath <- "~/Desktop/NelaData/out_2a_tr_1861-1900/"
outpath <- "2a_tr_1861-1900/"
run_nc()
#2b
inpath <- "~/Desktop/NelaData/out_2b_tr_1861-1900/"
outpath <- "2b_tr_1861-1900/"
run_nc()
#month 3
start_year <- 1901
time_start <- "1901"
nyears <- 2013-1901+1
inpath <- "~/Desktop/NelaData/out_3a_tr_1901-2013/"
outpath <- "3a_tr_1901-2013/"
run_nc()

#monthly 3b
inpath <- "~/Desktop/NelaData/out_3b_tr_1901-2013/"
outpath <- "3b_tr_1901-2013/"
run_nc()

