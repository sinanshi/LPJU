source("array3d2ncdf.r")
source("lpj2ncdf.r")


gridpath <- "~/workspace/OT-Med/LPJU/data/grid_global.bin"

run_nc <- function(){
    nfiles<-length(output_csv$File)
    for(i in 1:nfiles){
        filename <- paste(inpath,output_csv$File[i],".bin",sep="")
        units <- as.vector(output_csv$unit)[i]
        outfile <- paste(outpath, output_csv$Name.of.outfile[i], sep = "")
        var_name <- as.vector(output_csv$var.name)[i]


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
                        time_dim = nbands * nyears,
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



inputs<-"/home/mfader/_AndereProjekte/Trendy4/submit/outputs/"
output_csv<-read.csv("output_yearly.csv")
time_interval <- "years"

#years 2
#2a
start_year <- 1861 # year should be one year less
time_start <- "1861"
nyears <- 40
nbands <- 1

inpath <- paste(inputs,"out_2a_tr_1861-1900/",sep="")
outpath <- "2a_tr_1861-1900/"
run_nc()
#2b
inpath <- paste(inputs,"out_2b_tr_1861-1900/",sep="")
outpath <- "2b_tr_1861-1900/"
run_nc()
#month 3
start_year <- 1901
time_start <- "1901"
nyears <- 2013-1901+1
inpath <- paste(inputs,"out_3a_tr_1901-2013/",sep="")
outpath <- "3a_tr_1901-2013/"
run_nc()

#monthly 3b
inpath <- paste(inputs,"out_3b_tr_1901-2013/",sep="")
outpath <- "3b_tr_1901-2013/"
run_nc()


#--------------
#months
#-------------

output_csv<-read.csv("output_monthly.csv")
time_interval <- "months"

#month 2
#2a
start_year <- 1861
time_start <- "1861-1-15"
nyears <- 40
nbands <- 12

inpath <- paste(inputs,"out_2a_tr_1861-1900/",sep="")
outpath <- "2a_tr_1861-1900/"
run_nc()
#2b
inpath <- paste(inputs,"out_2b_tr_1861-1900/",sep="")
outpath <- "2b_tr_1861-1900/"
run_nc()
#month 3
start_year <- 1901
time_start <- "1901-1-15"
nyears <- 2013-1901+1
inpath <- paste(inputs,"out_3a_tr_1901-2013/",sep="")
outpath <- "3a_tr_1901-2013/"
run_nc()

#monthly 3b
inpath <- paste(inputs,"out_3b_tr_1901-2013/", sep="")
outpath <- "3b_tr_1901-2013/"
run_nc()







