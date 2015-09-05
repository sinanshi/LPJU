source("array3d2ncdf.r")
source("../read.output.r")

lpjoutput2ncdf <- function(lpjoutput)
{
    stopifnot(length(lpjoutput[["var_name"]]) == lpjoutput[["nbands"]])
    if(length(lpjoutput[["var_name"]]))
        IS_MULTI_VAR <- TRUE
    nc_single_var <- 
        new.var.ncdf(lpjoutput[["ncfile"]],
                     lpjoutput[["lpjgrid"]],
                     lpjoutput[["var_name"]],
                     lpjoutput[["units"]],
                     lpjoutput[["time_start"]],
                     lpjoutput[["time_interval"]],
                     lpjoutput[["longname"]],
                     lpjoutput[["missval"]])
    read.input.grid(lpjoutput[["lpjgrid"]])
    end_year <- lpjoutput[["start_year"]] + lpjoutput[["nyears"]] - 1
    count_ncdf_band <- 1

    for(this_year in lpjoutput[["start_year"]]:end_year){
        for(this_band in 1:lpjoutput[["nbands"]]){
            this_data <- read.output.yearband(lpjoutput[["filename"]], 
                                              this_year, this_band, 
                                              lpjoutput[["start_year"]], lpjoutput[["ncells"]], 
                                              lpjoutput[["nyears"]], lpjoutput[["nbands"]])
            this_map <- map.build(this_data)
            if(IS_MULTI_VAR)
                put.lpjvar.ncdf(nc_single_var, lpjoutput[["var_name"]][this_band], this_map, start = count_ncdf_band, count = 1)
            else{
                put.lpjvar.ncdf(nc_single_var, lpjoutput[["var_name"]], this_map, start = count_ncdf_band, count = 1)
                count_ncdf_band <- count_ncdf_band + 1
            }
            if(IS_MULTI_VAR)
                count_ncdf_band <- count_ncdf_band + 1
        }
    }
    close.ncdf(nc_single_var[["nc"]])
}

#not usefule
cft_lpjoutput2ncdf <- function(lpjoutput)
{
    nc_single_var <- 
        new.var.ncdf(lpjoutput[["ncfile"]],
                     lpjoutput[["lpjgrid"]],
                     lpjoutput[["var_name"]],
                     lpjoutput[["units"]],
                     lpjoutput[["time_start"]],
                     lpjoutput[["time_interval"]],
                     lpjoutput[["longname"]],
                     lpjoutput[["missval"]])
    read.input.grid(lpjoutput[["lpjgrid"]])
    end_year <- lpjoutput[["start_year"]] + lpjoutput[["nyears"]] - 1
    count_ncdf_band <- 1
    for(this_year in lpjoutput[["start_year"]]:end_year){
        for(this_band in 1:lpjoutput[["nbands"]]){

            this_data <- read.output.yearband(lpjoutput[["filename"]], 
                                              this_year, this_band, 
                                              lpjoutput[["start_year"]], lpjoutput[["ncells"]], 
                                              lpjoutput[["nyears"]], lpjoutput[["nbands"]])
            this_map <- map.build(this_data)
            put.lpjvar.ncdf(nc_single_var, lpjoutput[["var_name"]][this_band], this_map, start = count_ncdf_band, count = 1)
        }
        count_ncdf_band <- count_ncdf_band + 1

    }
    close.ncdf(nc_single_var[["nc"]])
}


lpjoutput<-list(
                filename = "~/Desktop/NelaData/out_2a_tr_1861-1900/cftfrac.bin",
                lpjgrid = "grid.bin",
                ncfile = "tempd.nc",
                var_name = c("wheat","rice"),
                start_year = 1900,
                nyears = 40, 
                ncells = 67420,
                nbands = 2, # yearly data set 1
                units = c("degC","frac"),
                time_start = 1901,
                time_interval = "years",
                longname = c("surface temperature","fuck"),
                missval = 1e32)

#lpjoutput2ncdf(lpjoutput)
lpjoutput2ncdf(lpjoutput)
nc<-open.ncdf("tempd.nc")
var<-get.var.ncdf(nc,"wheat")


