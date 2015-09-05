library(ncdf)
source("../read.input.r")
source("../map.r")

new.var.ncdf<-function(ncfile, lpjgrid_path, var_name, units, 
                              time_start, time_interval, 
                              longname = var_name, missval = 1e32){
    # create an empty ncdf file with single variable. 
    # Args:
    #    lpjgrid: the path of LPJ grid
    #    var_name: variable name
    #    time_start: start year for yearly output, start month for monthly output. 
    #                e.g. "1900" and "1900-01-01"
    #    time_interval: "years" or "months" or "days"
    #    lonname: the description of the variable
    # TODO:
    #    change the nasty read.input.grid to a list()
    nvar <- length(var_name)
    stopifnot(length(units) == nvar)
    stopifnot(length(longname) == nvar)

    read.input.grid(lpjgrid_path)
    nclat <- seq(SOUTH,NORTH,RES)
    nclon <- seq(WEST,EAST,RES)
    # strings to for time
    tchar<-paste(time_interval, "since", time_start)


    londim <- dim.def.ncdf('Longitude',"deg_E",as.double(nclon)) 
    latdim <- dim.def.ncdf('Latitude',"deg_N",as.double(nclat))
    timedim <- dim.def.ncdf('Time', tchar, 0, unlim=TRUE)
    # define variable
    vardef<-list()
    for(i in 1:length(var_name)){
        vardef[[i]] <- var.def.ncdf(var_name[i], units[i], list(londim, latdim, timedim),
                           missval, longname[i], prec = "single")
    }
    
    cout<-create.ncdf(ncfile, vardef)
    att.put.ncdf(cout,"Longitude","axis","X")
    att.put.ncdf(cout,"Latitude","axis","Y")
    att.put.ncdf(cout,"Time","axis","T")

    return(list(nc=cout,vardef=vardef))
}



put.lpjvar.ncdf <- function(nc, varid, lpj_raster, start, count){
    xdim <- nc[["nc"]]$dim[["Longitude"]]$lene
    ydim <- nc[["nc"]]$dim[["Latitude"]]$lene
    put.var.ncdf(nc[["nc"]], varid,
                 vals = as.vector(lpj_raster),
                 start = c(1, 1, start),
                 count = c(xdim, ydim, count))

}











