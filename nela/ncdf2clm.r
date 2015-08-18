#========================================
#convert ncdf climate data to LPJ inputs.
#Sinan Shi   18 Aug 2015
#========================================
library(ncdf)

#-----------------
#setting
#-----------------
CHECK<-TRUE
nc_filename<-"../data/cru_ts3.23.1901.2014.cld.dat.nc"
lpj_inname<-"../data/cld.clm"#name of result lpj data.
if(CHECK==TRUE){
    source("../read.input.r")
    source("../map.r")
    read.input.grid("../data/grid_global.bin")
}
 



#-----------------
#read data from ncdf
#-----------------
cat("reading ncdf...")
ncfile<-open.ncdf(nc_filename)
var_name<-ncfile$var[[1]]$name 
time<-get.var.ncdf(ncfile,"time")
var<-get.var.ncdf(ncfile,var_name)
nmonth<-length(time) # this is only valid in for monthly data

cat("[done]\n")

#-----------------
#setup LPJ header
#-----------------
scalar<-0.01
firstyear<-1900 #now we know it starts from 1900
nyear<-as.integer(nmonth/12)
npix<-length(which(!is.na(var[,,1])))
nbands<-12 #12 months
cellsize<-0.5

#----------
# convert the raster to LPJ format
#----------
lpjvar<-vector()
cat("converting raster to LPJ data:\n")
for(i in 1:nyear){
    var_this_year<-array(NA,c(npix,12))
    for(j in 1:12){
        this_month<-as.vector(t(var[,,(i-1)*12+j]))
        var_this_year[,j]<-this_month[!is.na(this_month)]
    }
    lpjvar<-c(lpjvar, as.vector(t(var_this_year)))
    cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b===>",
        round(i/nyear*100,digits=2),"%")
}
cat("\n")
lpjvar<-as.integer(round(lpjvar/scalar))




#--------
#writing LPJ output
#--------
cat("writting LPJ input",lpj_inname,"...")
output<-file(lpj_inname,"wb")
writeChar("LPJCLIM",output,eos=NULL)            #header name
writeBin(as.integer(1),output,size=4)              #header version
writeBin(as.integer(1),output,size=4)              #order
writeBin(as.integer(firstyear),output,size=4)           #firstyear
writeBin(as.integer(nyear),output,size=4)          #nyear
writeBin(as.integer(0),output,size=4)              #firstcell
writeBin(as.integer(npix),output,size=4)           #ncell
writeBin(as.integer(nbands),output,size=4)             #nbands
writeBin(as.numeric(cellsize),output,size=4) #cellsize
writeBin(as.numeric(scalar),output,size=4) #scalar
writeBin(lpjvar,output,size=2)
close(output)
cat("[done]\n")



#----------
#compare data in ncdf and lpj
#----------
if(CHECK==TRUE){
    cat("LPJmL input validation...")
    test_year<-1902
    test_month<-5
    lpj_check<-read.input.yearband(
                            filename=lpj_inname, band=test_month, 
                            data.size=2,year=test_year)#check second year jan
    nc_check<-var[,,(test_year-firstyear)*12+test_month]
    nc_check<-as.vector(t(nc_check))
    nc_check<-nc_check[!is.na(nc_check)]
    if(any(abs(nc_check-lpj_check)>1e-5)){
        cat("[FAILED]\n")
        stop()
    }
    else{
        cat("[Successful]\n")
    }


}















#-----------
# How to know the vaiable name, e.g. cld?
#-----------
num_var <- ncfile$nvar
# sure if you have multiple variable, i.e. num_var is larger than 1, 
# you have to use a loop to access. 


#----------------
# about time
#   time<-get.var.ncdf(ncfile, "time")
# check this start year is here
#   nc_start_year<-ncfile$var[[1]]$dim[[3]]$units 
#-----------------

