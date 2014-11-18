library("ncdf")
npix<-67420
nyear<-109
cru<-open.ncdf("~/OT-Med/Climate/cru_ts_3_10.1901.2009.vap.dat.nc")


output<-file("prec.clm","wb")
writeChar("LPJCLIM",output,eos=NULL)		#header name
     writeBin(as.integer(1),output,size=4) 		#header version
     writeBin(as.integer(1),output,size=4)		#order
     writeBin(as.integer(1901),output,size=4)		#firstyear
     writeBin(as.integer(nyear),output,size=4)		#nyear
     writeBin(as.integer(0),output,size=4)		#firstcell
     writeBin(as.integer(npix),output,size=4)		#ncell
     writeBin(as.integer(12),output,size=4)		#nbands
     writeBin(as.numeric(0.5),output,size=4) #cellsize
     writeBin(as.numeric(0.1),output,size=4) #scalar
     


# for(mois in 1:12){
# 	cat("mois=",mois,"\n")
# 	vap_out<-array(NA,c(npix,nyear))
# 	month<-seq(mois,nyear*12,12)
# 	for(i in 1:109){
# 		vap_map<-as.integer(get.var.ncdf(cru,"vap",start=c(1,1,month[i]),count=c(720,360,1))*10)
# 		vap_mask<-vap_map[which(is.na(vap_map)==FALSE)]
# 		vap_out[,i]<-vap_mask
# 		cat(i,"-->",month[i],"\n")
# 	}
# 	out_v<-as.vector(t(vap_out))
# 	writeBin(out_v, output,size=2)
# }
	
	for(year in 1:109){
		start_month<-((year-1)*12)+1
		month<-seq(start_month,start_month+11,1)
		vap_out<-array(NA, c(npix,12))
		print(year)
		print(month)
		for(m in 1:12){
			vap_map<-as.integer(get.var.ncdf(cru,"vap",start=c(1,1,month[m]),count=c(720,360,1))*10)
			vap_mask<-vap_map[which(is.na(vap_map)==FALSE)]
			vap_out[,m]<-vap_mask
			}
		out_v<-as.vector(t(vap_out))
        	writeBin(out_v, output,size=2)
		
		}
		
			
	
	close(output)
	