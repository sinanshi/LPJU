library(lpjutil)
path<-"~/Desktop/NelaData/out_2a_tr_1861-1900/"
outpath<-"~/Desktop/NelaData/good_unit/out_2a_tr_1861-1900/"
unit_csv<-read.csv("unit_convert.csv")

nfile<-length(unit_csv$File)

lpjoutfile<-list()
lpjoutfile[["nyears"]]=40 # please manually setup this value
lpjoutfile[["ncells"]]=67420
lpjoutfile[["start_year"]]=1861 # and this too.

#configure done!
#=================================================

# the operation
op_convert<-function(data){
    return(data/1000)
}

# convert files from the csv
for(i in 1:nfile){
    lpjoutfile[["path"]]=paste(path,unit_csv$File[i],".bin", sep="")
    lpjoutfile[["nbands"]]=as.integer(unit_csv$nbands[i])
    newfile<-paste(outpath,unit_csv$File[i],".bin",sep="")
    
    # try ?output.operation to learn more. 
    output.operation(lpjoutfile, op_convert, newfile=newfile,VERBOSE=TRUE)
}


