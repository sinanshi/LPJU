########################
#To run one pixel of LPJmL model
#lpjdir:                      [vector] specifying lpj location of each run. 
#pixelnum:              [vector] of pixel number for each run.
#conffiles:                [matrix] listed multiple parameter files address for each run. 
#                                 (conffiles->conffile.direction)
#                                 i.e. location of parameter files to substitude for each run. 
#                                 The alignment of conffiles: 
#                                 run1        run2         run3
#                                 file1          file1            file3
#                                 file2          file2            file4
#                                   ....              ....             ....
#conffile.drection: [matrix] listed corresponding changing direction of conffiles in LPJmL model
#                                 alignment the same as conffiles
#dir.out                     [vector] directory to put outputs (optional) 
#outsuffix                [vector] of suffix added on each output file(optional)                                 
########################


onepix.config.run<-function(lpjdir, pixelnum, conffiles, conffile.direction, dir.out="0",outsuffix=0){
    
    if(dim(conffile.direction)[2]!=dim(conffiles)[2])   stop("Configuration file number should be the same as configuration directory number")
    file.num<-dim(conffiles)[2] 
     run.num<-length(lpjdir)
    #check if the length of arguments are the same
    if(length(pixelnum)!=run.num)  stop("The argument pixelnum should equal to number of runs.")
    if(dim(conffiles)[1]!=run.num)  stop("The argument confiles should equal to number of runs.")
    if(dim(conffile.direction)[1]!=run.num)  stop("The argument conffile.direction should equal to number of runs.")
    if(dir.out[1]=="0"){
            default.folder<-1
    } else{
        default.folder<-0
        if(length(dir.out)!=run.num)  stop("The argument dir.out should equal to number of runs.")
    }
    if(outsuffix==0){
        default.suffix<-1
    }else{
        if(length(outsuffix!=run.num))  stop("The argument outsuffix should  be equal to number of runs.")
        default.suffix<-0
    }

    
    
    for(i in 1:run.num){
        #----------------------
        #create output directory
        #----------------------
        if(default.folder==1){
            current.day<-format(Sys.time(), "%Y-%b-%d")
            current.time<-format(Sys.time(), "%H:%M:%S")
            current.folder<-paste("../VisualResults/",current.day,"[",current.time,"]","/",sep="")
        }
         if(default.folder==0){
            
               current.folder<-paste(dir.out[i],sep="")
        }
        system2("mkdir",args=current.folder)
        #--------------------------
        #copy parameter files
        #--------------------------
        par_dir<-paste(current.folder,"parmeter_files/",sep="")
        system2("mkdir",args=par_dir) #make parameter directory
        for(j in 1:file.num){
            farg<-array(NA,2)
            farg[1]<-conffiles[i, j]
            farg[2]<-conffile.direction[i,j]            
            system2("cp",args=farg)
            cat(paste(conffiles[i,j],"->",conffile.direction[i,j],"\n"))
            farg[2]<-conffile.direction[i,j]
            farg[2]<-par_dir
            system2("cp",args=farg) #make one copy to output folder
            
         }
        #--------------------------
        #configure  pixel number in lpjml.conf
        #--------------------------   
        parg<-array(NA,2)
        parg[1]<-pixelnum[i] #pixel number
        parg[2]<-lpjdir[i]          #lpj directory
        system2("./runconfpix.sh",args=parg)
        cat(paste("pixel number changed to",pixelnum[i],"\n"))
        parg[1]<-paste(lpjdir[i],"lpjml.conf",sep="")
        parg[2]<-paste(par_dir,"lpjml_",i,"_p",pixelnum[i],".conf",sep="")#make a copy of lpjml.conf to output folder
        system2("cp", parg)
        #--------------------------
        #run LPJmL
        #--------------------------
        rarg<-array(NA,3)
        rarg[1]<-"111" #to run 3 times for creating 2 restart files with landuse and without and 1 run with restart file1
                                        #to modify, check the description on runconf.sh
        rarg[2]<-lpjdir[i] #lpj directory
        rarg[3]<-paste(par_dir,i,sep="")
        system2("./runconf.sh",args=rarg)
        
        #--------------------------
        #Copy output files
        #--------------------------
        lpj.output.path<-paste(lpjdir[i],"output/",sep="")
        outputfiles<-dir(lpj.output.path)
        outputname<-gsub(".bin","",outputfiles)
        if(default.suffix==1){
            outsuffix<-rep(paste("p",pixelnum[i],sep=""),length(outputname))
        }
        newoutputname<-paste(outputname,"_",outsuffix,".bin",sep="") #put suffix
        
        for(c in 1:length(outputfiles)){
            carg<-array(NA,2)
            carg[1]<-paste(lpj.output.path,outputfiles[c],sep="")
            carg[2]<-paste(current.folder,newoutputname[c],sep="")
            system2("cp", args=carg)
        }
   }
}

##################################
#read outputs 
#outpath: (variable) e.g. outpath="LPjmL2013/output"
#filename: (array)     e.g. filename=c("mnpp","d_temp","vegc")
#                                     if you have multiple files in one directory, i.e. mnpp_p2500, mnpp_p5000
#                                     then you have to specify them in the filename as c("mnpp_p2500")
#return a list:        to access like this
#                               list.run.a<-read.runoutput(outpath,filename)
#                               list.run.a$month$mnpp
#                               list.run.a$day$d_temp
##################################

read.runoutput<-function(outpath,filename){#filename without .bin
    
    index<-seq(1,length(filename))
    d<-grep("d_",filename)
    m<-grep("^m",filename)
    y<-setdiff(index,c(d,m))
    real.name<-dir(outpath)
   
    
   if(length(d)!=0){
        start=1
        for(i in d){
            real.name.d<-real.name[grep(filename[i],real.name)]#exact name in output directory
            daily.file<-paste(outpath,real.name.d,sep="")
            dfile.size<-file.info(daily.file)$size
            dfn<-file(daily.file,"rb")
            cat("Reading daily output:", daily.file,"\n")
            temp<-readBin(dfn,double(),dfile.size,size=4)
            if(start==1) {
                temp.ddata<-data.frame(temp)
                start<-0
                }
            else  temp.ddata<-data.frame(temp.ddata,temp)
            closeAllConnections()
        }
        colnames(temp.ddata)<-filename[d]
        daily.data.frame<-temp.ddata
   }
    
    
    if(length(m)!=0){

        for(i in m){
            real.name.m<-real.name[grep(filename[i],real.name)]#exact name in output directory
            month.file<-paste(outpath,real.name.m,sep="")
            mfile.size<-file.info(month.file)$size
            mfn<-file(month.file,"rb")
            cat("Reading monthly output:", month.file,"\n")
            temp<-readBin(mfn,double(),mfile.size,size=4)
            if(i==1) temp.mdata<-data.frame(temp)
            else  temp.mdata<-data.frame(temp.mdata,temp)
            closeAllConnections()
        }
        colnames(temp.mdata)<-filename[m]
        monthly.data.frame<-temp.mdata
   }
    
    if(length(y!=0)){
        start=1
        for(i in y){
            real.name.y<-real.name[grep(filename[i],real.name)]#exact name in output directory
            yearly.file<-paste(outpath,real.name.y,sep="")
            yfile.size<-file.info(yearly.file)$size
            yfn<-file(yearly.file,"rb")
            cat("Reading yearly output:", yearly.file,"\n")
            temp<-readBin(yfn,double(),yfile.size,size=4)
            if(start==1) {
                temp.ydata<-data.frame(temp)
                start<-0
                }
            else  temp.ydata<-data.frame(temp.ydata,temp)
            closeAllConnections()
        }
        colnames(temp.ydata)<-filename[y]
        yearly.data.frame<-temp.ydata
   }
       datalist<-list("day"=daily.data.frame,"month"=monthly.data.frame,"year"="yearly.data.frame")
       return(datalist)
}
 
 
 
 
 
 
 #####################
# running examples
# # ###################
##source("header") #not needed in running this scrpit but in seperate file is neccessary

lpjdir<-c("/home/sinan/workspace/LPJ_Utilities/--LPJmL2013/",
                     "/home/sinan/workspace/LPJ_Utilities/--LPJmL2013/",
                     "/home/sinan/workspace/LPJ_Utilities/0_LPJmL.in.progress/")


pixelnum<-c(2000,100,2100)

conff1<-c("sinan/bla1","sinan/bla2")
conff2<-c("sinan/par1","sinan/par2")
conff3<-c("sinan/par1","sinan/par2")
conffiles<-rbind(conff1,conff2,conff3)


conffile.dir1<-c("/home/sinan/workspace/LPJ_Utilities/--LPJmL2013/bla1", "/home/sinan/workspace/LPJ_Utilities/--LPJmL2013/bla2")
conffile.dir2<-c("/home/sinan/workspace/LPJ_Utilities/--LPJmL2013/bla1", "/home/sinan/workspace/LPJ_Utilities/--LPJmL2013/bla2")
conffile.dir3<-c("/home/sinan/workspace/LPJ_Utilities/0_LPJmL.in.progress/bla1", "/home/sinan/workspace/LPJ_Utilities/0_LPJmL.in.progress/bla2")
conffile.direction<-rbind(conffile.dir1,conffile.dir2,conffile.dir3)

dir.out<-c("../VisualResults/a/","../VisualResults/b/","../VisualResults/c/")
onepix.config.run(lpjdir=lpjdir,pixelnum=pixelnum,conffiles=conffiles,conffile.direction=conffile.direction,dir.out=dir.out)

#Example read.runoutput
filename=c("mnpp","d_temp","vegc")
list1<-read.runoutput(dir.out[1],filename)
list2<-read.runoutput(dir.out[2],filename)
list3<-read.runoutput(dir.out[3],filename)


plot(xlim=c(1,365),ylim=c(-15,40),c(1:365),list1$day$d_temp[1:365],"l",col=1)
lines(c(1:365),list2$day$d_temp[1:365],"l",col=2)
lines(c(1:365),list3$day$d_temp[1:365],"l",col=3)




