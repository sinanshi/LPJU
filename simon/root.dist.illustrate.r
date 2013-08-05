 #THIS IS A R ILLUSTRATION OF ROOT DISTRIBUTION
 #IT WAS COVERTED FROM FOLLOWING C CODE IN LPJmL2013/src/lpj/fscanpftpar.c
 #     fscanpftreal(isout,file,&pft->beta_root,pft->name,"beta_root");
 #     totalroots=1 - pow(pft->beta_root,layerbound[BOTTOMLAYER-1]/10);
 #     pft->rootdist[0]=(1 - pow(pft->beta_root,layerbound[0]/10))/totalroots;
 #     for(l=1;l<BOTTOMLAYER;l++)
 #       pft->rootdist[l]=(pow(pft->beta_root,layerbound[l-1]/10) - pow(pft->beta_root,layerbound[l]/10))/totalroots;  
 #   }
 
#200         /* soil depth layer (mm) */
#300         /* soil depth layer (mm) */
#500         /* soil depth layer (mm) */
#1000        /* soil depth layer (mm) */
#1000        /* soil depth layer (mm) */
#10000       /* soil depth layer (mm) */

#200         /* layerbound (mm) */
#500         /* layerbound (mm) */
#1000        /* layerbound (mm) */
#2000        /* layerbound (mm) */
#3000        /* layerbound (mm) */
#13000       /* layerbound (mm) */

#parameter beta_root is acquired from pft.par from LPJmL2013/par/
beta_root<-c(0.962,0.961,0.976,0.964,0.966,0.943,0.943,0.943,0.972,0.962,0.966,0.972,0.9690,0.9690,0.9690,0.9690,0.9690,0.9690,0.9690,0.9690,0.9690,0.9690,0.9690,0.9690)

#24 pft in pft.par
pft<-c("tropical broadleaved evergreen tree",
"tropical broadleaved raingreen tree",
"temperate needleleaved evergreen tree",
"temperate broadleaved evergreen tree",
"temperate broadleaved summergreen tree",
"boreal needleleaved evergreen tree",
"boreal broadleaved summergreen tree",
"C3 perennial grass",
"C4 perennial grass",
"bioenergy tropical tree",
"bioenergy temperate tree",
"bioenergy C4 grass",
"temperate cereals",
"rice",
"maize",
"tropical cereals",
"pulses",
"temperate roots",
"tropical roots"  ,      
"oil crops sunflower",
"oil crops soybean",
"oil crops groundnut",
"oil crops rapeseed",
"sugarcane")

#pft which is using here:because for all crops, the beta_root is the same.
pft_using<-c("1. tropical broadleaved evergreen tree",
"2. tropical broadleaved raingreen tree",
"3. temperate needleleaved evergreen tree",
"4. temperate broadleaved evergreen tree",
"5. temperate broadleaved summergreen tree",
"6. boreal needleleaved evergreen tree",
"7. boreal broadleaved summergreen tree",
"8. C3 perennial grass",
"9. C4 perennial grass",
"10. bioenergy tropical tree",
"11. bioenergy temperate tree",
"12. bioenergy C4 grass",
"13. CROPS")

BOTTOMLAYER<-6

layerbound<-c(200,500,1000,2000,3000,13000)
total_roots<-1-beta_root^(layerbound[BOTTOMLAYER-1]/10)
rootdist<-array(NA,dim=c(BOTTOMLAYER,length(beta_root)))
rootdist[1,]<-(1-beta_root^(layerbound[1]/10))/total_roots


for(l in 2:BOTTOMLAYER){
 rootdist[l,]<-((beta_root^(layerbound[l-1]/10)-beta_root^(layerbound[l]/10)))/total_roots
 }
 
 x11()
 plot(x=c(1:BOTTOMLAYER),y=rootdist[,1],"l",xlab="layer",ylab="",ylim=range(0:7)/10,xlim=range(1:6),col=1)
 title("Root Layer Distribution")
 for(i in 2:6)
   lines(x=c(1:BOTTOMLAYER),y=rootdist[,i],col=i,lty=1,lwd=3)
   
 for(i in 7:12)
   lines(x=c(1:BOTTOMLAYER),y=rootdist[,i],col=i,lty=2,lwd=3)
   lines(x=c(1:BOTTOMLAYER),y=rootdist[,13],col=13,lty=4,lwd=3)
   legend(x=3,y=0.7,col=c(1:13),lty=c(1,1,1,1,1,1,2,2,2,2,2,2,3),legend=pft_using)
  
  x11()
  par(mfrow=c(3,2))
  for(i in 1:BOTTOMLAYER)
  barplot(rootdist[i,c(1:13)],names.arg=c(1:13),xlab=paste("Layer",i))
  
  

  

  

  