/****************************************************************
*                                           Cut.c--input cutting
* Cut global LPJmL inputs according to the mask given. The mask 
* contains longitude latitude informations of the pixel that will be 
* chosen in new inputs. It will create an index according to the mask 
* given, the index will have the length of global input pixel number, where
* the chosen pixel will be marked as 1 while non-chosen 0. 
* 
* Header Data Type: 
* struct headers:  
*                                    [title;version;order;firstyear;nyears;firstcell;ncells;
*                                     nbands;cellsize;scalar]
*                                     real: 
*                                     real will be altered manually, which can be switched
*                                     to INT, LONG, and SHORT corresponding to the data type 
*                                     of the converting files. E.g. once real has been set to 
*                                     short, only short data type files, such as climate data,
*                                     will be converted. INT is the special one, when real was 
*                                     switched to int, it will also convert non-general inputs 
*                                     such as reservoir_info.
* 
* Mask file Layout: [lon_p1, lon_p2, lon_p3,...lat_p1, lat_p2,lat_p3,....]
* 
* Functions: readHeader--return a header structure of given filename
*                      writeHeader--write header to the given filename
*                      readGrid--read the input grids
*                      readMask--read the mask and write new input grid.bin
*                      indexing-- return index
*                      convGeneral--cutting general inputs (with the right header)
*                      convSoil_Lake--cutting inputs without headers such as soil and lakes
*                      conv_reservoir--special cutting for reservoir information
*  
* Usage: 
* 1. Modify Macros: detailed description was written in the code.
* 2. Modify files number and name
* 3. Set real data type and compile: gcc cutinput_global_mask.c -lm -o cut 
* 4. Do process 3 for three time for each data type, INT, LONG and SHORT
* 
*  Sinan Shi    2013-10-29 
* ********************************************************************/

#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>

typedef  short real;//int long short 
#define NPIX 67420 //global pixel number
#define NPIX_MASK 5794//6969//mask pixel number
#define MASKNAME "grid_basin" 
#define GRIDNAME "grid.bin"
#define CHECK //print header information
#define INPATH "/home/sinan/workspace/input_downscale/INPUTS/"//path of global inputs and mask
#define OUTPATH "/home/sinan/workspace/input_downscale/"//output of new input
#define HEADER 43 //header size
#define HEADERTITLE 7   //bytes of the header title

#define LINIDX(NY,NX,iz,iy,ix) ((NX)*(NY)*(iz) + (NX)*(iy) + (ix))
/****************
 *number of files and files name
 *****************/
#define NCOWFILES 2
#define NVAR6 6         /* number of climatic variables/files for CRU 2006*/
#define NVAR9 7
#define NVARERA 4       /*2 era + 2 gpcc*/
#define NCROPFILES 1    /*number of lu inputs to process*/
#define NSDATEFILES 2   
#define NDRAINFILES 1
#define NPOPFILES 3
#define NELEVFILES 1
#define NNEIFILES 1
#define NWUSEFILES 3 
#define NLAKEFILES 2
#define NRESFILES 1



const unsigned char filelist[][100]={"ListCountries.dat"};
const unsigned char filegrid[][100]={"grid.bin"};
const unsigned char filesoil[][100]={"soil_new_67420.bin"};
const unsigned char filecow[][100]={"cow_mg_2006.bin","cow_mg_2006_full.bin"};
const unsigned char fileclim6[][100]={"tmp.clm","pre.clm","cld.clm","wet.clm","tmx.clm","tmn.clm"};
const unsigned char fileclim9[][100]={"cru_ts_3_10.1901.2009.cld.clm","cru_ts_3_10.1901.2009.dtr.clm","cru_ts_3_10.1901.2009.pre.clm",
                                                                                      "cru_ts_3_10.1901.2009.tmn.clm","cru_ts_3_10.1901.2009.tmp.clm",
                                                                                      "cru_ts_3_10.1901.2009.tmx.clm","cru_ts_3_10.1901.2009.wet.clm"};
const unsigned char fileclimera[][100]={"lwnet_erainterim_1901-2010.clm","swdown_erainterim_1901-2010.clm",
                                                                                           "gpcc_cru09_prec_monthly_1901_2009.clm","gpcc_cru09_wet_monthly_1901_2009.clm"};
//const unsigned char filelu[][100]={"cft1700_2005_16cfts_SR.bin", "cft1700_2005_32bands.bin","cft1700_2005_bioenergy_sc.bin"};
const  unsigned char filelu[][100]={"cft1700_2010_new.bin"};
const unsigned char filesdate[][100]={"sdate_combined.clm", "sdate_combined_filled.clm"};
const unsigned char filedrain[][100]={"drainage.bin"};
const unsigned char filepopdens[][100]={"popdens_1901_2003.clm","popdens_HYDE3_1901_2007_bi.clm",
                                                                                             "popdens_HYDE_1901_2010_bi.clm"};
const unsigned char fileelev[][100]={"elevation.bin"};
const unsigned char filenei[][100]={"neighb_irrigation.bin"};
const unsigned char filewuse[][100]={"wateruse_1900_2000.bin","wateruse_1900_2100_A2.bin","wateruse_1900_2100_B1.bin"};
const unsigned char filelakes[][100]={"lakes.bin","lakeswithoutreservoirs.bin"};
const unsigned char fileres[][100]={"reservoir_info_grand5.bin"};



struct headers {
    char title[HEADERTITLE];
    int version;
    int order;
    int firstyear;
    int nyears;
    int firstcell;
    int ncells;
    int nbands;
    float cellsize;
    float scalar;
};


/**********************
 *readHeader
 * ********************/
struct headers readHeader(const char *filename){

     FILE *file;
     if((file=fopen(filename,"rt"))==NULL){
        printf("file %s can't open.\n",filename);
        exit(0);
    }
    struct headers thisHeader;
    printf("read %s headers.\n",filename);

    //read headers
    fseek(file,0,SEEK_SET);
    fread(thisHeader.title,sizeof(char),HEADERTITLE,file);     
    fread(&thisHeader.version,sizeof(int),1,file);             
    fread(&thisHeader.order,sizeof(int),1,file);               
    fread(&thisHeader.firstyear,sizeof(int),1,file);           
    fread(&thisHeader.nyears,sizeof(int),1,file);             
    fread(&thisHeader.firstcell,sizeof(int),1,file);           
    fread(&thisHeader.ncells,sizeof(int),1,file);            
    fread(&thisHeader.nbands,sizeof(int),1,file);           
    fread(&thisHeader.cellsize,sizeof(float),1,file);      
    fread(&thisHeader.scalar,sizeof(float),1,file);       
    close(file);
    return(thisHeader);
    
}

/**********************
 *printHeader
 * ********************/
void printHeader(const struct headers thisHeader){
    printf("title  |version|order|firstyear|nyear|firstcell|ncells|nbands |cellsize|scalar\n");
    printf("%s|   %d   |  %d  |   %d  |  %d  |    %d    |%d |   %d   |  %.2f  | %.2f\n",thisHeader.title,
           thisHeader.version,thisHeader.order,thisHeader.firstyear,thisHeader.nyears,thisHeader.firstcell,
           thisHeader.ncells,thisHeader.nbands,thisHeader.cellsize,thisHeader.scalar);
}
    
/**********************
 * readGrid
 * ********************/

void readGrid(int *glon, int *glat){
    printf("========\nGRID\n========\n");
    FILE *fgrid;
    int i;
    char intt[200];
    struct headers thisHeader;
    
    sprintf(intt,"%s%s",INPATH,GRIDNAME);
    thisHeader=readHeader(&intt[0]);
    short *temp;
    temp=(short *)malloc(2*thisHeader.ncells*sizeof(short));
#ifdef CHECK
    printHeader(thisHeader);
#endif
    sprintf(intt,"%s%s",INPATH,GRIDNAME);
    if((fgrid=fopen(intt,"rt"))==NULL){
        printf("file %s can't open.\n",intt);
        exit(0);
    }
    fseek(fgrid,HEADER,SEEK_SET);
    fread(temp,sizeof(short),thisHeader.ncells*2,fgrid);
    for(i=0;i<thisHeader.ncells;i++){
       glon[i]=(int)temp[2*i];
       glat[i]=(int) temp[2*i+1];
    }
    free(temp);
    fclose(fgrid);
    printf("Grid Reading Finished!\n");
}

void writeHeader(const struct headers originHeader,const char *filename){
  char intt[200];
  FILE *file;
  sprintf(intt,"%s%s",OUTPATH,filename);
  if((file=fopen(intt,"w"))==NULL){
    printf("file %s can't open.\n",intt);
    exit(0);
  }

  
  int ncells=NPIX_MASK;
  fseek(file,0,SEEK_SET);
  fwrite(originHeader.title,sizeof(char),HEADERTITLE,file);
  fwrite(&originHeader.version,sizeof(int),1,file);
  fwrite(&originHeader.order,sizeof(int),1,file);
  fwrite(&originHeader.firstyear,sizeof(int),1,file);
  fwrite(&originHeader.nyears,sizeof(int),1,file);
  fwrite(&originHeader.firstcell,sizeof(int),1,file);
  fwrite(&ncells,sizeof(int),1,file);//ncells is the only modification of original header and header after cut
  fwrite(&originHeader.nbands,sizeof(int),1,file);
  fwrite(&originHeader.cellsize,sizeof(float),1,file);
  fwrite(&originHeader.scalar,sizeof(float),1,file);
  close(file);
  
}
/**********************
 * readMask
 * ********************/
void readMask(int* lon, int *lat){
    printf("-----\nMask\n-----\n");
    int i;
    char intt[200];
    FILE *fmask,*fgrid;
    short *temp;
    sprintf(intt,"%s%s",INPATH,MASKNAME);
    if((fmask=fopen(intt,"rt"))==NULL){
        printf("file %s can't open.\n",intt);
        exit(0);
    }
    fread(&lon[0],4,NPIX_MASK,fmask);
    fread(&lat[0],4,NPIX_MASK,fmask);
    fclose(fmask);
#ifdef CHECK
    printf("Test of Mask:\n");
    printf("lon[0]=%d,lat[0]=%d,lon[NPIX]=%d,lat[NPIX]=%d\n",lon[0],lat[0],lon[NPIX_MASK-1],lat[NPIX_MASK-1]);
#endif
     printf("Read Mask Finished!\n");
     printf("Write Grid\n");
     temp=(short *)malloc(sizeof(short)*NPIX_MASK*2);
     for(i=0;i<NPIX_MASK;i++){
       temp[2*i]=(short)lon[i];
       temp[2*i+1]=(short)lat[i];
     }
     
     sprintf(intt,"%s%s",INPATH,GRIDNAME);
     struct headers thisHeader=readHeader(&intt[0]);
     writeHeader(thisHeader,GRIDNAME);
     sprintf(intt,"%s%s",OUTPATH,GRIDNAME);
     if((fgrid=fopen(intt,"w"))==NULL){
       printf("Writting Grid: file %s can't open.\n",intt);
       exit(0);
     }
     fseek(fgrid,HEADER,SEEK_SET);
     fwrite(temp,sizeof(short),NPIX_MASK*2,fgrid);
     fclose(fgrid);
     printf("Grid Write Finished.\n");
     free(temp);
}

/**************************************************
 * Put an index with the same lenght of global grid size (NPIX)
 *mark 1 for cells  taken for new grid
 * mark 0 for not taken
 * mlon, mlat: mask longitude, latitude
 * glon, glat:   global longitude, latitude
 ***************************************************/
void indexing(const int *mlon,const int *mlat,const int *glon,const int *glat,int *index){
    
    printf("-----\nIndexing\n-----\n");
    int i, j, k;
    int count=0;
    char intt[200];
    struct headers thisHeader;
    sprintf(intt,"%s%s",INPATH,GRIDNAME);
    thisHeader=readHeader(&intt[0]);
    //initialise
    for(i=0;i<thisHeader.ncells;i++){
        index[i]=0;
    }
    
    for(i=0;i<NPIX_MASK;i++){
        int find=0;
        for(j=0;j<thisHeader.ncells;j++){
               if(glon[j]==mlon[i]&&glat[j]==mlat[i]){
                   if(index[j]!=0){//check if the postion has already been written 
                       printf("Error in indexing:index[%d]=1!\n",j);
                       exit(0);
                   }
                   find=1;
                   index[j]=1;
                   count++;
               }
           }
        if(find==0){
            printf("NOT Found  %d:[%d,%d]\n", i,mlon[i],mlat[i]);
            printf("The pixel in the Mask can not find correspondent pixel in input file.\n");
            exit(0);
        }
    }
    if(count!=NPIX_MASK){
        printf("Error in indexing:count=%d,NPIX_MASK=%d\n",count,NPIX_MASK);
        exit(0);
    }
    printf("Indexing successful!\n");
}



/**********************
 * readGeneral
 * ********************/
int convGeneral(const int *index, const unsigned char filename[]){
    int i,j,k;
    char intt[200];
    struct headers thisHeader;
    struct headers outHeader;
    sprintf(intt,"%s%s",INPATH,filename);
    thisHeader=readHeader(&intt[0]);
#ifdef CHECK
    printHeader(thisHeader);
#endif 
    
    if(strcmp(filename,"wateruse_1900_2100_B1.bin")==0){//wrong header
        printf("correcting header...\n");
        thisHeader.nyears=201;
        thisHeader.nbands=1;
    }
    
    int nx=thisHeader.nbands;
    int ny=thisHeader.ncells;
    
    int nx_=thisHeader.nbands;
    int ny_=NPIX_MASK;
       
    FILE *inFile,*outFile;
    sprintf(intt,"%s%s",INPATH,filename);
    printf("converting %s.\n",intt);
    if((inFile=fopen(intt,"rt"))==NULL){
        printf("file %s can't open.\n",intt);
        exit(0);
    }

    real *temp;
    real *result;
    temp=(real *)malloc(thisHeader.nyears*thisHeader.nbands*thisHeader.ncells*sizeof(real));
    result=(real *)malloc(thisHeader.nyears*thisHeader.nbands*NPIX_MASK*sizeof(real));

    fseek(inFile,HEADER,SEEK_SET);
    fread(temp,sizeof(real),thisHeader.nyears*thisHeader.nbands*thisHeader.ncells,inFile);
    fclose(inFile);
    
    int p=0; 
    int cell,band,year;
    for(cell=0;cell<thisHeader.ncells;cell++){
        if(index[cell]==1){
            for(year=0;year<thisHeader.nyears;year++){
                for(band=0;band<thisHeader.nbands;band++){
                    result[LINIDX(ny_,nx_,year,p,band)]=temp[LINIDX(ny,nx,year,cell,band)];
                }
            }
            p++;
        }
    }
    
    if(strcmp(filename,"wateruse_1900_2100_B1.bin")==0){//writting the wrong header
        thisHeader.nyears=1;
        thisHeader.nbands=201;
    }
    
    //writting output
    writeHeader(thisHeader,filename);
    sprintf(intt,"%s%s",OUTPATH,filename);
    if((outFile=fopen(intt,"w"))==NULL){
       printf("file %s can't open.\n",intt);
      exit(0);
    }
    fseek(outFile,HEADER,SEEK_SET);
    fwrite(result,sizeof(real),thisHeader.nyears*thisHeader.nbands*NPIX_MASK,outFile);
    
    fclose(outFile);
        
    free(temp);
    free(result);
    

#ifdef CHECK
    char fname_out[200];
    printf("Checking Output Header:\n");
    sprintf(fname_out,"%s%s",OUTPATH,filename);
    outHeader=readHeader(&fname_out[0]);
   // printHeader(outHeader);
#endif
}

/**********************
 * readSoil_Lake: without proper headers
 * ********************/
int convSoil_Lake(const int *index, const unsigned char filename[]){
    int i,j,k;
    char intt[200];
       
    FILE *inFile,*outFile;
    sprintf(intt,"%s%s",INPATH,filename);
    printf("converting %s.\n",intt);
    if((inFile=fopen(intt,"rt"))==NULL){
        printf("file %s can't open.\n",intt);
        exit(0);
    }

    char  *temp;
    char *result;
    temp=(char *)malloc(NPIX*sizeof(char));
    result=(char *)malloc(NPIX_MASK*sizeof(char));

   fread(temp,sizeof(char),1*1*NPIX,inFile);
    fclose(inFile);

    int p=0; 
    int cell,band,year;
    for(cell=0;cell<NPIX;cell++){
        if(index[cell]==1){
            result[p]=temp[cell];
            p++;
        }
    }
    
    //writting output
    sprintf(intt,"%s%s",OUTPATH,filename);
    if((outFile=fopen(intt,"w"))==NULL){
       printf("file %s can't open.\n",intt);
      exit(0);
    }
    fwrite(result,sizeof(char),NPIX_MASK,outFile);

    fclose(outFile);
    free(temp);
    free(result);
}



/**********************
 * conv_reservoir
 * ********************/
int conv_reservoir(const int *index, const unsigned char filename[]){
    int i,j,k;
    char intt[200];
    struct headers thisHeader;
    int residual[4];  //values for last 16 bits of reserved values
    for(i=0;i<4;i++)   residual[i]=0;
   
       
    FILE *inFile,*outFile;
    sprintf(intt,"%s%s",INPATH,filename);
     thisHeader=readHeader(&intt[0]);
    printf("converting %s.\n",intt);
    if((inFile=fopen(intt,"rt"))==NULL){
        printf("file %s can't open.\n",intt);
        exit(0);
    }
    
    int *resyear, *resinst, *reshei, *respur;
    float *rescap, *resarea;
    
    resyear=(int *)malloc(NPIX_MASK*sizeof(int));
    rescap=(float *)malloc(NPIX_MASK*sizeof(float));
    resarea=(float *)malloc(NPIX_MASK*sizeof(float));
    resinst=(int *)malloc(NPIX_MASK*sizeof(int));
    reshei=(int *)malloc(NPIX_MASK*sizeof(int));
    respur=(int *)malloc(NPIX_MASK*sizeof(int));
    

    fseek(inFile,HEADER,SEEK_SET);
    int pp=0;
    for(i=0; i<NPIX; i++){
        if(index[i]==1){
            fread(&resyear[pp],sizeof(int),1,inFile); 
            fread(&rescap[pp],sizeof(float),1,inFile); 
            fread(&resarea[pp],sizeof(float),1,inFile); 
            fread(&resinst[pp],sizeof(int),1,inFile); 
            fread(&reshei[pp],sizeof(int),1,inFile); 
            fread(&respur[pp],sizeof(int),1,inFile); 
            fseek(inFile,16,SEEK_CUR);
            pp++;
        }
        else fseek(inFile,40,SEEK_CUR);
    }
        printf("reading finished\n");
        //writting output
    writeHeader(thisHeader,filename);
    sprintf(intt,"%s%s",OUTPATH,filename);
    if((outFile=fopen(intt,"w"))==NULL){
       printf("file %s can't open.\n",intt);
      exit(0);
    }
    fseek(outFile,HEADER,SEEK_SET);
    for(i=0;i<NPIX_MASK;i++){
        fwrite(&resyear[i],sizeof(int),1,outFile);
        fwrite(&rescap[i],sizeof(float),1,outFile);
        fwrite(&resarea[i],sizeof(float),1,outFile);
        fwrite(&resinst[i],sizeof(int),1,outFile);
        fwrite(&reshei[i],sizeof(int),1,outFile);
        fwrite(&respur[i],sizeof(int),1,outFile);
        fwrite(&residual[0],sizeof(int),4,outFile);
}
    
     
     fclose(outFile);
     free(resyear);
     free(rescap);
     free(resarea);
     free(resinst);
     free(reshei); 
     free(respur);   
}

int main(int argc, char *argv[]){
    int i,j,k;
    char intt[200];
    int lon_mask[NPIX_MASK], lat_mask[NPIX_MASK];
    struct headers gridHeader,thisHeader;
    sprintf(intt,"%s%s",INPATH,GRIDNAME);
    gridHeader=readHeader(&intt[0]);
    
    
    int *glon,*glat;//global longitude and latitude of original grid
    int *index;
    glon=(int *)malloc(gridHeader.ncells*sizeof(int));
    glat=(int *)malloc(gridHeader.ncells*sizeof(int));
    index=(int *)malloc(gridHeader.ncells*sizeof(int));
    
    readMask(lon_mask,lat_mask);
    readGrid(glon,glat);
    indexing(lon_mask,lat_mask,glon,glat,index);
    
    printf("%d\n",sizeof(real));

    /*SHORT*/
    if(sizeof(real)==sizeof(short)){
      for(i=0;i<NCOWFILES;i++)         convGeneral(index,filecow[i]);//cow file
      for(i=0;i<NVAR6;i++)                   convGeneral(index,fileclim6[i]);//climate
      for(i=0;i<NVAR9;i++)                   convGeneral(index,fileclim9[i]); 
      for(i=0;i<NVARERA;i++)              convGeneral(index,fileclimera[i]);        
      for(i=0;i<NSDATEFILES;i++)      convGeneral(index,filesdate[i]); //sowing data
      for(i=0;i<NPOPFILES;i++)           convGeneral(index,filepopdens[i]);//Population density
      for(i=0;i<NELEVFILES;i++)         convGeneral(index,fileelev[i]);       //elevation
      for(i=0;i<NCROPFILES;i++)        convGeneral(index,filelu[i]);      //CFT
}

    /*drainage & neibour irrigate(LONG)*/
    if(sizeof(real)==sizeof(long)){
      for(i=0;i<NDRAINFILES;i++)   convGeneral(index,filedrain[i]); //drainage
      for(i=0;i<NNEIFILES;i++)         convGeneral(index,filenei[i]);//neibourring irrigation
}
    
    /*water use & reservoir_info_grand5 & soil  (INT)*/
    if(sizeof(real)==sizeof(int)){
       for(i=0;i<NLAKEFILES;i++)        convSoil_Lake(index,filelakes[i]);//lakes
       for(i=0;i<NWUSEFILES;i++)      convGeneral(index,filewuse[i]);//water use
       for(i=0;i<NRESFILES;i++)          conv_reservoir(index,fileres[i]);//reservoir
       convSoil_Lake(index,filesoil[0]);//soil
    }
    
   
    free(glon);
    free(glat);
    free(index);
    

}
