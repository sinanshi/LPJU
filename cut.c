/*********************************
* Global map and LPJmL data cutting.
* provide a mask which contains longitude, latitude information 
* Mask file Layout: [lon1, lon2, lon3,...lat1, lat2,lat3,....]
* It provdes index which has as the s
* *******************************/

#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>


#define NPIX 67420
#define NC 440          /* total of countries and regions*/

/*#define NLMed 66*/
#define NLMed 113
#define NCMed 127

#define HEADER 43
#define HEADERTITLE 7   /*bytes of the header title*/

/*bands*/
#define NCROPS 32
#define NGRID 2         
#define NMONTH 12 
#define NCOW 2          /*country code, region code*/
#define NDRAIN 2        /*bands of the drain file, first one where is going the water next, second one, distance between cells)*/
#define NSDATE 22

/*number of files*/

#define NCOWFILES 2
#define NVAR6 6         /* number of climatic variables/files for CRU 2006*/
#define NVAR9 7
#define NVARERA 4       /*2 era + 2 gpcc*/
#define NCROPFILES 3    /*number of lu inputs to process*/
#define NSDATEFILES 2   
#define NDRAINFILES 1
#define NPOPFILES 3
#define NELEVFILES 1
#define NNEIFILES 1
#define NWUSEFILES 3 
#define NLAKEFILES 2
#define NRESFILES 1

#define LIN(ix,iy,iz) 
#define CHECK 
#define INPATH ""
#define OUTPATH "New"

#define LINIDX(NY,NX,iz,iy,ix) ((NX)*(NY)*(iz) + (NX)*(iy) + (ix))
#define NPIX_MASK 6735
#define MASKNAME "grid_basin"
#define GRIDNAME "grid.bin"


const unsigned char filelist[][100]={"ListCountries.dat"};
const unsigned char filegrid[][100]={"grid"};
const unsigned char filesoil[][100]={"soil_new_67420"};
const unsigned char filecow[][100]={"cow_mg_2006.bin","cow_mg_2006_full"};
const unsigned char fileclim6[][100]={"tmp.clm","pre.clm","cld","wet","tmx","tmn"};
const unsigned char fileclim9[][100]={"cru_ts_3_10.1901.2009.cld.clm","cru_ts_3_10.1901.2009.dtr",
  "cru_ts_3_10.1901.2009.pre","cru_ts_3_10.1901.2009.tmn","cru_ts_3_10.1901.2009.tmp","cru_ts_3_10.1901.2009.tmx",
  "cru_ts_3_10.1901.2009.wet"};
const unsigned char fileclimera[][100]={"lwnet_erainterim_1901-2010","swdown_erainterim_1901-2010",
  "gpcc_cru09_prec_monthly_1901_2009","gpcc_cru09_wet_monthly_1901_2009"};
const unsigned char filelu[][100]={"cft1700_2005_16cfts_SR", "cft1700_2005_32bands","cft1700_2005_bioenergy_sc"};
const unsigned char filesdate[][100]={"sdate_combined", "sdate_combined_filled"};
const unsigned char filedrain[][100]={"drainage.bin"};
const unsigned char filepopdens[][100]={"popdens_1901_2003","popdens_HYDE3_1901_2007_bi","popdens_HYDE_1901_2010_bi"};
const unsigned char fileelev[][100]={"elevation"};
const unsigned char filenei[][100]={"neighb_irrigation"};
const unsigned char filewuse[][100]={"wateruse_1900_2000","wateruse_1900_2100_A2","wateruse_1900_2100_B1"};
const unsigned char filelakes[][100]={"lakes.bin","lakeswithoutreservoirs"};
const unsigned char fileres[][100]={"reservoir_info_grand5"};


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

void readMask(int* lon, int *lat){
    printf("-----\nMask\n-----\n");
    char intt[200];
    FILE *fmask;
    sprintf(intt,"%s%s",INPATH,MASKNAME);
    if((fmask=fopen(intt,"rt"))==NULL){
        printf("file %s can't open.\n",intt);
        return;
    }
    
     fread(&lon[0],4,NPIX_MASK,fmask);
     fread(&lat[0],4,NPIX_MASK,fmask);
     fclose(fmask);
#ifdef CHECK
     printf("Test of Mask:\n");
     printf("lon[0]=%d,lat[0]=%d,lon[NPIX]=%d,lat[NPIX]=%d\n",lon[0],lat[0],lon[NPIX_MASK-1],lat[NPIX_MASK-1]);
#endif
     printf("Read Mask Finished!\n");
}

struct headers readHeader(const char *filename){
    char intt[200];
     FILE *file;
    sprintf(intt,"%s%s",INPATH,filename);
    if((file=fopen(intt,"rt"))==NULL){
        printf("file %s can't open.\n",intt);
        return;
    }
    struct headers thisHeader;
    printf("read %s headers.\n",filename);

    //read headers
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

void printHeader(const struct headers thisHeader){
  
    printf("title  |version|order|firstyear|nyear|firstcell|ncells|nbands |cellsize|scalar\n");
    printf("%s|   %d   |  %d  |   %d  |  %d  |    %d    |%d |   %d   |  %.2f  | %.2f\n",thisHeader.title,
           thisHeader.version,thisHeader.order,thisHeader.firstyear,thisHeader.nyears,thisHeader.firstcell,
           thisHeader.ncells,thisHeader.nbands,thisHeader.cellsize,thisHeader.scalar);
 
}
    
    

void readGrid(int *glon, int *glat){
    printf("========\nGRID\n========\n");
    FILE *fgrid;
    int i;
    char intt[200];
    struct headers thisHeader;
    thisHeader=readHeader(GRIDNAME);
    short *temp;
    temp=(short *)malloc(2*thisHeader.ncells*sizeof(short));

    printHeader(thisHeader);
    sprintf(intt,"%s%s",INPATH,GRIDNAME);
    if((fgrid=fopen(intt,"rt"))==NULL){
        printf("file %s can't open.\n",intt);
        return;
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

void indexing(const int *mlon,const int *mlat,const int *glon,const int *glat,int *index){
    
    printf("-----\nIndexing\n-----\n");
    int i, j, k;
    int count=1;
    struct headers thisHeader;
    thisHeader=readHeader(GRIDNAME);
    //initialise
    for(i=0;i<thisHeader.ncells;i++){
        index[i]=0;
    }
    
    for(i=0;i<NPIX_MASK;i++){
        for(j=0;j<thisHeader.ncells;j++){
               if(glon[j]==mlon[i]&&glat[j]==mlat[i]){
                   if(index[j]!=0){
                       printf("Error in indexing:index[%d]=1!\n",j);
                       exit;
                   }
                   index[j]=1;
                   count++;
               }
        }
    }
    if(count!=NPIX_MASK){
        printf("Error in indexing:count=%d,NPIX_MASK=%d\n",count,NPIX_MASK);
        exit;
    }
    printf("Indexing successful!\n");
}


int convClim(const int *index, const unsigned char filename[]){
    int i,j,k;
    char intt[200];
    struct headers thisHeader;
    thisHeader=readHeader(filename);
    printHeader(thisHeader);
    int nx=thisHeader.nbands;
    int ny=thisHeader.ncells;
    
    int nx_=thisHeader.nbands;
    int ny_=NPIX_MASK;
       
    FILE *file;
    sprintf(intt,"%s%s",INPATH,filename);
    if((file=fopen(filename,"rt"))==NULL){
        printf("file %s can't open.\n",intt);
        return;
    }

    short *temp;
    short *result;
    temp=(short *)malloc(thisHeader.nyears*thisHeader.nbands*thisHeader.ncells*sizeof(short));
    result=(short *)malloc(thisHeader.nyears*thisHeader.nbands*NPIX_MASK*sizeof(short));
    
    
    fseek(file,HEADER,SEEK_SET);
    fread(temp,sizeof(short),thisHeader.nyears*thisHeader.nbands*thisHeader.ncells,file);
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

    fclose(file);
    free(temp);
    free(result);
}




int main(int argc, char *argv[]){
    int i,j,k;
    int lon_mask[NPIX_MASK], lat_mask[NPIX_MASK];
    struct headers gridHeader;
    gridHeader=readHeader(GRIDNAME);
    
    
    int *glon,*glat;//global longitude and latitude of original grid
    int *index;
    glon=(int *)malloc(gridHeader.ncells*sizeof(int));
    glat=(int *)malloc(gridHeader.ncells*sizeof(int));
    index=(int *)malloc(gridHeader.ncells*sizeof(int));
    
    readMask(lon_mask,lat_mask);
    readGrid(glon,glat);
    indexing(lon_mask,lat_mask,glon,glat,index);
    
    convClim(index,fileclim6[0]);
    //convGeneral(index,filedrain[0]);
    





    
    
    free(glon);
    free(glat);
    free(index);
    

}
