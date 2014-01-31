/*************************
 * Convert integer grid input to short
 * Sinan Shi 2013-12-18
 * ************************/

#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>

typedef  long real;//int long short 
#define NPIX 67420 //global pixel number
#define NPIX_MASK 5794//mask pixel number
#define MASKNAME "basin_mask" 
#define GRIDNAME "grid_quater.bin"
#define CHECK //print header information
#define INPATH "/home/sinan/workspace/LPJ_Utilities/src/nela/half2quarter/"
#define OUTPATH  "/home/sinan/workspace/LPJ_Utilities/src/nela/half2quarter/out/"
#define HEADER 43 //header size
#define HEADERTITLE 7   //bytes of the header title


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




int main(int argc, char *argv[]){
    int i,j,k;
    char intt[200];
    FILE *fgrid;
    struct headers gridHeader,thisHeader;
    sprintf(intt,"%s%s",INPATH,GRIDNAME);
    gridHeader=readHeader(&intt[0]);
    printHeader(gridHeader);
    int *data_int;
    short *data_short;
    
    data_int=(int *)malloc(gridHeader.ncells*sizeof(int)*2);
    data_short=(short *)malloc(gridHeader.ncells*sizeof(short)*2);
    
    sprintf(intt,"%s%s",INPATH,GRIDNAME);
    if((fgrid=fopen(intt,"rt"))==NULL){
        printf("file %s can't open.\n",intt);
        exit(0);
    }
    fseek(fgrid,HEADER,SEEK_SET);
    fread(data_int,sizeof(int),gridHeader.ncells*2,fgrid);
    fclose(fgrid);
    
    for(i=0;i<gridHeader.ncells*2;i++){
        data_short[i]=(short)data_int[i];
	//if(data_int[i]<32767)
	//printf("%d=%d  ",data_int[i],data_short[i]);
    }
    

    char filename[200]="grid_short";
     FILE *file;
     sprintf(intt,"%s%s",OUTPATH,filename);
     if((file=fopen(intt,"w"))==NULL){
      printf("file %s can't open.\n",intt);
    exit(0);
  }
  int ncells=gridHeader.ncells;
  gridHeader.scalar=0.01;
  fseek(file,0,SEEK_SET);
  fwrite(gridHeader.title,sizeof(char),HEADERTITLE,file);
  fwrite(&gridHeader.version,sizeof(int),1,file);
  fwrite(&gridHeader.order,sizeof(int),1,file);
  fwrite(&gridHeader.firstyear,sizeof(int),1,file);
  fwrite(&gridHeader.nyears,sizeof(int),1,file);
  fwrite(&gridHeader.firstcell,sizeof(int),1,file);
  fwrite(&ncells,sizeof(int),1,file);//ncells is the only modification of gridal header and header after cut
  fwrite(&gridHeader.nbands,sizeof(int),1,file);
  fwrite(&gridHeader.cellsize,sizeof(float),1,file);
  fwrite(&gridHeader.scalar,sizeof(float),1,file);
  fwrite(&data_short[0],sizeof(short),gridHeader.ncells*2,file);
   close(file);
   
   thisHeader=readHeader(&intt[0]);
   printHeader(thisHeader);
  
  
 
    
    
    

    
  

}
