/*****************************
 * drain2basin.c
 * derive basins from drainage
 *Sinan Shi 2013-11-13
 ******************************/

#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>

#define INPATH ""

#define NPIX 67420
#define HEADER 43
#define BASIN_NAME "basin.bin"

const unsigned char filedrain[100]={"drainage.bin"};

int readdrain(const unsigned char *filename, int *next_drain){
    int i,j,k;
    char intt[200];
    
    FILE *inFile;
    sprintf(intt,"%s%s",INPATH,filename);
    printf("reading %s.\n",intt);
    if((inFile=fopen(intt,"rt"))==NULL){
        printf("file %s can't open(drainage).\n",intt);
        exit(0);
    }

    int *temp;
    temp=(int *)malloc(2*NPIX*sizeof(int));

    fseek(inFile,HEADER,SEEK_SET);
    fread(temp,sizeof(int),2*NPIX,inFile);//int????
    fclose(inFile);
    
    for(i=0;i<NPIX;i++){
        next_drain[i]=temp[2*i];
    }
    
    free(temp);
    return(0);
}

int check_drain(const int *next_drain){
    int i;
    for( i=0;i<NPIX;i++){
     //if(next_drain[i]!=-1&&next_drain[i]!=-9&&next_drain[i]>NPIX&&next_drain[i]<0){
       if(next_drain[i]==-77)
        printf("%d,%d\n",i,next_drain[i]);
    //    }
    }
}

int creatBasin(const int *next_drain,int *basin){
    int i,j,k;
    int basin_id=0;
    int step,current,next,this_basin;
    
        for(i=0;i<NPIX;i++){
        if(basin[i]==-77){
            if(next_drain[i]==-1||next_drain[i]==-9){
                basin[i]=basin_id;
                basin_id++;
            }
        }
    }
    
    for(i=0;i<NPIX;i++){
        if(basin[i]==-77){
            current=i;
            step=0;
            next=-77;
            while(1){
                if(basin[current]!=-77){
                    this_basin=basin[current];
                    break;
                }
                   next=next_drain[current];
                   current=next;
                   step++;
                //  printf("%d ",step);
            }
     
            basin[i]=this_basin;
          }
     }
     
    

}

writeBasin(int *basin){
    char intt[200];
    FILE *outFile;
    sprintf(intt,"%s",BASIN_NAME);
    if((outFile=fopen(intt,"w"))==NULL){
       printf("file %s can't open.\n",intt);
      exit(0);
    }
    fwrite(&basin[0],sizeof(int),NPIX,outFile);
    printf("writing basin to %s\n",intt);
    fclose(outFile);
    
}
    

int main(int argc, char *argv[]){
    int i,j,k;
    int *next_drain,*basin;
    
    next_drain=(int *)malloc(NPIX*sizeof(int));
    basin=(int *)malloc(NPIX*sizeof(int));
    
    for(i=0;i<NPIX;i++) next_drain[i]=-77;
    for(i=0;i<NPIX;i++) basin[i]=-77;
    readdrain(&filedrain[0],next_drain);
    //check_drain(next_drain);
    creatBasin(next_drain,basin);
    check_drain(basin);
    writeBasin(basin);
    
    free(next_drain);
    free(basin);
    return(0);
    
}