#include "lpjoutdata.h"
#include <stdlib.h>
#include <sys/stat.h>
#include <math.h>

FILE *open_output(
        const char *filename,
        const int nyears,
        const int nbands,
        const int ncells,
        const int size)
{
    struct stat st;
    FILE *fp;
    fp=fopen(filename,"rb");
    if(fp==NULL)
    {
        printf("[ERROR]: file cannot be found!\n");
        exit(EXIT_FAILURE);
    }
    //! check if size is correct
    stat(filename, &st);
    if(st.st_size/ncells/nyears/nbands/size!=1.0)
    {
        printf("[ERROR]: file size is wrong.\n");
        exit(EXIT_FAILURE);
    }
    return(fp);
}


//! need to be tested
void read_output_thisband(
        const char *filename,
        float *data_thisband,//!one dimentional ncells array
        const int nyears,
        const int nbands,
        const int ncells,
        const int start_year,
        const int year,
        //! first band is band 1.
        const int band)
{
    int this_year=year-start_year;
    int start_pos= this_year*nbands*ncells+(band-1)*nbands*ncells;
    if(start_pos<0 || start_pos>nyears*nbands*ncells)
    {
        printf("[ERROR]: postion error.");
        exit(EXIT_FAILURE);
    }

    FILE *fp=open_output(
            filename, nyears, nbands, ncells, sizeof(float));
    fseek(fp, start_pos, SEEK_SET);
    fread(data_thisband, sizeof(float), ncells, fp);
    fclose(fp);
}

//! read yearly output of LPJmL, return a
//! 2D pointer data[nyear][ncells]. By freeing 
//! this pointer one may need use free_yearly().
float **read_yearly_output(
        const char *filename,
        const int nyears,
        const int ncells)
{
    printf("reading yearly output->%s...",filename);
    int y;
    float **yearly_data;
    yearly_data =(float **) malloc(sizeof(float *)*nyears);
    for( y = 0; y < nyears; ++y)
        yearly_data[y] = (float *)malloc(sizeof(float)*ncells);
    for( y = 0; y<nyears; ++y)
    {
        read_output_thisband(filename, &yearly_data[y][0],
                nyears, 1, ncells, 0, y, 1);
    }
    printf("[done]\n");
    return(yearly_data);
}

void free_yearly(float **output, const int nyears)
{
    int y;
    for(y = 0; y < nyears; ++y)
        free(output[y]);
    free(output);
}

//! read yearly output of LPJmL, return a
//! 3D pointer data[nyear][nmonth=12][ncell]. 
//! By freeing this pointer one may need 
//! use free_montly().
float ***read_monthly_output(
        const char *filename,
        const nyears,
        const ncells)
{
    printf("reading monthly output:->%s...",filename);
    int nmonths = 12;
    int y, m;
    float ***monthly_data;
    monthly_data = (float ***)malloc(sizeof(float **) * nyears);
    for(y = 0; y < nyears; ++y)
    {
        monthly_data[y] = (float **)malloc(sizeof(float *) * nmonths);
        for(m = 0; m < nmonths; ++m)
        {
            monthly_data[y][m] = 
                (float *)malloc(sizeof(float) * ncells);
        }
    }

    for(y = 0; y < nyears; ++y)
    {
        for(m = 0; m < nmonths; ++m)
        {
            read_output_thisband(filename, &monthly_data[y][m][0],
                nyears, 12, ncells, 0, y, m);
        }
    }
    printf("[done]\n");
    return(monthly_data);
}


void free_montly(float ***output, const int nyears, const int nmonths)
{
    int y, m;
    for(y = 0; y < nyears; ++y)
    {
        for(m = 0; m < nmonths; ++m)
        {
            free(output[y][m]);
        }
        free(output[y]);
    }
    free(output);
}




