#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<string.h>

//! coordination of LPJ grid
struct coord
{
    float *lon;
    float *lat;
    float *ind_lon;
    float *ind_lat;
    float lonmin;
    float lonmax;
    float latmin;
    float latmax;
};


FILE *open_output(const char *filename, const int nyears,
        const int nbands, const int ncells, const int size);
void read_output_thisband(const char *filename, float *data_thisband, 
        const int nyears, const int nbands, const int ncells,
        const int start_year, const int year, const int band);
//! read yearly output of LPJmL, return a
//! 2D pointer data[nyear][ncells]. By freeing 
//! this pointer one may need use free_yearly().
float **read_yearly_output(const char *filename, const int nyears, 
        const int ncells);
void free_yearly(float **output, const int nyears);


//! read yearly output of LPJmL, return a
//! 3D pointer data[nyear][nmonth=12][ncell]. 
//! By freeing this pointer one may need 
//! use free_montly().
float ***read_monthly_output(const char *filename,
        const int nyears, const int ncells);
void free_montly(float ***output, const int nyears, const int nmonths);
struct coord read_output_grid(char *filename, const int ncells, const int resolution);

