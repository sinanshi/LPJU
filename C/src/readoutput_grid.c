#include "lpjoutdata.h"
#include <sys/stat.h>
#define max(a,b) a>b?a:b
#define min(a,b) a<b?a:b


struct coord read_output_grid(char *filename, const int ncells, const int resolution)
{
    struct coord latlon;
    struct stat st;
    FILE *fp;
    short buflon, buflat;
    int i;
    printf("reading output grid...");
    latlon.lonmin = 180.;
    latlon.lonmax = -180.;
    latlon.latmin = 90.;
    latlon.latmax = -90.;


    fp=fopen(filename,"rb");
    if(fp==NULL)
    {
        printf("[ERROR]: file cannot be found!\n");
        exit(EXIT_FAILURE);
    }
    //! check if size is correct
    stat(filename, &st);
    if((int)st.st_size/ncells/sizeof(short)/2!=1)
    {
        printf("[ERROR]: grid size is wrong. ncell=%f\n",
                (float)st.st_size/sizeof(short)/2);
        exit(EXIT_FAILURE);
    }

    latlon.lon = (float *)malloc(sizeof(float) * ncells);
    latlon.lat = (float *)malloc(sizeof(float) * ncells);
    latlon.ind_lon = (float *)malloc(sizeof(float) * ncells);
    latlon.ind_lat = (float *)malloc(sizeof(float) * ncells);
    
    for(i = 0; i < ncells; ++i)
    {
        fread(&buflon, sizeof(short), 1, fp);
        fread(&buflat, sizeof(short), 1, fp);
        latlon.lon[i] = (float) buflon/100;
        latlon.lat[i] = (float) buflat/100;
        latlon.lonmin = min(latlon.lon[i], latlon.lonmin);
        latlon.lonmax = max(latlon.lon[i], latlon.lonmax);
        latlon.latmin = min(latlon.lat[i], latlon.latmin);
        latlon.latmax = max(latlon.lat[i], latlon.latmax);
    }

    for(i = 0; i < ncells; ++i)
    {
        latlon.ind_lon[i] = ceil(latlon.lon[i]/resolution - 
                latlon.lonmin/resolution);
        latlon.ind_lat[i] = ceil(latlon.lat[i]/resolution - 
                latlon.latmin/resolution);
    }
    printf("[done]\n");
    return latlon;
}
