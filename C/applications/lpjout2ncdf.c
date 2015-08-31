#include "lpjoutdata.h"
#include <netcdf.h>
#include <unistd.h>
int main(int argc, char* argv[])
{
    int opt;
    char *grid_path, *lpjout_path, *unit;
    int ncells, nyears;
    int start_year;
    while((opt = getopt(argc, argv, "g:i:u:c:y:s:"))!=-1)
    {
        switch(opt)
        {
            case 'g':
                grid_path = optarg;
            case 'i':
                lpjout_path  = optarg;
            case 'u':
                unit = optarg;
            case 'c':
                ncells = atoi(optarg);
            case 'y':
                nyears = atoi(optarg);
            case 's':
                start_year = atoi(optarg);
        }
    }

    struct coord latlon;
    latlon = read_output_grid(grid_path, ncells, 0.5);
    
    float **data = read_yearly_output(lpjout_path, nyears, ncells);
    free_yearly(data, nyears);

       printf("%f|",latlon.latmax);
//    printf("%s",grid_path);

}


