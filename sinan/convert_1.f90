program convert
  use netcdf
  implicit none

  integer :: ncid

  ! We are writing 4D data, a 2 x 6 x 12 lvl-lat-lon grid, with 2
  ! timesteps of data.
  integer, parameter :: NDIMS = 3, NRECS = 1
  integer, parameter :: NLATS = 32, NLONS = 253
  character (len = *), parameter :: LAT_NAME = "y"
  character (len = *), parameter :: LON_NAME = "x"
  character (len = *), parameter :: REC_NAME = "time"
  character (len = *), parameter :: GDD_NAME = "pp"
  character (len = *), parameter :: TREE_NAME = "pd"
  character (len = *), parameter :: GRASS_NAME = "pw"
  integer :: lon_dimid, lat_dimid, rec_dimid

  ! These program variables hold the latitudes and longitudes.
  real*8  :: lats(NLATS), lons(NLONS)
  real*8  :: pp(NLATS,NLONS),pd(NLATS,NLONS),pw(NLATS,NLONS)
  integer :: lon_varid, lat_varid

  ! We will create two netCDF variables, one each for temperature and
  ! pressure fields.
  character (len = *), parameter :: NAME="long_name"
  character (len = *), parameter :: GDD=""
  character (len = *), parameter :: TREE=""
  character (len = *), parameter :: GRASS=""
  integer :: tree_varid,grass_varid,gdd_varid
  integer :: dimids(NDIMS)

  ! We recommend that each variable carry a "units" attribute.
  character (len = *), parameter :: UNITS = "units"
  character (len = *), parameter :: LAT_UNITS = ""
  character (len = *), parameter :: LON_UNITS = ""
  character (len = *), parameter :: FILLVALUE = "_FillValue"
  character (len = *), parameter :: MISSVALUE = "missing_value"

  ! Program variables to hold the data we will write out. We will only
  ! need enough space to hold one timestep of data; one record.
  real*8 :: gdd_out(NLONS, NLATS, NRECS)
  real*8 :: tree_out(NLONS, NLATS, NRECS)
  real*8 :: grass_out(NLONS, NLATS, NRECS)
  real*8, parameter :: fillval = 999. ;
  real*8, parameter :: missval = -1.e+32 ;

  ! This is the name of the data file we will create.
  character (nf90_max_name) :: inputfile,outputfile

  ! Use these to construct some latitude and longitude data for this
  ! example.
  real, parameter :: START_LAT = -85.76059, START_LON = 0.

  ! Loop indices
  integer :: lon, i, j, k

!  data lats / -85.76059, -80.26878, -74.74454, -69.21297, -63.67863, -58.14296, &
!    -52.60653, -47.06964, -41.53246, -35.99508, -30.45755, -24.91993, &
!    -19.38223, -13.84448, -8.306703, -2.768903, 2.768903, 8.306703, 13.84448, &
!    19.38223, 24.91993, 30.45755, 35.99508, 41.53246, 47.06964, 52.60653, &
!    58.14296, 63.67863, 69.21297, 74.74454, 80.26878, 85.76059 /
!  lats(1) = -85.76059
  do lon = 1, NLATS
     lats(lon) = lon
  end do

  !get argument for filename
  call getarg(1,inputfile)
  call getarg(2,outputfile)

  ! Create pretend data. If this wasn't an example program, we would
  ! have some real data to write, for example, model output.
  do lon = 1, NLONS
!     lons(lon) = START_LON + (lon - 1) * 5.625
     lons(lon) = lon
  end do

  open(65,file=inputfile,status='old')
  read (65,*) pp
  write(*,*) pp(27,235)
  read (65,*) pd
  read (65,*) pw
  do i=1,NLONS
     do j=1,NLATS
        gdd_out(i,j,1)=pp(j,i)
        tree_out(i,j,1)=pd(j,i)
        grass_out(i,j,1)=pw(j,i)
     end do
  end do
  close(65)
  write(*,*) gdd_out(235,27,1)

  write(*,*) "Start Netcdf creation..."

  ! Create the file. 
  call check( nf90_create(outputfile, nf90_clobber, ncid) ,i)
  
  ! Define the dimensions. The record dimension is defined to have
  ! unlimited length - it can grow as needed. In this example it is
  ! the time dimension.
  call check( nf90_def_dim(ncid, LAT_NAME, NLATS, lat_dimid) ,i)
  call check( nf90_def_dim(ncid, LON_NAME, NLONS, lon_dimid) ,i)
  call check( nf90_def_dim(ncid, REC_NAME, NF90_UNLIMITED, rec_dimid) ,i)

  ! Define the coordinate variables. We will only define coordinate
  ! variables for lat and lon.  Ordinarily we would need to provide
  ! an array of dimension IDs for each variable's dimensions, but
  ! since coordinate variables only have one dimension, we can
  ! simply provide the address of that dimension ID (lat_dimid) and
  ! similarly for (lon_dimid).
  call check( nf90_def_var(ncid, LAT_NAME, NF90_REAL, lat_dimid, lat_varid) ,i)
  call check( nf90_def_var(ncid, LON_NAME, NF90_REAL, lon_dimid, lon_varid) ,i)

  ! Assign units attributes to coordinate variables.
  call check( nf90_put_att(ncid, lat_varid, UNITS, LAT_UNITS) ,i)
  call check( nf90_put_att(ncid, lon_varid, UNITS, LON_UNITS) ,i)

  ! The dimids array is used to pass the dimids of the dimensions of
  ! the netCDF variables. Both of the netCDF variables we are creating
  ! share the same four dimensions. In Fortran, the unlimited
  ! dimension must come last on the list of dimids.
  dimids = (/ lon_dimid, lat_dimid, rec_dimid /)

  ! Define the netCDF variables for the pressure and temperature data.
  call check( nf90_def_var(ncid, TREE_NAME, NF90_DOUBLE, dimids, tree_varid) ,i)
  call check( nf90_def_var(ncid, GRASS_NAME, NF90_DOUBLE, dimids, grass_varid) ,i)
  call check( nf90_def_var(ncid, GDD_NAME, NF90_DOUBLE, dimids, gdd_varid) ,i)

  ! Assign attributes to the netCDF variables.
  call check( nf90_put_att(ncid, tree_varid, NAME, TREE) ,i)
  call check( nf90_put_att(ncid, tree_varid, FILLVALUE, fillval) ,i)
  call check( nf90_put_att(ncid, tree_varid, MISSVALUE, missval) ,i)
  call check( nf90_put_att(ncid, grass_varid, NAME, GRASS) ,i)
  call check( nf90_put_att(ncid, grass_varid, FILLVALUE, fillval) ,i)
  call check( nf90_put_att(ncid, grass_varid, MISSVALUE, missval) ,i)
  call check( nf90_put_att(ncid, gdd_varid, NAME, GDD) ,i)
  call check( nf90_put_att(ncid, gdd_varid, FILLVALUE, fillval) ,i)
  call check( nf90_put_att(ncid, gdd_varid, MISSVALUE, missval) ,i)
  
  ! End define mode.
  call check( nf90_enddef(ncid) ,i)
  
  ! Write the coordinate variable data. This will put the latitudes
  ! and longitudes of our data grid into the netCDF file.
  call check( nf90_put_var(ncid, lat_varid, lats) ,i)
  call check( nf90_put_var(ncid, lon_varid, lons) ,i)
  
  ! Write the pretend data. This will write our surface pressure and
  ! surface temperature data. The arrays only hold one timestep worth
  ! of data. We will just rewrite the same data for each timestep. In
  ! a real :: application, the data would change between timesteps.
  call check( nf90_put_var(ncid, tree_varid, tree_out),i)
  call check( nf90_put_var(ncid, grass_varid, grass_out),i)
  call check( nf90_put_var(ncid, gdd_varid, gdd_out),i)
  
  ! Close the file. This causes netCDF to flush all buffers and make
  ! sure your data are really written to disk.
  call check( nf90_close(ncid) ,i)
  
  print *,"*** SUCCESS writing example file ", trim(outputfile), "!"

contains
  subroutine check(status,pos)
    integer, intent ( in) :: status, pos
    
    if(status /= nf90_noerr) then 
      print *, "Step: ",pos
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check
end program convert
