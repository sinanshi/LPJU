

find_package(NetCDF)
add_executable(lpjout2ncdf lpjout2ncdf.c)
target_link_libraries(lpjout2ncdf netcdf lpju)
