# Fortran-NetCDF
A light weighted NetCDF C Library wrapper and an intermediate interface written in modern Fortran.

## Get started

### Using fpm

As currently the netcdf has not been fully supported by the fpm (see [Candidate packages to get working](https://github.com/fortran-lang/fpm/issues/17)), the safest way to include the netcdf would be

```
fpm build --profile debug --flag "-L$NC_LIB -I$NC_INC -lnetcdf -lnetcdff"
```
