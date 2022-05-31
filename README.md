# Fortran-NetCDF
Light weighted NetCDF Fortran API wrapper written in modern Fortran

## Get started

### Using fpm

```
fpm build --profile debug --flag "$(pkg-config --cflags --libs netcdf-fortran)"
```