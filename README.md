# Self-baked NetCDF Fortran Library

## Get started
The Self-baked NetCDF Fortran library is a light weighted NetCDF C Library wrapper and an intermediate interface written in modern Fortran. This project is designed as a solution of the current fpm/NetCDF incoherence (see [fpm discussion](https://github.com/fortran-lang/fpm/discussions/458), [fpm issue](https://github.com/fortran-lang/fpm/issues/17), [discussion on discourse](https://fortran-lang.discourse.group/t/using-netcdf-with-fpm/4225)). The major difference between this library and most of the other implementations (for example [nc4fortran](https://github.com/geospace-code/nc4fortran)) is that this library is built on top of NetCDF C library directly, thus you won't need the official NetCDF Fortran library as a dependence. If you are intereseted in using this project with [fpm](https://github.com/fortran-lang/fpm), add the following lines in your `fpm.toml`
```
[dependencies]
self-baked-nf = {git="https://github.com/han190/Self-Baked-Fortran-NetCDF"}

[build]
link = "netcdf"
```
and make sure you have NetCDF C library properly installed.

## Quick tutorial
Here is an example of reading a 3 dimensional value from a NetCDF file. The data `sample02.nc` can be found in folder `data`. You could also download it from [the fifth generation ECMWF reanalysis (ERA5) website](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview).

```
program main

  use module_netcdf
  use iso_fortran_env, only: int16, real64, int64
  implicit none

  type(group_type) :: nc
  type(variable_type) :: var

  integer(int16), allocatable :: raw(:)
  integer(int64) :: s(3)
  real(real64) :: scale_factor, add_offset
  real, allocatable, target :: vals(:)
  real, pointer :: ptr(:,:,:) => null()

  !> Only dimension will loaded unless optional
  !> arguments are specified
  nc = dataset("./data/sample02.nc", "r")

  !> Inquire variable metadata 
  !> (name, id, dimension, attribute, etc.)
  var = inq_var(nc, "t2m")

  !> User-defined Derived Type I/O
  print *, var

  !> Extract raw data and by convention
  !> scale factor and add offset.
  call get_var(var, raw)
  call get_att(var, "scale_factor", scale_factor)
  call get_att(var, "add_offset", add_offset)

  !> Map the data to a 3-dimensional pointer.
  vals = raw*scale_factor + add_offset
  s = shape(var)
  ptr(1:s(1), 1:s(2), 1:s(3)) => vals
  print *, "Data successfully read."

end program main
```
