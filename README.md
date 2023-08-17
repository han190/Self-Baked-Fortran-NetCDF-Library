# Self-baked NetCDF Fortran Library

## Get started
The Self-baked NetCDF Fortran library is a light weighted NetCDF C Library wrapper and an intermediate interface written in modern Fortran. This project is designed as a workaround of the current fpm/NetCDF incoherence (see [fpm discussion](https://github.com/fortran-lang/fpm/discussions/458), [fpm issue](https://github.com/fortran-lang/fpm/issues/17), [discussion on discourse](https://fortran-lang.discourse.group/t/using-netcdf-with-fpm/4225)). The major difference between this library and most of the other implementations (for example [nc4fortran](https://github.com/geospace-code/nc4fortran)) is that this library is built on top of NetCDF C library directly, thus you won't need the official NetCDF Fortran library as a dependence. If you are intereseted in using this project with [fpm](https://github.com/fortran-lang/fpm), add the following lines in your `fpm.toml`
```
[dependencies]
self-baked-nf = {git="https://github.com/han190/Self-Baked-Fortran-NetCDF"}

[build]
link = "netcdf"
```
and make sure you have NetCDF C library properly installed.

## Examples
### Write to NetCDF
```Fortran
program main
  use module_netcdf
  implicit none

  integer, parameter :: nx = 4, ny = 7
  real :: data(nx, ny)
  type(file_type) :: nc
  type(variable_type) :: var

  call random_number(data)
  nc = dataset("simple_example.nc", "w")
  call def_dim(nc, ["x", "y"], [nx, ny])
  var = def_var(nc, "data", nc_float, ["x", "y"])
  call put_var(var, data)
  call close_dataset(nc)
end program main
```
### Read from NetCDF
```Fortran
program main
  use module_netcdf
  implicit none

  type(file_type) :: nc
  real, allocatable :: data(:,:)
  nc = dataset("simple_example.nc", "r")
  call get_var(nc, "data", data)
end program main
