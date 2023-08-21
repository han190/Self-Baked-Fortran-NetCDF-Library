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

  integer, parameter :: nlat = 4, nlon = 7
  real, allocatable :: raw(:, :)
  class(variable_type), allocatable :: var

  !> Fill data with random numbers.
  allocate (raw(nlat, nlon))
  call random_number(raw)

  !> Construct variable
  var = data_array(raw=raw, name="data", &
    & dims=["lat".dim.nlat, "lon".dim.nlon], &
    & atts=["units".att."degC", "add_offset".att.-273.16])

  !> Write to netcdf
  call execute_command_line("mkdir -p ./data/")
  call to_netcdf(var, "./data/simple_example.nc")
end program main
```
### Read from NetCDF
```Fortran
program main
  use module_netcdf
  implicit none

  real, allocatable :: raw(:, :)
  real :: add_offset
  type(file_type) :: nc
  class(variable_type), allocatable :: var
  class(attribute_type), allocatable :: add_offset

  !> Read from file
  nc = dataset("./data/simple_example.nc", "r")

  !> Inquire variable, and extract raw data
  var = get_var(nc, "data")
  add_offset = get_att(var, "add_offset")
end program main
```
