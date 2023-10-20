# Self-baked NetCDF Fortran Library

The Self-baked NetCDF Fortran library is a light weighted NetCDF C Library wrapper and an intermediate interface written in modern Fortran. This project is designed as a temporary workaround of the current fpm/NetCDF incoherence (see [fpm discussion](https://github.com/fortran-lang/fpm/discussions/458), [fpm issue](https://github.com/fortran-lang/fpm/issues/17), [discussion on discourse](https://fortran-lang.discourse.group/t/using-netcdf-with-fpm/4225), [suggestions from Richard Weed](https://github.com/Unidata/netcdf-fortran/issues/153)). Thus, you are welcome to use and contribute to this project, but _use at your own risk_.

The major difference between this library and most of the other implementations (for example [nc4fortran](https://github.com/geospace-code/nc4fortran)) is that this library is built on top of NetCDF C library directly. If you are intereseted in using this project with [fpm](https://github.com/fortran-lang/fpm), add the following lines to your `fpm.toml`
```
[dependencies]
self-baked-nf = {git="https://github.com/han190/Self-Baked-Fortran-NetCDF"}
[build]
link = "netcdf"
```

## Examples of the intermediate interface
### Write to a NetCDF file
#### Write a single variable to a NetCDF file
```Fortran
program main

use module_netcdf
implicit none

integer, parameter :: nx = 3, ny = 4
character(len=*), parameter :: filename = "simple_xy.nc"
type(netcdf_variable) :: var
real :: raw(nx, ny)

! Fill an array with random numbers.
call random_number(raw)

! Construct a data array and write to a NetCDF file.
var = data_array(raw, "data", dims=["x".dim.nx, "y".dim.ny])
call to_netcdf(var, filename)

end program main
```
#### Write multiple variables to a NetCDF file
```fortran
program main

use module_netcdf
implicit none

integer, parameter :: nx = 4, ny = 3, nz = 2, nt = 1
type(netcdf_variable) :: geopt, temp, slp
real :: geopt_raw(nx, ny, nz)
integer :: temp_raw(nx, ny, nt)
double precision :: slp_raw(nx, ny)
real :: nan

nan = ieee_value(0.0, ieee_quiet_nan)
call random_number(geopt_raw)
geopt = data_array(geopt_raw, "geopt", &
  & dims=["latitude".dim.nx, "longitude".dim.ny, "level".dim.nz], &
  & atts=["long_name".att."geopotenail", "_FillValue".att.nan])

temp_raw = 1
temp = data_array(temp_raw, "temp", &
  & dims=["latitude".dim.nx, "longitude".dim.ny, "time".dim.nt], &
  & atts=["long_name".att."temperature", "add_offset".att.-273.15, "scale_factor".att.1.0])

call random_number(slp_raw)
slp = data_array(slp_raw, "slp", &
  & dims=["latitude".dim.nx, "longitude".dim.ny], &
  & atts=["long_name".att."sea level pressure", "missing_value".att.-2147483647])

print "(*(dt))", geopt, temp, slp
call to_netcdf([geopt, temp, slp], "multiple_vars.nc")
end program main
```
### Read from a NetCDF file
#### Read a single variable from a NetCDF file
```Fortran
program main

use module_netcdf
implicit none

character(len=*), parameter :: filename = "simple_xy.nc"
type(netcdf_variable) :: var
real, pointer :: raw(:, :) => null()

! Read from NetCDF and extract data
var = from_netcdf(filename, "data")
call extract(var, raw)
end program main
```

## Miscellaneous
To count the LOC for this project
```
cloc --force-lang-def=./misc/language_definitions.txt --exclude-dir=backups,src,doc,build .
```
To generate and compile source code
```
./compile
```