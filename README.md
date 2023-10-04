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

character(len=*), parameter :: filename = "simple_xy.nc"
type(nc_var) :: var
real, target :: raw(3, 4)
class(*), pointer :: ptr(:)

! Fill an array with random numbers, and
! map the array to a 1D pointer
call random_number(raw)
ptr(1:3*4) => raw 

! Construct a data array and write to a NetCDF file.
var = data_array(ptr, name="data", &
  & dims=dims(["x".dim.3, "y".dim.4]))
call to_netcdf(var, filename)
nullify (ptr)

end program main
```
#### Write multiple variables to a NetCDF file
```fortran
program main

use module_netcdf
implicit none

integer, parameter :: nx = 4, ny = 3, nz = 2, nt = 1
type(nc_var) :: geopt, temp, slp
real, target :: geopt_raw(nx, ny, nz)
double precision, target :: temp_raw(nx, ny, nt), slp_raw(nx, ny)
class(*), pointer :: ptr(:)

call random_number(geopt_raw)
ptr(1:size(geopt_raw)) => geopt_raw
geopt = data_array(ptr, "geopt", &
  & dims=dims(["latitude".dim.nx, "longitude".dim.ny, "level".dim.nz]), &
  & atts=atts(["long_name".att."geopotenail"]))
nullify (ptr)

call random_number(temp_raw)
ptr(1:size(temp_raw)) => temp_raw
temp = data_array(ptr, "temp", &
  & dims=dims(["latitude".dim.nx, "longitude".dim.ny, "time".dim.nt]), &
  & atts=atts(["long_name".att."temperature"]))
nullify (ptr)

call random_number(slp_raw)
ptr(1:size(slp_raw)) => slp_raw
slp = data_array(ptr, "slp", &
  & dims=dims(["latitude".dim.nx, "longitude".dim.ny]), &
  & atts=atts(["long_name".att."sea level pressure"]))
nullify (ptr)

! print "(dt)", geopt, temp, slp
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
type(nc_var), target :: var
real, pointer :: ptr(:, :)

! Read from NetCDF
var = from_netcdf(filename, "data")

! Select type and then map the 1d 
! unlimited polymorphic to a 2d array.
select type (vals => var%vals)
type is (real)
  associate (s => shape(var))
    ptr(1:s(1), 1:s(2)) => vals
  end associate
end select
nullify (ptr)

end program main
```

## Miscellaneous
To count the LOC for this project
```
cloc --force-lang-def=./misc/language_definitions.txt --exclude-dir=backups,src .
```
To generate and compile source code
```
./compile
```