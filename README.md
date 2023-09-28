# Self-baked NetCDF Fortran Library

The Self-baked NetCDF Fortran library is a light weighted NetCDF C Library wrapper and an intermediate interface written in modern Fortran. This project is designed as a temporary workaround of the current fpm/NetCDF incoherence (see [fpm discussion](https://github.com/fortran-lang/fpm/discussions/458), [fpm issue](https://github.com/fortran-lang/fpm/issues/17), [discussion on discourse](https://fortran-lang.discourse.group/t/using-netcdf-with-fpm/4225), [suggestions from Richard Weed](https://github.com/Unidata/netcdf-fortran/issues/153)). The major difference between this library and most of the other implementations (for example [nc4fortran](https://github.com/geospace-code/nc4fortran)) is that this library is built on top of NetCDF C library directly, thus you won't need the official NetCDF Fortran library as a dependence. If you are intereseted in using this project with [fpm](https://github.com/fortran-lang/fpm), add the following lines to your `fpm.toml`
```
[dependencies]
self-baked-nf = {git="https://github.com/han190/Self-Baked-Fortran-NetCDF"}
[build]
link = "netcdf"
```

## Examples
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