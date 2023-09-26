# Self-baked NetCDF Fortran Library

The Self-baked NetCDF Fortran library is a light weighted NetCDF C Library wrapper and an intermediate interface written in modern Fortran. This project is designed as a temporary workaround of the current fpm/NetCDF incoherence (see [fpm discussion](https://github.com/fortran-lang/fpm/discussions/458), [fpm issue](https://github.com/fortran-lang/fpm/issues/17), [discussion on discourse](https://fortran-lang.discourse.group/t/using-netcdf-with-fpm/4225), [suggestions from Richard Weed](https://github.com/Unidata/netcdf-fortran/issues/153)). The major difference between this library and most of the other implementations (for example [nc4fortran](https://github.com/geospace-code/nc4fortran)) is that this library is built on top of NetCDF C library directly, thus you won't need the official NetCDF Fortran library as a dependence. If you are intereseted in using this project with [fpm](https://github.com/fortran-lang/fpm), add the following lines in your `fpm.toml`
```
[dependencies]
self-baked-nf = {git="https://github.com/han190/Self-Baked-Fortran-NetCDF"}

[build]
link = "netcdf"
```

## Examples
### Write to NetCDF
#### Write a single variable to NetCDF
```Fortran
program main

use module_netcdf
implicit none

integer, parameter :: nx = 6, ny = 12
character(len=*), parameter :: path = "./data/", filename = "simple_xy_nc4.nc"
type(nc_var) :: dummy_var
real :: data(nx, ny)

! Fill data with random numbers.
call execute_command_line("mkdir -p "//path)
call random_number(data)

! Construct a data array and write to a netcdf file.
dummy_var = data_array(data, name="data", &
  & dims=dims(["x".dim.nx, "y".dim.ny]), &
  & atts=atts(["descriptions".att."dummy variable"]))
print *, dummy_var
call to_netcdf(dummy_var, path//filename)

end program main
```

## Miscellaneous
To count the LOC for this project
```
cloc --force-lang-def=./misc/language_definitions.txt --exclude-dir=backups,src .
```