# Self-baked NetCDF Fortran library

## Get started
The self-baked NetCDF Fortran library is a light weighted NetCDF C Library wrapper and an intermediate interface written in modern Fortran. The major difference between this library and most of the other implementations (for example [nc4fortran](https://github.com/geospace-code/nc4fortran)) is that this library is built on top of netcdf-c library directly. If you build your project with [fpm](https://github.com/fortran-lang/fpm), add the following lines in your `fpm.toml`
```
[dependencies]
self-baked-nc = {git="https://github.com/han190/Self-Baked-Fortran-NetCDF"}

[build]
link = "netcdf"
```
and you are ready to use `module_netcdf`.

## Quick tutorial
Here is an example

```fortran
program main
  use module_netcdf
  implicit none

  type(group_type) :: nc
  type(variable_type) :: var

  nc = dataset("sample.nc", "r")
  print *, nc
  var = get_var(nc, "variable_name")
end program main
```

Note that `get_var` does not really read data from the NC dataset, but only the meta data (for example, name, type and dimensions of the variable). To actually "get" the data, you need the subroutine `extract`.

```fortran
!> Fortran2023
real, allocatable, target :: var_data(:)
integer :: var_rank

call extract(var, var_data)
var_rank = rank(var)

block
  real, rank(var_rank), pointer :: var_ptr => null()

  var_ptr(@shape(var)) => var
end block
```

