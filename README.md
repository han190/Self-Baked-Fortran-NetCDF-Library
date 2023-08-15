# Self-baked NetCDF Fortran library

## Get started
The self-baked NetCDF Fortran library is a light weighted NetCDF C Library wrapper and an intermediate interface written in modern Fortran. The major difference between this library and most of the other implementations (for example [nc4fortran](https://github.com/geospace-code/nc4fortran)) is that this library is built on top of netcdf-c library directly. If you build your project with [fpm](https://github.com/fortran-lang/fpm), add the following lines in your `fpm.toml`
```
[dependencies]
self-baked-nf = {git="https://github.com/han190/Self-Baked-Fortran-NetCDF"}

[build]
link = "netcdf"
```
and you are ready to use `module_netcdf`.

## Quick tutorial
Here is an example

```Fortran
program main

  use module_netcdf
  use iso_fortran_env, only: int16, real64, int64
  implicit none

  type(group_type) :: nc
  type(variable_type) :: var
  integer(int16), allocatable :: raw(:)
  real(real64) :: scale_factor, add_offset
  real, allocatable, target :: vals(:)
  integer(int64) :: r

  nc = dataset("./data/sample02.nc", "r")
  var = inq_var(nc, "t2m")
  print "(dt)", var

  call get_var(var, raw)
  call get_att(var, "scale_factor", scale_factor)
  call get_att(var, "add_offset", add_offset)
  vals = raw*scale_factor + add_offset

  r = rank(var)
  block
    real, rank(r), pointer :: ptr => null()
    
    associate (l => [(1, i=1, r)], u => shape(var))
      ptr(@l:u) => vals
    end associate
  end block

end program main
```