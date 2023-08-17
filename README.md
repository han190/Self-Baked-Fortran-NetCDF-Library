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
```
program main

  integer, parameter :: nlat = 361
  integer, parameter :: nlon = 1440
  integer, parameter :: nitme = 24
  integer, parameter :: nlvl = 40

  integer :: t2m(nlat, nlon)
  real(real64) :: slp(nlat, nlon)
  real :: ght(nlat, nlon, nlvl, nitme)

  type(group_type) :: nc
  type(variable_type) :: var

  !> Fill arrays with some values
  t2m = 1
  slp = 2.0_real64
  ght = 3.0

  !> Open file, prepare to write
  filename = "dummy_era5.nc"
  nc = dataset(filename, "w")

  !> Define dimension
  call def_dim(nc, ["lat", "lon"], [nlat, nlon])
  !> Add more dimensions
  call def_dim(nc, [character(len=4) :: "time", "lvl"], [nitme, nlvl])

  var = def_var(nc, "lat", nc_float, ["lat"])
  call put_att(var, "units", "deg")
  call put_att(var, "long_name", "latitude")

  var = def_var(nc, "lon", nc_float, ["lon"])
  call put_att(var, "units", "deg")
  call put_att(var, "long_name", "longitude")

  var = def_var(nc, "time", nc_float, ["time"])
  call put_att(var, "units", "hour")
  call put_att(var, "long_name", "hour in a day")

  var = def_var(nc, "level", nc_float, ["lvl"])
  call put_att(var, "units", "hPa")
  call put_att(var, "long_name", "pressure level")

  !> Define temperature at 2 metre
  var = def_var(nc, "t2m", nc_int, ["lat", "lon"])
  call put_var(var, t2m)
  call put_att(var, "units", "Kelvin")
  call put_att(var, "long_name", "temperature at 2 metre")
  call put_att(var, "add_offset", 1.0)
  call put_att(var, "scale_factor", 2.0_real64)

  !> Define sea level pressure
  var = def_var(nc, "slp", nc_double, ["lat", "lon"])
  call put_var(var, slp)
  call put_att(var, "units", "hPa")
  call put_att(var, "long_name", "sea level pressure")

  !> Define geopotential height
  var = def_var(nc, "ght", nc_float, &
    & [character(len=4) :: "lat", "lon", "lvl", "time"])
  call put_var(var, ght)
  call put_att(var, "units", "m2 s-2")
  call put_att(var, "long_name", "geopotential height")
  call put_att(var, "FillValue", 99999)

  !> Add global attribute
  call put_att(nc, "global attribute", "Dummy ERA5 Dataset")
  stat = nc_close(nc%id)
  call close_dataset(nc)

end program main
```
