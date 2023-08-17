module module_test

  use :: module_netcdf
  use :: iso_fortran_env, only: real64
  implicit none

contains

  function test_write(path) result(succeed)
    character(len=*), intent(in) :: path
    logical :: succeed

    character(len=:), allocatable :: filename
    integer, parameter :: nlat = 361
    integer, parameter :: nlon = 1440
    integer, parameter :: ntime = 24
    integer, parameter :: nlvl = 5

    integer :: t2m(nlat, nlon)
    real(real64) :: slp(nlat, nlon)
    real :: ght(nlat, nlon, nlvl, ntime)

    type(file_type) :: nc
    type(variable_type) :: var

    !> Fill arrays with some values
    t2m = 1
    slp = 2.0_real64
    ght = 3.0

    !> Open file, prepare to write
    filename = path//"dummy.nc"
    nc = dataset(filename, "w")

    !> Define dimension
    call def_dim(nc, &
      & [character(len=4) :: "lat", "lon", "time", "lvl"], &
      & [nlat, nlon, ntime, nlvl])

    !> Define latitude
    var = def_var(nc, "latitude", nc_float, ["lat"])
    call put_att(var, "units", "deg")
    call put_att(var, "long_name", "latitude")

    !> Define longitude
    var = def_var(nc, "longitude", nc_float, ["lon"])
    call put_att(var, "units", "deg")
    call put_att(var, "long_name", "longitude")

    !> Define time
    var = def_var(nc, "time", nc_int, ["time"])
    call put_att(var, "units", "hour")
    call put_att(var, "long_name", "hour in a day")

    var = def_var(nc, "level", nc_float, ["lvl"])
    call put_att(var, "units", "hPa")
    call put_att(var, "long_name", "pressure level")

    !> Define temperature at 2 metre
    var = def_var(nc, "T2M", nc_int, ["lat", "lon"])
    call put_var(var, t2m)
    call put_att(var, "units", "Kelvin")
    call put_att(var, "long_name", "temperature at 2 metre")
    call put_att(var, "add_offset", 1.0)
    call put_att(var, "scale_factor", 2.0_real64)

    !> Define sea level pressure
    var = def_var(nc, "SLP", nc_double, ["lat", "lon"])
    call put_var(var, slp)
    call put_att(var, "units", "hPa")
    call put_att(var, "long_name", "sea level pressure")

    !> Define geopotential height
    var = def_var(nc, "GHT", nc_float, &
      & [character(len=4) :: "lat", "lon", "lvl", "time"])
    call put_var(var, ght)
    call put_att(var, "units", "m2 s-2")
    call put_att(var, "long_name", "geopotential height")
    call put_att(var, "FillValue", 99999)

    !> Add global attribute
    call put_att(nc, "created by", "Self baked NetCDF")
    call put_att(nc, "description", "Dummy ERA5 dataset")
    call close_dataset(nc)
    succeed = .true.
  end function test_write

  function test_read(path) result(succeed)
    character(len=*), intent(in) :: path
    logical :: succeed
    type(file_type) :: nc
    type(variable_type) :: var

    nc = dataset(path//"dummy.nc", "r")
    var = inq_var(nc, "T2M")
    print "(dt)", nc
    succeed = .true.
  end function test_read

end module module_test
