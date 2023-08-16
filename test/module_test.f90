module module_test

  use :: module_netcdf
  use :: iso_fortran_env, only: real64
  implicit none

contains

  function test_simple_xy_wr(path) result(succeed)
    character(len=*), intent(in) :: path
    logical :: succeed

    character(len=:), allocatable :: filename
    integer, parameter :: nlat = 361
    integer, parameter :: nlon = 1440
    integer, parameter :: nitme = 24
    integer, parameter :: nlvl = 5

    integer :: i, j, k, l, stat
    integer, target :: t2m(nlat, nlon)
    real(real64), target :: slp(nlat, nlon)
    real, target :: ght(nlat, nlon, nlvl, nitme)
    class(*), pointer :: vals(:)

    type(group_type) :: nc
    type(variable_type) :: var

    !> Fill arrays with some values
    t2m = 1
    slp = 2.0_real64
    ght = 3.0

    !> Open file, prepare to write
    filename = path//"simple_xy_wr.nc"
    nc = dataset(filename, "w")

    !> Define dimension
    call def_dim(nc, ["lat", "lon"], [nlat, nlon])

    !> Define temperature at 2 metre
    var = def_var(nc, "t2m", nc_int, ["lat", "lon"])
    vals(1:size(var)) => t2m !> Map 2d data to 1d pointer
    call put_var(var, vals)
    nullify (vals)
    call put_att(var, "units", "Kelvin")
    call put_att(var, "long_name", "temperature at 2 metre")
    call put_att(var, "add_offset", 1.0)
    call put_att(var, "scale_factor", 2.0_real64)

    !> Define sea level pressure
    var = def_var(nc, "slp", nc_double, ["lat", "lon"])
    vals(1:size(var)) => slp
    call put_var(var, vals)
    nullify (vals)
    call put_att(var, "units", "hPa")
    call put_att(var, "long_name", "sea level pressure")

    !> Add more dimensions
    call def_dim(nc, [character(len=4) :: "time", "lvl"], [nitme, nlvl])

    !> Define geopotential height
    var = def_var(nc, "ght", nc_float, &
      & [character(len=4) :: "lat", "lon", "lvl", "time"])
    vals(1:size(var)) => ght
    call put_var(var, vals)
    nullify (vals)
    call put_att(var, "units", "m2 s-2")
    call put_att(var, "long_name", "geopotential height")
    call put_att(var, "FillValue", 99999)

    !> Add global attribute
    call put_att(nc, "global attribute", "Dummy ERA5 Dataset")
    stat = nc_close(nc%id)
    nullify (vals)
    succeed = .true.
  end function test_simple_xy_wr

  function test_simple_xy_rd(path) result(succeed)
    character(len=*), intent(in) :: path
    logical :: succeed
    type(group_type) :: nc
    type(variable_type) :: var

    nc = dataset(path//"simple_xy_wr.nc", "r", &
      & inq_vars=.true., inq_atts=.true.)
    print "(dt)", nc
    var = inq_var(nc, "t2m")
    print "(dt)", var
    succeed = .true.
  end function test_simple_xy_rd

end module module_test
