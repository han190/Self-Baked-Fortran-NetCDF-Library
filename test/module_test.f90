module module_test

  use :: module_netcdf
  implicit none

contains

  function test_simple_xy_wr(path) result(succeed)
    character(len=*), intent(in) :: path
    logical :: succeed

    character(len=:), allocatable :: filename
    integer, parameter :: NX = 6
    integer, parameter :: NY = 12

    integer :: i, j, stat
    integer, target :: data_out(NX, NY)
    integer, pointer :: vals(:) => null()

    type(group_type) :: nc
    type(variable_type) :: var

    do concurrent (i=1:NX, j=1:NY)
      data_out(i, j) = i + (NX - 1)*j
    end do
    vals(1:NX*NY) => data_out

    filename = path//"simple_xy_wr.nc"
    nc = dataset(filename, "w")
    call def_dim(nc, ["x", "y"], [NX, NY])
    var = def_var(nc, "data", nc_int, ["x", "y"])
    call put_var(var, vals)

    call put_att(nc, "global attribute", "Dataset")
    call put_att(var, "unit", "Dummy Unit")
    call put_att(var, "add offset", 273.15)
    call put_att(var, "scale factor", 1.0e-6)
    stat = nc_close(nc%id)
    succeed = .true.
  end function test_simple_xy_wr

  function test_simple_xy_rd(path) result(succeed)
    character(len=*), intent(in) :: path
    logical :: succeed
    type(group_type) :: nc

    nc = dataset(path//"simple_xy_wr.nc", "r", &
      & inq_vars=.true., inq_atts=.true.)
    print "(dt)", nc
    succeed = .true.
  end function test_simple_xy_rd

end module module_test
