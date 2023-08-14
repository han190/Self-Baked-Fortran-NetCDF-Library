program main

  use module_netcdf
  use iso_fortran_env, only: int16, real64
  implicit none

  type(group_type) :: nc
  type(variable_type) :: var
  integer(int16), allocatable :: vals(:)
  real(real64) :: scale_factor, add_offset

  nc = dataset("./data/sample02.nc", "r")
  var = inq_var(nc, "t2m")
  print "(dt)", var

  call get_var(var, vals)
  call get_att(var, "scale_factor", scale_factor)
  call get_att(var, "add_offset", add_offset)

  print *, scale_factor, add_offset
  associate (x => vals*scale_factor + add_offset)
    print *, x(1:5)
  end associate

end program main

