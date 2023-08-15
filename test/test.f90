program main

  use module_netcdf
  use iso_fortran_env, only: int16, real64
  implicit none

  type(group_type) :: nc
  type(variable_type) :: var
  integer(int16), allocatable :: raw(:)
  real(real64) :: scale_factor, add_offset
  real, allocatable, target :: vals(:)
  real, pointer :: ptr(:,:,:) => null()

  nc = dataset("./data/sample02.nc", "r")
  var = inq_var(nc, "t2m")
  print "(dt)", var

  call get_var(var, raw)
  call get_att(var, "scale_factor", scale_factor)
  call get_att(var, "add_offset", add_offset)

  vals = raw*scale_factor + add_offset
  associate (s => shape(var))
    ptr(1:s(1), 1:s(2), 1:s(3)) => vals
  end associate

end program main
