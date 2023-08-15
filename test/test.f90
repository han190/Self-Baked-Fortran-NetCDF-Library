program main

  use module_netcdf
  use iso_fortran_env, only: int16, real64, int64
  implicit none

  type(group_type) :: nc
  type(variable_type) :: var

  integer(int16), allocatable :: raw(:)
  integer(int64) :: s(3)
  real(real64) :: scale_factor, add_offset
  real, allocatable, target :: vals(:)
  real, pointer :: ptr(:,:,:) => null()

  !> Only dimension will loaded unless optional
  !> arguments are specified
  nc = dataset("./data/sample02.nc", "r")

  !> Inquire variable metadata 
  !> (name, id, dimension, attribute, etc.)
  var = inq_var(nc, "t2m")

  !> User-defined Derived Type I/O
  print *, var

  !> Extract raw data and by convention
  !> scale factor and add offset.
  call get_var(var, raw)
  call get_att(var, "scale_factor", scale_factor)
  call get_att(var, "add_offset", add_offset)

  !> Map the data to a 3-dimensional pointer.
  vals = raw*scale_factor + add_offset
  s = shape(var)
  ptr(1:s(1), 1:s(2), 1:s(3)) => vals
  print *, "Data successfully read."

end program main
