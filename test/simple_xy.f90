program main
  use module_netcdf
  implicit none

  integer, parameter :: nx = 4, ny = 7
  real :: data(nx, ny)
  type(file_type) :: nc
  type(variable_type) :: var

  call random_number(data)
  nc = dataset("simple_example.nc", "w")
  call def_dim(nc, ["x", "y"], [nx, ny])
  var = def_var(nc, "data", nc_float, ["x", "y"])
  call put_var(var, data)
  call close_dataset(nc)
end program main