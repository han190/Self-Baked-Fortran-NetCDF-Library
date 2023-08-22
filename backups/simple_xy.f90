program main
  use module_netcdf
  implicit none

  integer, parameter :: nx = 4, ny = 7
  real, allocatable, target :: raw(:)
  real, pointer :: ptr(:,:)
  type(file_type) :: nc
  type(variable_type) :: var

  allocate (raw(nx*ny))
  call random_number(raw)
  ptr(1:nx, 1:ny) => raw
  call execute_command_line("mkdir -p ./data/")
  
  nc = dataset("./data/simple_example.nc", "w")
  call def_dim(nc, ["x", "y"], [nx, ny])
  var = def_var(nc, "data", nc_float, ["x", "y"])
  call put_var(var, ptr)
  call close_dataset(nc)

  nc = dataset("./data/simple_example.nc", "r")
  call get_var(nc, "data", raw)
  ptr(1:nx, 1:ny) => raw
  nullify (ptr)
end program main