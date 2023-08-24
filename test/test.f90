program main

use module_netcdf
implicit none

integer, parameter :: nx = 6, ny = 12
character(len=*), parameter :: path = "./data/", filename = "simple_xy_nc4.nc"
type(variable_type) :: var
real :: data(nx, ny)

! Fill data with random numbers.
call execute_command_line("mkdir -p "//path)
call random_number(data)

! Construct a data array and write to a netcdf file.
var = data_array(data, name="data", &
  & dims=dims(["x".dim.nx, "y".dim.ny]))
call to_netcdf(var, path//filename)

end program main
