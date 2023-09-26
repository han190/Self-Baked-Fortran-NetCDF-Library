program main

use module_netcdf
implicit none

integer, parameter :: nx = 6, ny = 12
character(len=*), parameter :: path = "./data/", filename = "simple_xy_nc4.nc"
type(nc_var) :: dummy_var
real :: data(nx, ny)

! Fill data with random numbers.
call execute_command_line("mkdir -p "//path)
call random_number(data)

! Construct a data array and write to a netcdf file.
dummy_var = data_array(data, name="data", &
  & dims=dims(["x".dim.nx, "y".dim.ny]), &
  & atts=atts(["unit".att."dummy variable"]))
print *, dummy_var
call to_netcdf(dummy_var, path//filename)

end program main
