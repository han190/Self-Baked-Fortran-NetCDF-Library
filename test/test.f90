program main

use module_netcdf
implicit none

integer, parameter :: nx = 3, ny = 4
type(variable_type) :: var
real, allocatable :: data(:, :)

! Fill data with random numbers.
allocate (data(nx, ny))
call random_number(data)

! Construct a data array and write to a netcdf file.
var = data_array(data, name="data", &
  & dims=dims(["x".dim.nx, "y".dim.ny]))
! call to_netcdf(var, "dummy.nc")

end program main
