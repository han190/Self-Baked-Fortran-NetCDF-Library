program main

use module_netcdf
implicit none

integer, parameter :: nx = 3, ny = 4
type(variable_type) :: var
real, allocatable :: data(:, :)

! Fill data with random numbers.
allocate (data(nx, ny))
call random_number(data)

! Construct a data array.
var = data_array(data, name="data", &
  & dims=dims(["x".dim.nx, "y".dim.ny]), &
  & atts=atts(["comment".att."dummy variable"]))
print "(dt)", var

! Write to netcdf
! call to_netcdf(var, "dummy.nc")

end program main
