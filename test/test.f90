program main

use module_netcdf
implicit none

type(variable_type) :: var
real, allocatable :: data(:, :)

allocate (data(3, 4))
call random_number(data)
var = data_array(data, name="data", &
  & dims=dims(["x".dim.3, "y".dim.4]))

end program main
