program main

use module_netcdf
implicit none

type(variable_type) :: var
type(group_type) :: nc
real, allocatable :: t2m(:, :, :)
character(:), allocatable :: file

file = "t2m_2023_01"
nc = dataset("./data/"//file//".nc", "r")
t2m = get_var(nc, "t2m")
call close_dataset(nc)

end program main
