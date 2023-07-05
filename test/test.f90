program main

use module_netcdf
use iso_fortran_env
implicit none

type(group_type) :: nc
real, allocatable :: t2m(:, :, :)

nc = dataset("./data/t2m_2023_01.nc", "r")
t2m = extract(nc, "t2m")
call close(nc)

print *, t2m(1, 1:5, 1)

end program main
