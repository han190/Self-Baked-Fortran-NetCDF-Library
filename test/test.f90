program main

use module_netcdf
use module_interface
use module_constant
use iso_fortran_env
implicit none

type(group_type) :: nc
type(variable_type) :: var
integer :: i
real(real64) :: add_offset, scale_factor
class(*), allocatable :: arr(:, :, :)

nc = dataset("./data/t2m_2023_01.nc", "r")
var = get_var(nc, "t2m")
call extract(var, arr)

end program main
