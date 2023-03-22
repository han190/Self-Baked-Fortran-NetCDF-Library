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
real(real64), allocatable :: arr(:, :, :)

nc = dataset("./data/sresa1b_ncar_ccsm3-example.nc", "r")
var = get_var(nc, "tas")
print "(a, *(i0, 1x))", "shape(var): ", shape(var)

end program main
