module module_constant

implicit none
public

!> Data type
integer, parameter :: nc_byte = 1
integer, parameter :: nc_char = 2
integer, parameter :: nc_short = 3
integer, parameter :: nc_int = 4
integer, parameter :: nc_float = 5
integer, parameter :: nc_double = 6
integer, parameter :: nc_int64 = 10

!> Flags
integer, parameter :: nc_nowrite = 0
integer, parameter :: nc_noerr = 0

end module module_constant
