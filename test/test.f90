program main

use module_netcdf
use iso_fortran_env
implicit none

type(group_type) :: nc
type(variable_type) :: var
integer :: i

nc = dataset("./data/t2m_2023_01.nc", "r")
var = get_var(nc, "t2m")

do i = 1, size(var%attributes)
  select type (values_ => var%attributes(i)%values)
  type is (character(*))
    print "(a, ':', 1x, a)", var%attributes(i)%name, values_
  type is (integer(int16))
    print "(a, ':', 1x, i0)", var%attributes(i)%name, values_
  type is (integer(int32))
    print "(a, ':', 1x, i0)", var%attributes(i)%name, values_
  type is (integer(int64))
    print "(a, ':', 1x, i0)", var%attributes(i)%name, values_
  type is (real(real32))
    print "(a, ':', 1x, f0.6)", var%attributes(i)%name, values_
  type is (real(real64))
    print "(a, ':', 1x, f0.6)", var%attributes(i)%name, values_
  end select
end do

select type (container_ => var%container)
type is (container_3d)

  select type (data_ => container_%data)
  type is (integer(int16))
    print *, shape(data_)
  end select

end select

call close(nc)

end program main
