program main

use module_netcdf
use module_cwrapper
use iso_fortran_env
implicit none

type(group_type) :: nc
type(variable_type) :: var, lon, lat, time
integer :: i

nc = dataset("./data/t2m_2023_01.nc", "r")
var = get_var(nc, "t2m")

print *, nc_show_metadata(nc%id)
print "('Variable:', 1x, a)", var%name
print "(2x, a)", "Dimension:"
print "(4x, *(a, ':', 1x, i0, 1x))", (var%dimensions(i - 1)%name, var%dimensions(i - 1)%length, i=1, size(var%dimensions))

print "(2x, a)", "Attribute:"
do i = 1, size(var%attributes)
  select type (values_ => var%attributes(i)%values)
  type is (character(*))
    print "(4x, a, ':', 1x, a)", var%attributes(i)%name, values_
  type is (integer(int16))
    print "(4x, a, ':', 1x, i0)", var%attributes(i)%name, values_
  type is (integer(int32))
    print "(4x, a, ':', 1x, i0)", var%attributes(i)%name, values_
  type is (integer(int64))
    print "(4x, a, ':', 1x, i0)", var%attributes(i)%name, values_
  type is (real(real32))
    print "(4x, a, ':', 1x, f0.6)", var%attributes(i)%name, values_
  type is (real(real64))
    print "(4x, a, ':', 1x, f0.6)", var%attributes(i)%name, values_
  end select
end do

select type (container_ => var%container)
type is (container_3d)

  select type (data_ => container_%data)
  type is (integer(int16))
    print *, shape(data_)
    print *, data_(1:5, 1, 1)*.001275 + 265.876300 - 273.16
  end select

end select

call close(nc)

end program main
