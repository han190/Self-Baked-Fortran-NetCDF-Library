program main

use module_netcdf
implicit none

type(dimension_type), allocatable :: dimn(:)
type(attribute_type), allocatable :: attr(:)
character(len=:), allocatable :: iomsg

dimn = dims(["lat".dim.361, "lon".dim.1440, "time".dim.1], unlim_dim=3)
iomsg = "inline"
write (*, "(*(dt))", iomsg=iomsg) dimn

attr = atts([ &
  & "long_name".att."temperature at 2 metre", &
  & "units".att."degC", &
  & "add_offset".att.-273.16, &
  & "scale_factor".att.1e-3])
write (*, "(*(dt(1)))") attr

end program main
