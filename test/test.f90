program main

use module_netcdf
implicit none

type(dimension_type), allocatable :: dimn(:)
type(attribute_type), allocatable :: attr(:)

dimn = dims(["lat".dim.361, "lon".dim.1440])
attr = atts(["units".att."degC", "long_name".att."temperature at 2 metre"])

end program main
