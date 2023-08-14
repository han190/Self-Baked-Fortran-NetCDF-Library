program main

  use module_netcdf
  implicit none

  type(group_type) :: nc
  nc = dataset("./data/sample02.nc", "r")
  print "(dt)", nc

end program main

