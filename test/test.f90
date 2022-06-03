program main

  use netcdf_group_module
  implicit none

  type(netcdf_dataset) :: dataset

  call dataset%open("./sample/ecmwf.nc", "r")

end program main
