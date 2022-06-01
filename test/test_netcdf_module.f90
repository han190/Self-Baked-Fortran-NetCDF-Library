program main

  use netcdf_module
  implicit none

  type(netcdf_file) :: nc
  integer :: iunit

  open(newunit=iunit, file="./sample/ecmwf.nc", action="read")
  read(iunit, *) nc
  close (iunit)

end program main
