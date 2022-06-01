program main

  use netcdf_module
  implicit none

  type(netcdf_file) :: nc
  integer :: iunit
  character(:), allocatable :: filename

  filename = "./sample/ecmwf.nc"
  open (newunit=iunit, file=filename, action="read")
  read (iunit, *) nc
  close (iunit)

end program main
