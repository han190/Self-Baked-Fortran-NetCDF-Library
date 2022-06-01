program main

  use netcdf_module, only: netcdf_dataset
  implicit none

  type(netcdf_dataset) :: ds
  integer :: iunit, iostat
  character(:), allocatable :: filename
  character(:), allocatable :: iomsg

  filename = "./sample/ecmwf.nc"
  open (newunit=iunit, file=filename, action="read")
  iomsg = "nothing"
  read (iunit, *, iomsg=iomsg) ds

  print *, ds%natt
  print *, ds%status
  print *, ds%mode
  print *, ds%ncid
  print *, ds%ndim
  print *, ds%nvar
  print *, ds%natt
  print *, ds%unlimited_dimid
  print *, ds%format_num
  print *, ds%ndims
  print *, ds%dimids
  print *, ds%include_parents

  close (iunit)

end program main
