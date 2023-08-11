program main

  use module_netcdf
  use iso_fortran_env
  implicit none

  type(group_type), allocatable :: nc ! netcdf
  type(variable_type) :: var
  real, allocatable, target :: var_data(:)

  nc = dataset("./data/sample01.nc", "r")
  var = get_var(nc, "pr")
  print "(dt)", nc
  print "(dt)", var
  
  call extract(var, var_data)
  deallocate (nc)
  ! call close_dataset(nc)

end program main