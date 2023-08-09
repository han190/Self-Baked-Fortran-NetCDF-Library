program main

  use module_netcdf
  use iso_fortran_env
  implicit none

  type(group_type) :: nc ! netcdf
  type(variable_type) :: var
  real, allocatable, target :: var_data(:)
  real, pointer :: var_ptr(:,:,:)

  nc = dataset("./data/sresa1b_ncar_ccsm3-example.nc", "r")
  var = get_var(nc, "pr")
  print *, var
  
  call extract(var, var_data)
  call close_dataset(nc)

end program main