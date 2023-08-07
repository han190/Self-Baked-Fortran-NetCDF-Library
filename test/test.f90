program main

  use module_netcdf
  use iso_fortran_env
  implicit none

  type(group_type) :: nc ! netcdf
  type(variable_type) :: var
  real, allocatable, target :: var_data(:)
  real, pointer :: var_ptr(:,:,:,:)

  nc = dataset("./data/sresa1b_ncar_ccsm3-example.nc", "r")
  var = get_var(nc, "ua")
  var_data = extract(var)
  associate (s => shape(var))
    print *, s
    var_ptr(1:s(1),1:s(2),1:s(3),1:s(4)) => var_data
  end associate

  call close(nc)

end program main
