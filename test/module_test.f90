module module_test

use module_netcdf
implicit none
contains

subroutine simple_xy()
  integer, parameter :: nx = 6, ny = 12
  character(len=*), parameter :: path = "./data/", filename = "simple_xy_nc4.nc"
  type(nc_var) :: dummy_var
  real :: data(nx, ny)

  ! Fill data with random numbers.
  call execute_command_line("mkdir -p "//path)
  call random_number(data)

  ! Construct a data array and write to a netcdf file.
  dummy_var = data_array(data, name="data", &
    & dims=dims(["x".dim.nx, "y".dim.ny]), &
    & atts=atts(["description".att."dummy variable"]))
  call to_netcdf(dummy_var, path//filename)
  print "(a)", "Successfully generated 'simple_xy.nc'."
end subroutine simple_xy

end module module_test
