module module_test

use :: module_netcdf
use :: ieee_arithmetic
implicit none

integer, parameter :: nx = 3, ny = 5
character(len=*), parameter :: path = "./data/"
character(len=*), parameter :: filename = "simple_xy.nc"

contains

subroutine simple_xy_wr()
  type(nc_var) :: dummy_var
  type(nc_dim), allocatable :: dummy_dims(:)
  type(nc_att), allocatable :: dummy_atts(:)
  real, target :: raw(nx, ny)
  class(*), pointer :: dummy_data(:)
  real :: nan
  integer :: i

  nan = ieee_value(0.0, ieee_quiet_nan)
  ! Fill raw with random numbers.
  call execute_command_line("mkdir -p "//path)
  call random_number(raw)
  raw(2, 2) = nan
  dummy_data(1:size(raw)) => raw

  ! Construct a raw array and write to a netcdf file.
  dummy_dims = dims(["x".dim.nx, "y".dim.ny])
  dummy_atts = atts([ &
      & "long_name".att."dummy variable", &
      & "_FillValue".att.nan])
  dummy_var = data_array(dummy_data, name="dummy_var", &
    & dims=dummy_dims, atts=dummy_atts)

  ! Write to netcdf.
  call to_netcdf(dummy_var, path//filename)
  nullify (dummy_data)
  
  print "(dt)", dummy_var
  print "(a)", "values = "
  do i = 1, size(raw, 2)
    print "(3(f10.4, 1x))", raw(:, i)
  end do

  associate (str => "Successfully write to 'simple_xy.nc'.")
    print "(a)", repeat("=", len(str))
    print "(a)", str
    print "(a)", repeat("=", len(str)), new_line("(a)")
  end associate
end subroutine simple_xy_wr

subroutine simple_xy_rd()
  type(nc_var), target :: dummy_var
  real, pointer :: dummy_data(:, :)
  integer :: i

  dummy_var = from_netcdf(path//filename, "dummy_var")
  select type (v => dummy_var%vals)
  type is (real(real32))
    associate (s => shape(dummy_var))
      dummy_data(1:s(1), 1:s(2)) => v
    end associate
  end select

  print "(dt)", dummy_var
  print "(a)", "values = "
  do i = 1, size(dummy_data, 2)
    print "(3(f10.4, 1x))", dummy_data(:, i)
  end do

  associate (str => "Successfully read from 'simple_xy.nc'.")
    print "(a)", repeat("=", len(str))
    print "(a)", str
    print "(a)", repeat("=", len(str)), new_line("(a)")
  end associate
end subroutine simple_xy_rd

end module module_test
