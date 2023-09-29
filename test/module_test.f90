module module_test

use :: module_netcdf
use :: module_data_structure
use :: ieee_arithmetic
implicit none

integer, parameter :: nx = 3, ny = 5, nz = 4, nt = 1
character(len=*), parameter :: path = "./data/"
character(len=*), parameter :: filename = "single_var.nc"

contains

subroutine data_structure()
  type(dict_type) :: physicists_dict
  type(pair_type), allocatable :: physicists_pairs(:)
  character(len=:), allocatable :: physicists(:), physicist
  integer :: i, n

  physicists = [character(len=30) :: &
    & "Albert Einstein", "Isaac Newton", "James Maxwell", "Galileo Galilei"]
  physicists_dict = new_dict()

  do i = 1, size(physicists)
    call append(physicists_dict, trim(physicists(i)) .pair.i)
  end do
  physicists_pairs = to_array(physicists_dict)
  print "(a)", "Unsorted pair array = "
  do i = 1, size(physicists_pairs)
    print "(2x, a, t30, i0)", physicists_pairs(i)%key, &
      & physicists_pairs(i)%val
  end do
  print "(a)", ""

  print "(a)", "Scan results = "
  do i = 1, size(physicists)
    physicist = trim(physicists(i))
    n = scan(physicists_dict, physicist)
    print "(2x, a, t30, i0)", physicist, n
  end do

  associate (someone => "Nicolaus Copernicus")
    n = scan(physicists_dict, someone)
    if (n == invalid) then
      print "(a)", someone//" not found."
    else
      print "(a)", someone//" found."
    end if
  end associate

  associate (str => "Successfully test customized data structures.")
    print "(a)", repeat("=", len(str))
    print "(a)", str
    print "(a)", repeat("=", len(str)), new_line("(a)")
  end associate
end subroutine data_structure

subroutine single_var_wr()
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

  associate (str => "Successfully write to 'single_var.nc'.")
    print "(a)", repeat("=", len(str))
    print "(a)", str
    print "(a)", repeat("=", len(str)), new_line("(a)")
  end associate
end subroutine single_var_wr

subroutine single_var_rd()
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

  associate (str => "Successfully read from 'single_var.nc'.")
    print "(a)", repeat("=", len(str))
    print "(a)", str
    print "(a)", repeat("=", len(str)), new_line("(a)")
  end associate
end subroutine single_var_rd

subroutine multiple_vars_wr()
  type(nc_var) :: geopt, temp, slp
  real, target :: geopt_raw(nx, ny, nz)
  double precision, target :: temp_raw(nx, ny, nt), slp_raw(nx, ny)
  class(*), pointer :: ptr(:)
  real :: nan

  nan = ieee_value(0.0, ieee_quiet_nan)
  call execute_command_line("mkdir -p "//path)

  call random_number(geopt_raw)
  ptr(1:size(geopt_raw)) => geopt_raw
  geopt = data_array(ptr, "geopt", &
    & dims=dims(["latitude".dim.nx, "longitude".dim.ny, "level".dim.nz]), &
    & atts=atts(["long_name".att."geopotenail", "_FillValue".att.nan]))
  nullify (ptr)

  call random_number(temp_raw)
  ptr(1:size(temp_raw)) => temp_raw
  temp = data_array(ptr, "temp", &
    & dims=dims(["latitude".dim.nx, "longitude".dim.ny, "time".dim.nt]), &
    & atts=atts(["long_name".att."temperature"]))
  nullify (ptr)

  call random_number(slp_raw)
  ptr(1:size(slp_raw)) => slp_raw
  slp = data_array(ptr, "slp", &
    & dims=dims(["latitude".dim.nx, "longitude".dim.ny]), &
    & atts=atts(["long_name".att."sea level pressure"]))
  nullify (ptr)

  print "(dt)", geopt, temp, slp
  call to_netcdf([geopt, temp, slp], path//"multiple_vars.nc")
end subroutine multiple_vars_wr

end module module_test
