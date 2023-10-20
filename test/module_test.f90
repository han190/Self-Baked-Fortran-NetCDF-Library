module module_test

use :: iso_fortran_env
use :: module_netcdf
use :: module_data_structure
use :: ieee_arithmetic
implicit none

integer, parameter :: nx = 3, ny = 5, nz = 4, nt = 1
character(len=*), parameter :: path = "./data/"
character(len=*), parameter :: signle_var_file = "single_var.nc"
character(len=*), parameter :: multi_vars_file = "multi_vars.nc"

contains

subroutine test_data_structure()
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
end subroutine test_data_structure

subroutine single_var_wr()
  type(netcdf_variable) :: var
  real :: raw(nx, ny)
  integer :: i

  ! Fill raw with random numbers.
  call execute_command_line("mkdir -p "//path)
  call random_number(raw)

  ! Construct a raw array and write to a netcdf file.
  var = data_array(raw, name="var", &
    & dims=["x".dim.nx, "y".dim.ny], &
    & atts=["long_name".att."dummy variable"])

  ! Write to netcdf.
  call to_netcdf(var, path//signle_var_file)

  print "(dt)", var
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
  type(netcdf_variable), target :: var
  real, pointer :: raw(:, :) => null()
  integer :: i

  var = from_netcdf(path//signle_var_file, "var")
  call extract(var, raw)

  print "(dt)", var
  print "(a)", "values = "
  do i = 1, size(raw, 2)
    print "(3(f10.4, 1x))", raw(:, i)
  end do
  nullify (raw)

  associate (str => "Successfully read from 'single_var.nc'.")
    print "(a)", repeat("=", len(str))
    print "(a)", str
    print "(a)", repeat("=", len(str)), new_line("(a)")
  end associate
end subroutine single_var_rd

subroutine multiple_vars_wr()
  type(netcdf_variable) :: geopt, temp, slp
  real :: geopt_raw(nx, ny, nz)
  integer :: temp_raw(nx, ny, nt)
  double precision :: slp_raw(nx, ny)
  real :: nan

  nan = ieee_value(0.0, ieee_quiet_nan)
  call execute_command_line("mkdir -p "//path)

  call random_number(geopt_raw)
  geopt = data_array(geopt_raw, "geopt", &
    & dims=["latitude".dim.nx, "longitude".dim.ny, "level".dim.nz], &
    & atts=["long_name".att."geopotenail", "_FillValue".att.nan])

  temp_raw = 1
  temp = data_array(temp_raw, "temp", &
    & dims=["latitude".dim.nx, "longitude".dim.ny, "time".dim.nt], &
    & atts=["long_name".att."temperature", "add_offset".att.-273.15, "scale_factor".att.1.0])

  call random_number(slp_raw)
  slp = data_array(slp_raw, "slp", &
    & dims=["latitude".dim.nx, "longitude".dim.ny], &
    & atts=["long_name".att."sea level pressure", "missing_value".att.-2147483647])

  print "(*(dt))", geopt, temp, slp
  call to_netcdf([geopt, temp, slp], path//multi_vars_file)

  associate (str => "Successfully write to 'multiple_vars.nc'.")
    print "(a)", repeat("=", len(str))
    print "(a)", str
    print "(a)", repeat("=", len(str)), new_line("(a)")
  end associate
end subroutine multiple_vars_wr

subroutine multiple_vars_rd()
  real, pointer :: add_offset(:), scale_factor(:)
  integer, pointer :: raw(:, :, :)
  type(netcdf_variable) :: var

  var = from_netcdf(path//multi_vars_file, "temp")
  call extract(var, raw)
  call extract(var, "add_offset", add_offset)
  call extract(var, "scale_factor", scale_factor)

  print "(dt)", var
  associate (str => "Successfully read to 'multiple_vars.nc'.")
    print "(a)", repeat("=", len(str))
    print "(a)", str
    print "(a)", repeat("=", len(str)), new_line("(a)")
  end associate
end subroutine multiple_vars_rd

end module module_test
