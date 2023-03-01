module module_netcdf

use iso_fortran_env, only: int8, int16, int32, int64, real32, real64
use iso_c_binding, only: c_ptr, c_null_char, c_f_pointer
use module_constant
use module_cwrapper
implicit none

public :: file_type
public :: group_type
public :: attribute_type
public :: dimension_type
public :: variable_type

public :: dataset
public :: close_dataset
public :: get_var
public :: operator(.att.)
public :: write(formatted)
private

!> The data model follows the netCDF data model introduced
!> https://docs.unidata.ucar.edu/netcdf-c/current/netcdf_data_model.html

!> File
type, abstract :: file_type
  character(:), allocatable :: filename
end type file_type

!> Root group type
type, extends(file_type) :: group_type
  character(:), allocatable :: name
  type(group_type), allocatable :: groups(:)
  type(dimension_type), allocatable :: dimensions(:)
  type(attribute_type), allocatable :: attributes(:)
  type(variable_type), allocatable :: variables(:)
  integer(c_int), private :: id = 0
  integer(c_int), private :: mode = 0
end type group_type

!> Attribute type
type :: attribute_type
  character(:), allocatable :: name
  integer(int64) :: length = 0
  integer(c_int), private :: type = 0
end type attribute_type

!> Dimension type
type :: dimension_type
  character(:), allocatable :: name
  integer(int64) :: length = 0
  integer(c_int), private :: id = 0
  integer(c_int), private :: unlimited_dim = 0
end type dimension_type

!> Variable type
type :: variable_type
  character(:), allocatable :: name
  type(dimension_type), pointer :: dimensions(:) => null()
  type(attribute_type), allocatable :: attributes(:)
  integer(c_int), private :: type = 0
  integer(c_int), private :: id = 0
end type variable_type

!> Group constructor
interface dataset
  procedure :: new_group
end interface dataset

!> Interface to submodules
interface

  !> Group constructor
  module function new_group(path, mode) result(group)
    character(*), intent(in) :: path, mode
    type(group_type) :: group
  end function new_group

  !> Group destructor
  module subroutine close_dataset(group)
    type(group_type) :: group
  end subroutine close_dataset

  !> Check function with error messages.
  module subroutine check(status, error_message)
    integer, intent(in) :: status
    character(*), intent(in), optional :: error_message
  end subroutine check

  !> Strip c string
  module function strip(cstring, nlen) result(string)
    character(len=*), intent(in) :: cstring
    integer, intent(in) :: nlen
    character(:), allocatable :: string
  end function strip

  !> Get variable
  module function get_var_(group, name) result(variable)
    type(group_type), intent(in) :: group
    character(*), intent(in) :: name
    type(variable_type) :: variable
  end function get_var_

end interface

contains

!> Retrieve data
!> This routine is designed specifically for ERA5 data
!> and therefore not included in the submodules.
function get_var(nc, name) result(ret)
  type(group_type), intent(in) :: nc
  character(*), intent(in) :: name
  real, allocatable :: ret(:, :, :)
  type(variable_type) :: var
  integer(int16), allocatable :: temp(:, :, :)
  real(real64) :: add_offset(1), scale_factor(1)
  integer :: status, i

  var = get_var_(nc, name)

  status = nc_get_att_double(nc%id, var%id, &
    & "add_offset"//c_null_char, add_offset)
  status = nc_get_att_double(nc%id, var%id, &
    & "scale_factor"//c_null_char, scale_factor)
  
  associate (dims => var%dimensions)
    allocate (temp(dims(3)%length, &
      & dims(2)%length, dims(1)%length))
  end associate
  status = nc_get_var_short(nc%id, var%id, temp)
  ret = scale_factor(1)*temp + add_offset(1)
end function get_var

end module module_netcdf
