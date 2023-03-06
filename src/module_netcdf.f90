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
public :: container_type
public :: container_1d
public :: container_3d

public :: dataset
public :: get_var
public :: close
public :: shape
public :: operator(.att.)
public :: write (formatted)
private

!> The data model follows the netCDF data model introduced
!> https://docs.unidata.ucar.edu/netcdf-c/current/netcdf_data_model.html

!> Data container
type, abstract :: container_type
end type container_type

type, extends(container_type) :: container_1d
  class(*), allocatable :: data(:)
end type container_1d

type, extends(container_type) :: container_2d
  class(*), allocatable :: data(:, :)
end type container_2d

type, extends(container_type) :: container_3d
  class(*), allocatable :: data(:, :, :)
end type container_3d

type, extends(container_type) :: container_4d
  class(*), allocatable :: data(:, :, :, :)
end type container_4d

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
  class(*), allocatable :: values(:)
  integer(c_int) :: type = 0
end type attribute_type

!> Dimension type
type :: dimension_type
  character(:), allocatable :: name
  integer(int64) :: length = 0
  logical :: is_unlimited = .false.
  integer(c_int), private :: id = 0
end type dimension_type

!> Variable type
type :: variable_type
  character(:), allocatable :: name
  type(dimension_type), allocatable :: dimensions(:)
  type(attribute_type), allocatable :: attributes(:)
  class(container_type), allocatable :: container
  integer(c_int), private :: type = 0
  integer(c_int), private :: id = 0
end type variable_type

!> Group constructor
interface dataset
  procedure :: open_dataset
end interface dataset

interface shape
  procedure :: shape_dimensions
end interface shape

interface close
  procedure :: close_dataset
end interface close

!> Interface to submodules
interface

  !> New dimensions
  module function inquire_dimensions(nc_id) result(dimensions)
    integer(c_int), intent(in) :: nc_id
    type(dimension_type), allocatable :: dimensions(:)
  end function inquire_dimensions

  !> Inquire variable dimensions
  module function inquire_variable_dimensions(nc_id, var_id) result(dimensions)
    integer(c_int), intent(in) :: nc_id, var_id
    type(dimension_type), allocatable :: dimensions(:)
  end function inquire_variable_dimensions

  !> Shape of dimensions
  module function shape_dimensions(dimensions) result(shapes)
    type(dimension_type), intent(in) :: dimensions(:)
    integer(int64), allocatable :: shapes(:)
  end function shape_dimensions

  !> Inquire variable attributes
  module function inquire_variable_attributes(nc_id, var_id) result(attributes)
    integer(c_int), intent(in) :: nc_id, var_id
    type(attribute_type), allocatable :: attributes(:)
  end function inquire_variable_attributes

  !> Group constructor
  module function open_dataset(path, mode) result(group)
    character(*), intent(in) :: path, mode
    type(group_type) :: group
  end function open_dataset

  !> Group destructor
  module subroutine close_dataset(group)
    type(group_type) :: group
  end subroutine close_dataset

  !> Check function with error messages.
  module subroutine handle_error(status, error_message)
    integer, intent(in) :: status
    character(*), intent(in), optional :: error_message
  end subroutine handle_error

  !> Strip c string
  module function strip(cstring, nlen) result(string)
    character(len=*), intent(in) :: cstring
    integer, intent(in) :: nlen
    character(:), allocatable :: string
  end function strip

  !> Get variable
  module function get_var(group, name) result(variable)
    type(group_type), intent(in) :: group
    character(*), intent(in) :: name
    type(variable_type) :: variable
  end function get_var

end interface

integer, parameter :: num_chars = 500

end module module_netcdf
