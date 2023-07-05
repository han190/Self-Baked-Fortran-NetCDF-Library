module module_netcdf

  use iso_fortran_env, only: int8, int16, int32, int64, real32, real64
  use iso_c_binding, only: c_ptr, c_null_char, c_f_pointer
  use module_constant
  use module_interface
  implicit none

  public :: file_type
  public :: group_type
  public :: attribute_type
  public :: dimension_type
  public :: variable_type

  public :: dataset
  public :: get_var
  public :: extract
  public :: close
  public :: shape
  private

  !> The data model follows the netCDF data model introduced
  !> https://docs.unidata.ucar.edu/netcdf-c/current/netcdf_data_model.html

  !> File
  type, abstract :: file_type
    !> Filename
    character(:), allocatable :: filename
  end type file_type

  !> Root group type
  type, extends(file_type) :: group_type
    !> Group name
    character(:), allocatable :: name
    !> Subgroups
    type(group_type), allocatable :: groups(:)
    !> Group dimension
    type(dimension_type), allocatable :: dimensions(:)
    !> Group attribute
    type(attribute_type), allocatable :: attributes(:)
    !> Group variables
    type(variable_type), allocatable :: variables(:)
    !> Group ID
    integer(c_int) :: id = 0
    !> Group mode (read, write, etc.)
    integer(c_int) :: mode = 0
    !> Group format (NetCDF3, NetCDF4, etc.)
    integer(c_int) :: format = 0
  end type group_type

  !> Attribute type
  type :: attribute_type
    !> Attribute name
    character(:), allocatable :: name
    !> Attribute length
    integer(c_size_t) :: length = 0
    !> Attribute type
    integer(c_int) :: type = 0
  end type attribute_type

  !> Dimension type
  type :: dimension_type
    !> Dimension name
    character(:), allocatable :: name
    !> Dimension size
    integer(c_size_t) :: length = 0
    !> Is unlimited dimension
    logical :: is_unlimited = .false.
    !> Dimension ID
    integer(c_int) :: id = 0
  end type dimension_type

  !> Variable type
  type :: variable_type
    !> Variable name
    character(:), allocatable :: name
    !> Variable dimension
    type(dimension_type), pointer :: dimensions(:) => null()
    !> Variable attribute
    type(attribute_type), allocatable :: attributes(:)
    !> Variable type
    integer(c_int) :: type = 0
    !> Variable ID
    integer(c_int) :: id = 0
    !> Logical
    logical :: scale_offset = .false.
  end type variable_type

  !> Group constructor
  interface dataset
    module procedure :: open_dataset
  end interface dataset

  !> Shape of dimension/variable
  interface shape
    module procedure :: shape_dimensions
    module procedure :: shape_variable
  end interface shape

  !> Close dataset
  interface close
    module procedure :: close_dataset
  end interface close

  !> Inquire dimensions (internal)
  interface inquire_dimensions
    module procedure :: inquire_group_dimensions
    module procedure :: inquire_variable_dimensions
  end interface inquire_dimensions

  !> Inquire attributes (internal)
  interface inquire_attributes
    module procedure :: inquire_variable_attributes
  end interface inquire_attributes

  !> Interface to submodules
  interface

    !> New dimensions
    module subroutine inquire_group_dimensions(group)
      type(group_type), intent(inout) :: group
    end subroutine inquire_group_dimensions

    !> Inquire variable dimensions
    module subroutine inquire_variable_dimensions(group, variable)
      type(group_type), intent(inout) :: group
      type(variable_type), intent(inout) :: variable
    end subroutine inquire_variable_dimensions

    !> Shape of dimensions
    module function shape_dimensions(dimensions) result(shapes)
      type(dimension_type), intent(in) :: dimensions(:)
      integer(int64), allocatable :: shapes(:)
    end function shape_dimensions

    !> Shape of variable
    module function shape_variable(variable) result(shapes)
      type(variable_type), intent(in) :: variable
      integer(int64), allocatable :: shapes(:)
    end function shape_variable

    !> Inquire variable attributes
    module subroutine inquire_variable_attributes(group, variable)
      type(group_type), intent(inout) :: group
      type(variable_type), intent(inout) :: variable
    end subroutine inquire_variable_attributes

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
      type(group_type), intent(inout) :: group
      character(*), intent(in) :: name
      type(variable_type) :: variable
    end function get_var

  end interface

  integer, parameter :: num_chars = 500

contains

  !> Retrieve data
  !> This routine is designed specifically for 
  !> extracting 3-dimensional ERA5 data
  !> and therefore not included in the submodules.
  function extract(nc, name) result(ret)
    type(group_type), intent(inout) :: nc
    character(*), intent(in) :: name
    real, allocatable :: ret(:, :, :)
    integer :: status, i
    integer(int16), allocatable :: tmp(:, :, :)
    type(variable_type) :: var
    real(real64) :: add_offset(1), scale_factor(1)

    var = get_var(nc, name)
    if (var%scale_offset) then
      status = nc_get_att_double(nc%id, var%id, &
        & "add_offset"//c_null_char, add_offset)
      status = nc_get_att_double(nc%id, var%id, &
        & "scale_factor"//c_null_char, scale_factor)
    else
      add_offset = 0.
      scale_factor = 1.
    end if

    associate (s => shape(var))
      allocate (tmp(s(1), s(2), s(3)))
    end associate
    status = nc_get_var_short(nc%id, var%id, tmp)
    ret = scale_factor(1)*tmp + add_offset(1)
  end function extract

end module module_netcdf
