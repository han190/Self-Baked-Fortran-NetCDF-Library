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
  public :: close_dataset
  public :: shape

  public :: write(formatted)
  private

  !> The data model follows the netCDF data model introduced
  !> https://docs.unidata.ucar.edu/netcdf-c/current/netcdf_data_model.html
  !> All the derived types are merely a thin layer and no data container
  !> is included. Thus, the 'nc_get_var' or 'nc_get_att' function 
  !> will only be called when procedure 'extract' is executed.

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
    !> Data container
    class(*), allocatable :: values(:)
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
    !> Group ID pointer
    integer(c_int), pointer :: nc_id => null()
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

  !> Size of variable
  interface size
    module procedure :: size_variable
  end interface size

  !> Extract data
  interface extract
    module procedure :: extract_variable_int16
    module procedure :: extract_variable_int32
    module procedure :: extract_variable_int64
    module procedure :: extract_variable_real32
    module procedure :: extract_variable_real64
  end interface extract

  !> I/O
  interface write(formatted)
    module procedure :: write_formatted_variable
    module procedure :: write_formatted_group
    module procedure :: write_formatted_attribute
    module procedure :: write_formatted_dimension
  end interface write(formatted)

  !> Inquire dimensions (internal)
  interface inquire_dimensions
    module procedure :: inquire_group_dimensions
    module procedure :: inquire_variable_dimensions
  end interface inquire_dimensions

  !> Inquire attributes (internal)
  interface inquire_attributes
    module procedure :: inquire_variable_attributes
    module procedure :: inquire_group_attributes
  end interface inquire_attributes

  !> Inquire variables (internal)
  interface inquire_variables
    module procedure :: inquire_group_variables
  end interface inquire_variables

  !> Interface to submodules
  interface

    !> New dimensions
    module subroutine inquire_group_dimensions(group)
      type(group_type), intent(inout) :: group
    end subroutine inquire_group_dimensions

    !> Inquire variable dimensions
    module subroutine inquire_variable_dimensions(group, variable)
      type(group_type), intent(inout), target :: group
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

    !> Size of a variable
    module function size_variable(variable) result(ret)
      type(variable_type), intent(in) :: variable
      integer(int64) :: ret
    end function size_variable

    !> Inquire variable attributes
    module subroutine inquire_variable_attributes(group, variable)
      type(group_type), intent(inout) :: group
      type(variable_type), intent(inout) :: variable
    end subroutine inquire_variable_attributes

    !> Inquire group attributes
    module subroutine inquire_group_attributes(group)
      type(group_type), intent(inout) :: group
    end subroutine inquire_group_attributes

    !> Group constructor
    module function open_dataset(path, mode, inq_att, inq_var) result(group)
      character(*), intent(in) :: path, mode
      logical, intent(in), optional :: inq_att, inq_var
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
    module function strip(cstring) result(string)
      character(len=*), intent(in) :: cstring
      character(:), allocatable :: string
    end function strip

    !> Inquire variables from a group
    module subroutine inquire_group_variables(group)
      type(group_type), intent(inout) :: group
    end subroutine inquire_group_variables

    !> Get variable
    module function get_var(group, name) result(variable)
      type(group_type), intent(inout) :: group
      character(*), intent(in) :: name
      type(variable_type) :: variable
    end function get_var

    module subroutine extract_variable_int16(variable, values)
      type(variable_type), intent(in) :: variable
      integer(int16), allocatable, intent(out) :: values(:)
    end subroutine extract_variable_int16

    module subroutine extract_variable_int32(variable, values)
      type(variable_type), intent(in) :: variable
      integer(int32), allocatable, intent(out) :: values(:)
    end subroutine extract_variable_int32

    module subroutine extract_variable_int64(variable, values)
      type(variable_type), intent(in) :: variable
      integer(int64), allocatable, intent(out) :: values(:)
    end subroutine extract_variable_int64

    module subroutine extract_variable_real32(variable, values)
      type(variable_type), intent(in) :: variable
      real(real32), allocatable, intent(out) :: values(:)
    end subroutine extract_variable_real32

    module subroutine extract_variable_real64(variable, values)
      type(variable_type), intent(in) :: variable
      real(real64), allocatable, intent(out) :: values(:)
    end subroutine extract_variable_real64

    module subroutine write_formatted_attribute( &
      & attribute, unit, iotype, v_list, iostat, iomsg)
      class(attribute_type), intent(in) :: attribute
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list (:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine write_formatted_attribute

    module subroutine write_formatted_dimension( &
      & dimension_, unit, iotype, v_list, iostat, iomsg)
      class(dimension_type), intent(in) :: dimension_
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list (:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine write_formatted_dimension

    module subroutine write_formatted_variable( &
      & variable, unit, iotype, v_list, iostat, iomsg)
      class(variable_type), intent(in) :: variable
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list (:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine write_formatted_variable

    module subroutine write_formatted_group( &
      & group, unit, iotype, v_list, iostat, iomsg)
      class(group_type), intent(in) :: group
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list (:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine write_formatted_group

  end interface

end module module_netcdf
