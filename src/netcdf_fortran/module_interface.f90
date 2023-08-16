module module_interface

  use, intrinsic :: iso_fortran_env, only: &
    & int8, int16, int32, int64, real32, real64
  use, intrinsic :: iso_c_binding, only: &
    & c_ptr, c_f_pointer, c_null_char
  use :: module_constant
  use :: module_c_interface
  use :: module_data_structure
  implicit none

  public :: dimension_type, attribute_type
  public :: group_type, variable_type, handle_error
  public :: inq_var, dataset, get_var, get_att
  public :: def_dim, def_var, put_var, put_att
  public :: write (formatted), shape, size, rank
  private

  !> NetCDF Data Model
  !> -----------------

  !> The data model follows the netCDF data model introduced
  !> https://docs.unidata.ucar.edu/netcdf-c/current/netcdf_data_model.html

  !> Abstract NetCDF type
  type, abstract :: netcdf_type
    !> grp, var, dim, att ID
    integer(c_int) :: id = 0
    !> grp, var, dim, att name
    character(len=:), allocatable :: name
  end type netcdf_type

  !> Dimension type
  type, extends(netcdf_type) :: dimension_type
    !> length
    integer(c_size_t) :: length = 0
    !> is unlimited
    logical :: is_unlimited = .false.
  end type dimension_type

  !> Attribute type
  type, extends(netcdf_type) :: attribute_type
    !> length
    integer(c_size_t) :: length = 0
    !> type (default: not a type)
    integer(c_int) :: type = 0
    !> container
    class(*), allocatable :: values(:)
  end type attribute_type

  !> Variable type
  type, extends(netcdf_type) :: variable_type
    !> dimensions
    type(dictionary_type) :: dims
    !> attribute
    type(dictionary_type) :: atts
    !> group id
    integer(c_int), pointer :: grp_id => null()
    !> type
    integer(c_int) :: type = 0
  end type variable_type

  !> Group type
  type, extends(netcdf_type) :: group_type
    !> subgroups
    type(group_type), allocatable :: grps(:)
    !> dimensions
    type(dictionary_type) :: dims
    !> attribute
    type(dictionary_type) :: atts
    !> variables
    type(variable_type), allocatable :: vars(:)
    !> mode (read, write, etc.)
    integer(c_int) :: mode = 0
    !> format (NetCDF3, NetCDF4, etc.)
    integer(c_int) :: format = 0
    !> filename
    character(len=:), allocatable :: filename
  end type group_type

  interface shape
    module procedure :: shape_dict
    module procedure :: shape_var
  end interface shape

  interface size
    ! module procedure :: size_dims ! recursive defined
    module procedure :: size_var
  end interface size

  interface rank
    ! module procedure :: rank_dims
    module procedure :: rank_var
  end interface rank

  interface def_dim
    module procedure :: def_grp_dims
  end interface def_dim

  interface def_var
    module procedure :: def_grp_var
  end interface def_var

  interface put_att
    module procedure :: put_grp_att
    module procedure :: put_var_att
  end interface put_att

  interface get_att
    module procedure :: get_att_name_scalar_int16
    module procedure :: get_att_name_scalar_int32
    module procedure :: get_att_name_scalar_int64
    module procedure :: get_att_name_scalar_real32
    module procedure :: get_att_name_scalar_real64
  end interface get_att

  interface get_var
    module procedure :: get_var_int16
    module procedure :: get_var_int32
    module procedure :: get_var_int64
    module procedure :: get_var_real32
    module procedure :: get_var_real64
    module procedure :: get_var_name_int16
    module procedure :: get_var_name_int32
    module procedure :: get_var_name_int64
    module procedure :: get_var_name_real32
    module procedure :: get_var_name_real64
  end interface get_var

  interface write (formatted)
    module procedure :: write_formatted_var
    module procedure :: write_formatted_grp
  end interface write (formatted)

  !> Interface to submodules
  interface

    !> submodule utility
    !> -----------------

    !> Convert string to cstring
    module pure function to_cstr(string) result(cstring)
      character(len=*), intent(in) :: string
      character(kind=c_char, len=:), allocatable :: cstring
    end function to_cstr

    !> cstring to fstring
    module function strip(cstring) result(string)
      character(len=*), intent(in) :: cstring
      character(:), allocatable :: string
    end function strip

    !> error handler
    module subroutine handle_error(stat, err_msg)
      integer(c_int), intent(in) :: stat
      character(*), intent(in), optional :: err_msg
    end subroutine handle_error

    !> submodule dimension
    !> -------------------

    !> Define group dimensions
    module subroutine def_grp_dims(grp, names, ndims)
      type(group_type), intent(inout) :: grp
      character(len=*), intent(in) :: names(:)
      integer, intent(in) :: ndims(:)
      type(dimension_type), allocatable :: dims(:)
    end subroutine def_grp_dims

    !> Inquire group dimension
    module subroutine inq_grp_dims(grp)
      type(group_type), intent(inout) :: grp
    end subroutine inq_grp_dims

    !> Inquire variable dimensions
    module subroutine inq_var_dims(grp, var)
      type(group_type), target, intent(inout) :: grp
      type(variable_type), intent(inout) :: var
    end subroutine inq_var_dims

    !> shape of dimensions
    module function shape_dict(dict) result(ret)
      type(dictionary_type), intent(in) :: dict
      integer(int64), allocatable :: ret(:)
    end function shape_dict

    ! !> size of dimensions
    ! module function size_dims(dims) result(ret)
    !   type(dictionary_type), intent(in) :: dims
    !   integer(int64) :: ret
    ! end function size_dims

    ! !> rank of dimensions
    ! module function rank_dims(dims) result(ret)
    !   type(dictionary_type), intent(in) :: dims
    !   integer(int64) :: ret
    ! end function rank_dims

    !> submodule attribute
    !> -------------------

    !> Put group attribute
    module subroutine put_grp_att(grp, name, val)
      type(group_type), intent(inout) :: grp
      character(len=*), intent(in) :: name
      class(*), intent(in) :: val
    end subroutine put_grp_att

    !> Put variable attribute
    module subroutine put_var_att(var, name, val)
      type(variable_type), intent(inout) :: var
      character(len=*), intent(in) :: name
      class(*), intent(in) :: val
    end subroutine put_var_att

    !> Inquire group attribute
    module subroutine inq_grp_atts(grp)
      type(group_type), intent(inout) :: grp
    end subroutine inq_grp_atts

    !> Inquire variable attributes
    module subroutine inq_var_atts(grp, var)
      type(group_type), intent(inout) :: grp
      type(variable_type), intent(inout) :: var
    end subroutine inq_var_atts

    module subroutine get_att_name_scalar_int16(var, name, val)
      type(variable_type), intent(in) :: var
      character(len=*), intent(in) :: name
      integer(int16), intent(out) :: val
    end subroutine get_att_name_scalar_int16

    module subroutine get_att_name_scalar_int32(var, name, val)
      type(variable_type), intent(in) :: var
      character(len=*), intent(in) :: name
      integer(int32), intent(out) :: val
    end subroutine get_att_name_scalar_int32

    module subroutine get_att_name_scalar_int64(var, name, val)
      type(variable_type), intent(in) :: var
      character(len=*), intent(in) :: name
      integer(int64), intent(out) :: val
    end subroutine get_att_name_scalar_int64

    module subroutine get_att_name_scalar_real32(var, name, val)
      type(variable_type), intent(in) :: var
      character(len=*), intent(in) :: name
      real(real32), intent(out) :: val
    end subroutine get_att_name_scalar_real32

    module subroutine get_att_name_scalar_real64(var, name, val)
      type(variable_type), intent(in) :: var
      character(len=*), intent(in) :: name
      real(real64), intent(out) :: val
    end subroutine get_att_name_scalar_real64

    !> submodule variable
    !> ------------------

    !> Define variable
    module function def_grp_var(grp, name, type, dim_names) result(var)
      type(group_type), target, intent(in) :: grp
      character(len=*), intent(in) :: name, dim_names(:)
      integer(nc_type), intent(in) :: type
      type(variable_type) :: var
    end function def_grp_var

    !> Put variable
    module subroutine put_var(var, vals)
      type(variable_type), intent(in) :: var
      class(*), intent(in) :: vals(:)
    end subroutine put_var

    ! module subroutine put_var_int(var, vals)
    !   type(variable_type), intent(in) :: var
    !   integer, intent(in) :: vals(*)
    ! end subroutine put_var_int

    !> Inquire group variables
    module subroutine inq_grp_vars(grp)
      type(group_type), target, intent(inout) :: grp
    end subroutine inq_grp_vars

    !> Inquire variable
    module function inq_var(grp, name) result(var)
      type(group_type), target, intent(inout) :: grp
      character(len=*), intent(in) :: name
      type(variable_type) :: var
    end function inq_var

    !> Shape of a variable
    module function shape_var(var) result(ret)
      type(variable_type), intent(in) :: var
      integer(int64), allocatable :: ret(:)
    end function shape_var

    !> Size of a variable
    module function size_var(var) result(ret)
      type(variable_type), intent(in) :: var
      integer(int64) :: ret
    end function size_var

    !> Rank of a variable
    module function rank_var(var) result(ret)
      type(variable_type), intent(in) :: var
      integer(int64) :: ret
    end function rank_var

    module subroutine get_var_name_int16(grp, name, vals)
      type(group_type), intent(inout) :: grp
      character(len=*), intent(in) :: name
      integer(int16), allocatable, intent(out) :: vals(:)
    end subroutine get_var_name_int16

    module subroutine get_var_int16(var, vals)
      type(variable_type), intent(in) :: var
      integer(int16), allocatable, intent(out) :: vals(:)
    end subroutine get_var_int16

    module subroutine get_var_name_int32(grp, name, vals)
      type(group_type), intent(inout) :: grp
      character(len=*), intent(in) :: name
      integer(int32), allocatable, intent(out) :: vals(:)
    end subroutine get_var_name_int32

    module subroutine get_var_int32(var, vals)
      type(variable_type), intent(in) :: var
      integer(int32), allocatable, intent(out) :: vals(:)
    end subroutine get_var_int32

    module subroutine get_var_name_int64(grp, name, vals)
      type(group_type), intent(inout) :: grp
      character(len=*), intent(in) :: name
      integer(int64), allocatable, intent(out) :: vals(:)
    end subroutine get_var_name_int64

    module subroutine get_var_int64(var, vals)
      type(variable_type), intent(in) :: var
      integer(int64), allocatable, intent(out) :: vals(:)
    end subroutine get_var_int64

    module subroutine get_var_name_real32(grp, name, vals)
      type(group_type), intent(inout) :: grp
      character(len=*), intent(in) :: name
      real(real32), allocatable, intent(out) :: vals(:)
    end subroutine get_var_name_real32

    module subroutine get_var_real32(var, vals)
      type(variable_type), intent(in) :: var
      real(real32), allocatable, intent(out) :: vals(:)
    end subroutine get_var_real32

    module subroutine get_var_name_real64(grp, name, vals)
      type(group_type), intent(inout) :: grp
      character(len=*), intent(in) :: name
      real(real64), allocatable, intent(out) :: vals(:)
    end subroutine get_var_name_real64

    module subroutine get_var_real64(var, vals)
      type(variable_type), intent(in) :: var
      real(real64), allocatable, intent(out) :: vals(:)
    end subroutine get_var_real64

    !> submodule group
    !> ---------------

    !> Open dataset
    module function dataset(path, mode, inq_atts, inq_vars) result(file)
      character(len=*), intent(in) :: path, mode
      logical, intent(in), optional :: inq_atts, inq_vars
      type(group_type) :: file
    end function dataset

    !> submodule io
    !> ------------

    !> write(formatted) of variable
    module subroutine write_formatted_var( &
      & var, unit, iotype, v_list, iostat, iomsg)
      class(variable_type), intent(in) :: var
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine write_formatted_var

    !> write(formatted) of group
    module subroutine write_formatted_grp( &
      & grp, unit, iotype, v_list, iostat, iomsg)
      class(group_type), intent(in) :: grp
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine write_formatted_grp

  end interface

end module module_interface
