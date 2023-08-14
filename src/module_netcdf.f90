module module_netcdf

  use, intrinsic :: iso_fortran_env, only: &
    & int8, int16, int32, int64, real32, real64
  use, intrinsic :: iso_c_binding, only: &
    & c_ptr, c_f_pointer, c_null_char
  use :: module_constant
  use :: module_interface
  use :: module_types
  use :: module_data_structure
  implicit none

  public :: dimension_type, attribute_type
  public :: group_type, variable_type
  public :: inq_var, dataset, extract
  public :: write(formatted), shape, size, rank
  private

  interface shape
    module procedure :: shape_dims
  end interface shape

  interface size
    module procedure :: size_dims
    module procedure :: size_var
  end interface size

  interface rank
    module procedure :: rank_dims
    module procedure :: rank_var
  end interface rank

  interface extract
    module procedure :: extract_var_int16
    module procedure :: extract_var_int32
    module procedure :: extract_var_int64
    module procedure :: extract_var_real32
    module procedure :: extract_var_real64
  end interface extract

  interface write(formatted)
    module procedure :: write_formatted_var
    module procedure :: write_formatted_grp
  end interface write(formatted)

  !> Interface to submodules
  interface

    !> submodule utility
    !> -----------------

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
    module function shape_dims(dims) result(ret)
      type(dimensions_type), intent(in) :: dims
      integer(int64), allocatable :: ret(:)
    end function shape_dims

    !> size of dimensions
    module function size_dims(dims) result(ret)
      type(dimensions_type), intent(in) :: dims
      integer(int64) :: ret
    end function size_dims

    !> rank of dimensions
    module function rank_dims(dims) result(ret)
      type(dimensions_type), intent(in) :: dims
      integer(int64) :: ret
    end function rank_dims

    !> submodule attribute
    !> -------------------

    !> Inquire group attribute
    module subroutine inq_grp_atts(grp)
      type(group_type), intent(inout) :: grp
    end subroutine inq_grp_atts

    !> Inquire variable attributes
    module subroutine inq_var_atts(grp, var)
      type(group_type), intent(inout) :: grp
      type(variable_type), intent(inout) :: var
    end subroutine inq_var_atts

    !> submodule variable
    !> ------------------

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

    !> Extract variable int16
    module subroutine extract_var_int16(var, vals)
      type(variable_type), intent(in) :: var
      integer(int16), intent(out) :: vals(*)
    end subroutine extract_var_int16

    !> Extract variable int32
    module subroutine extract_var_int32(var, vals)
      type(variable_type), intent(in) :: var
      integer(int32), intent(out) :: vals(*)
    end subroutine extract_var_int32

    !> Extract variable int64
    module subroutine extract_var_int64(var, vals)
      type(variable_type), intent(in) :: var
      integer(int64), intent(out) :: vals(*)
    end subroutine extract_var_int64

    !> Extract variable real32
    module subroutine extract_var_real32(var, vals)
      type(variable_type), intent(in) :: var
      real(real32), intent(out) :: vals(*)
    end subroutine extract_var_real32

    !> Extract variable real64
    module subroutine extract_var_real64(var, vals)
      type(variable_type), intent(in) :: var
      real(real64), intent(out) :: vals(*)
    end subroutine extract_var_real64

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
      integer, intent(in) :: v_list (:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine write_formatted_var

    !> write(formatted) of group
    module subroutine write_formatted_grp( &
      & grp, unit, iotype, v_list, iostat, iomsg)
      class(group_type), intent(in) :: grp
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list (:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg
    end subroutine write_formatted_grp

  end interface

end module module_netcdf