module module_fortran_interface

use :: iso_fortran_env
use :: iso_c_binding
use :: module_c_interface
use :: module_data_structure
implicit none

!> The data model follows the netCDF data model introduced
!> https://docs.unidata.ucar.edu/netcdf-c/current/netcdf_data_model.html

public :: netcdf_dimension, netcdf_attribute, netcdf_variable, netcdf_group, netcdf_file
public :: operator(.dim.), operator(.att.)
public :: write(formatted)
public :: shape, atts, dims
public :: to_netcdf, from_netcdf
public :: data_array, data_set, extract
private

type, abstract :: netcdf_abstract
  integer(c_int) :: ID
  character(len=:), allocatable :: name
end type netcdf_abstract

type, extends(netcdf_abstract) :: netcdf_dimension
  integer(c_size_t) :: len = 0
  logical :: is_unlim = .false.
end type netcdf_dimension

type, extends(netcdf_abstract) :: netcdf_attribute
  integer(c_size_t) :: len = 0
  integer(c_int) :: type = 0
  class(*), pointer :: vals(:) => null()
end type netcdf_attribute

type, extends(netcdf_abstract) :: netcdf_variable
  type(netcdf_dimension), allocatable :: dims(:)
  type(netcdf_attribute), allocatable :: atts(:)
  integer(c_int), pointer :: grpID => null()
  integer(c_int) :: type = 0
  class(*), pointer :: vals(:) => null()
end type netcdf_variable

type, extends(netcdf_abstract) :: netcdf_group
  type(netcdf_dimension), allocatable :: dims(:)
  type(netcdf_attribute), allocatable :: atts(:)
  type(netcdf_variable), allocatable :: vars(:)
  type(netcdf_group), allocatable :: grps(:)
  integer(c_int) :: mode = 0
  integer(c_int) :: fmt = 0
end type netcdf_group

type, extends(netcdf_group) :: netcdf_file
  character(len=:), allocatable :: filename
end type netcdf_file

interface operator(.dim.)
  module procedure :: new_dim
end interface operator(.dim.)

interface dims
  module procedure :: new_dims
  module procedure :: new_dims_unlim
end interface dims

interface operator(.att.)
  module procedure :: new_att_int8_0d
  module procedure :: new_att_int16_0d
  module procedure :: new_att_int32_0d
  module procedure :: new_att_int64_0d
  module procedure :: new_att_real32_0d
  module procedure :: new_att_real64_0d
  module procedure :: new_att_int8_1d
  module procedure :: new_att_int16_1d
  module procedure :: new_att_int32_1d
  module procedure :: new_att_int64_1d
  module procedure :: new_att_real32_1d
  module procedure :: new_att_real64_1d
  module procedure :: new_att_char_0d
  module procedure :: new_att_char_1d
end interface operator(.att.)

interface atts
  module procedure :: new_atts
end interface atts

interface data_array
  module procedure :: new_var
end interface data_array

interface data_set
  module procedure :: new_file
end interface data_set

interface to_netcdf
  module procedure :: to_netcdf_var
  module procedure :: to_netcdf_vars
end interface to_netcdf

interface from_netcdf
  module procedure :: from_netcdf_grp
  module procedure :: from_netcdf_var
end interface from_netcdf

interface extract
  module procedure :: extract_var_int8_1d
  module procedure :: extract_var_int16_1d
  module procedure :: extract_var_int32_1d
  module procedure :: extract_var_int64_1d
  module procedure :: extract_var_real32_1d
  module procedure :: extract_var_real64_1d
  module procedure :: extract_var_int8_2d
  module procedure :: extract_var_int16_2d
  module procedure :: extract_var_int32_2d
  module procedure :: extract_var_int64_2d
  module procedure :: extract_var_real32_2d
  module procedure :: extract_var_real64_2d
  module procedure :: extract_var_int8_3d
  module procedure :: extract_var_int16_3d
  module procedure :: extract_var_int32_3d
  module procedure :: extract_var_int64_3d
  module procedure :: extract_var_real32_3d
  module procedure :: extract_var_real64_3d
  module procedure :: extract_var_int8_4d
  module procedure :: extract_var_int16_4d
  module procedure :: extract_var_int32_4d
  module procedure :: extract_var_int64_4d
  module procedure :: extract_var_real32_4d
  module procedure :: extract_var_real64_4d
  module procedure :: extract_var_int8_5d
  module procedure :: extract_var_int16_5d
  module procedure :: extract_var_int32_5d
  module procedure :: extract_var_int64_5d
  module procedure :: extract_var_real32_5d
  module procedure :: extract_var_real64_5d
  module procedure :: extract_att_int8
  module procedure :: extract_att_int16
  module procedure :: extract_att_int32
  module procedure :: extract_att_int64
  module procedure :: extract_att_real32
  module procedure :: extract_att_real64
end interface extract

interface shape
  module procedure :: shape_dims
  module procedure :: shape_var
end interface shape

interface write(formatted)
  module procedure :: write_formatted_att
  module procedure :: write_formatted_dim
  module procedure :: write_formatted_var
end interface write(formatted)

interface
  module pure function cstr(string) result(cstring)
    character(len=*), intent(in) :: string
    character(kind=c_char, len=:), allocatable :: cstring
  end function cstr

  module pure function cstrip(cstring) result(string)
    character(len=*), intent(in) :: cstring
    character(:), allocatable :: string
  end function cstrip

  module subroutine handle_error(stat, err_msg)
    integer(c_int), intent(in) :: stat
    character(*), intent(in), optional :: err_msg
  end subroutine handle_error

  module pure function new_dim(name, len) result(dim)
    character(len=*), intent(in) :: name
    integer, intent(in) :: len
    type(netcdf_dimension) :: dim
  end function new_dim

  module pure function new_dims(dims) result(ret)
    type(netcdf_dimension), intent(in) :: dims(:)
    type(netcdf_dimension), allocatable :: ret(:)
  end function new_dims

  module pure function new_dims_unlim(dims, unlim_dim) result(ret)
    type(netcdf_dimension), intent(in) :: dims(:)
    integer, intent(in) :: unlim_dim
    type(netcdf_dimension), allocatable :: ret(:)
  end function new_dims_unlim

  module pure function shape_dims(dims) result(ret)
    type(netcdf_dimension), intent(in) :: dims(:)
    integer, allocatable :: ret(:)
  end function shape_dims

  module subroutine def_grp_dim(grp)
    class(netcdf_group), intent(in) :: grp
  end subroutine def_grp_dim

  module subroutine def_var_dim(var)
    type(netcdf_variable), intent(in) :: var
  end subroutine def_var_dim

  module subroutine inq_grp_dims(grp)
    class(netcdf_group), intent(inout) :: grp
  end subroutine inq_grp_dims

  module subroutine inq_var_dims(var)
    type(netcdf_variable), intent(inout) :: var
  end subroutine inq_var_dims

  module pure function new_att_vec(name, vals) result(ret)
    character(len=*), intent(in) :: name
    class(*), intent(in) :: vals(:)
    type(netcdf_attribute) :: ret
  end function new_att_vec

  module pure function new_att_int8_0d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    integer(int8), intent(in) :: vals
    type(netcdf_attribute) :: ret
  end function new_att_int8_0d

  module pure function new_att_int16_0d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    integer(int16), intent(in) :: vals
    type(netcdf_attribute) :: ret
  end function new_att_int16_0d

  module pure function new_att_int32_0d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    integer(int32), intent(in) :: vals
    type(netcdf_attribute) :: ret
  end function new_att_int32_0d

  module pure function new_att_int64_0d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    integer(int64), intent(in) :: vals
    type(netcdf_attribute) :: ret
  end function new_att_int64_0d

  module pure function new_att_real32_0d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    real(real32), intent(in) :: vals
    type(netcdf_attribute) :: ret
  end function new_att_real32_0d

  module pure function new_att_real64_0d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    real(real64), intent(in) :: vals
    type(netcdf_attribute) :: ret
  end function new_att_real64_0d

  module pure function new_att_int8_1d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    integer(int8), intent(in) :: vals(:)
    type(netcdf_attribute) :: ret
  end function new_att_int8_1d

  module pure function new_att_int16_1d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    integer(int16), intent(in) :: vals(:)
    type(netcdf_attribute) :: ret
  end function new_att_int16_1d

  module pure function new_att_int32_1d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    integer(int32), intent(in) :: vals(:)
    type(netcdf_attribute) :: ret
  end function new_att_int32_1d

  module pure function new_att_int64_1d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    integer(int64), intent(in) :: vals(:)
    type(netcdf_attribute) :: ret
  end function new_att_int64_1d

  module pure function new_att_real32_1d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    real(real32), intent(in) :: vals(:)
    type(netcdf_attribute) :: ret
  end function new_att_real32_1d

  module pure function new_att_real64_1d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    real(real64), intent(in) :: vals(:)
    type(netcdf_attribute) :: ret
  end function new_att_real64_1d


  module pure function new_att_char_0d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: vals
    type(netcdf_attribute) :: ret
  end function new_att_char_0d

  module pure function new_att_char_1d(name, vals) result(ret)
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: vals(:)
    type(netcdf_attribute) :: ret
  end function new_att_char_1d


  module function new_atts(atts) result(ret)
    type(netcdf_attribute), intent(in) :: atts(:)
    type(netcdf_attribute), allocatable :: ret(:)
  end function new_atts

  module subroutine put_grp_atts(grp)
    class(netcdf_group), intent(in) :: grp
  end subroutine put_grp_atts

  module subroutine put_var_atts(var)
    type(netcdf_variable), intent(in) :: var
  end subroutine put_var_atts

  module subroutine inq_grp_atts(grp)
    class(netcdf_group), intent(inout) :: grp
  end subroutine inq_grp_atts

  module subroutine inq_var_atts(var)
    type(netcdf_variable), intent(inout) :: var
  end subroutine inq_var_atts

  module subroutine get_var_atts(var)
    type(netcdf_variable), intent(inout) :: var
  end subroutine get_var_atts

  module subroutine extract_att_int8(var, name, raw)
    type(netcdf_variable), target, intent(in) :: var
    character(len=*), intent(in) :: name
    integer(int8), pointer, intent(out) :: raw(:)
  end subroutine extract_att_int8

  module subroutine extract_att_int16(var, name, raw)
    type(netcdf_variable), target, intent(in) :: var
    character(len=*), intent(in) :: name
    integer(int16), pointer, intent(out) :: raw(:)
  end subroutine extract_att_int16

  module subroutine extract_att_int32(var, name, raw)
    type(netcdf_variable), target, intent(in) :: var
    character(len=*), intent(in) :: name
    integer(int32), pointer, intent(out) :: raw(:)
  end subroutine extract_att_int32

  module subroutine extract_att_int64(var, name, raw)
    type(netcdf_variable), target, intent(in) :: var
    character(len=*), intent(in) :: name
    integer(int64), pointer, intent(out) :: raw(:)
  end subroutine extract_att_int64

  module subroutine extract_att_real32(var, name, raw)
    type(netcdf_variable), target, intent(in) :: var
    character(len=*), intent(in) :: name
    real(real32), pointer, intent(out) :: raw(:)
  end subroutine extract_att_real32

  module subroutine extract_att_real64(var, name, raw)
    type(netcdf_variable), target, intent(in) :: var
    character(len=*), intent(in) :: name
    real(real64), pointer, intent(out) :: raw(:)
  end subroutine extract_att_real64


  module function new_var(data, name, dims, unlim_dim, atts) result(var)
    class(*), target, intent(in) :: data(..)
    character(len=*), intent(in) :: name
    type(netcdf_dimension), intent(in) :: dims(:)
    integer, intent(in), optional :: unlim_dim
    type(netcdf_attribute), intent(in), optional :: atts(:)
    type(netcdf_variable) :: var
  end function new_var

  module pure function shape_var(var) result(ret)
    type(netcdf_variable), intent(in) :: var
    integer, allocatable :: ret(:)
  end function shape_var

  module subroutine def_var(var)
    type(netcdf_variable), intent(inout) :: var
  end subroutine def_var

  module subroutine put_var(var)
    type(netcdf_variable), intent(in) :: var
  end subroutine put_var

  module subroutine to_netcdf_var(var, filename, mode)
    type(netcdf_variable), intent(in) :: var
    character(len=*), intent(in) :: filename
    integer(c_int), intent(in), optional :: mode
  end subroutine to_netcdf_var

  module subroutine to_netcdf_vars(vars, filename, mode)
    type(netcdf_variable), intent(in) :: vars(:)
    character(len=*), intent(in) :: filename
    integer(c_int), intent(in), optional :: mode
  end subroutine to_netcdf_vars

  module function from_netcdf_var(filename, name) result(var)
    character(len=*), intent(in) :: filename, name
    type(netcdf_variable) :: var
  end function from_netcdf_var

  module subroutine extract_var_int8_1d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int8), pointer, intent(out) :: raw(:)
  end subroutine extract_var_int8_1d

  module subroutine extract_var_int16_1d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int16), pointer, intent(out) :: raw(:)
  end subroutine extract_var_int16_1d

  module subroutine extract_var_int32_1d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int32), pointer, intent(out) :: raw(:)
  end subroutine extract_var_int32_1d

  module subroutine extract_var_int64_1d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int64), pointer, intent(out) :: raw(:)
  end subroutine extract_var_int64_1d

  module subroutine extract_var_real32_1d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    real(real32), pointer, intent(out) :: raw(:)
  end subroutine extract_var_real32_1d

  module subroutine extract_var_real64_1d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    real(real64), pointer, intent(out) :: raw(:)
  end subroutine extract_var_real64_1d

  module subroutine extract_var_int8_2d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int8), pointer, intent(out) :: raw(:,:)
  end subroutine extract_var_int8_2d

  module subroutine extract_var_int16_2d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int16), pointer, intent(out) :: raw(:,:)
  end subroutine extract_var_int16_2d

  module subroutine extract_var_int32_2d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int32), pointer, intent(out) :: raw(:,:)
  end subroutine extract_var_int32_2d

  module subroutine extract_var_int64_2d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int64), pointer, intent(out) :: raw(:,:)
  end subroutine extract_var_int64_2d

  module subroutine extract_var_real32_2d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    real(real32), pointer, intent(out) :: raw(:,:)
  end subroutine extract_var_real32_2d

  module subroutine extract_var_real64_2d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    real(real64), pointer, intent(out) :: raw(:,:)
  end subroutine extract_var_real64_2d

  module subroutine extract_var_int8_3d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int8), pointer, intent(out) :: raw(:,:,:)
  end subroutine extract_var_int8_3d

  module subroutine extract_var_int16_3d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int16), pointer, intent(out) :: raw(:,:,:)
  end subroutine extract_var_int16_3d

  module subroutine extract_var_int32_3d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int32), pointer, intent(out) :: raw(:,:,:)
  end subroutine extract_var_int32_3d

  module subroutine extract_var_int64_3d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int64), pointer, intent(out) :: raw(:,:,:)
  end subroutine extract_var_int64_3d

  module subroutine extract_var_real32_3d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    real(real32), pointer, intent(out) :: raw(:,:,:)
  end subroutine extract_var_real32_3d

  module subroutine extract_var_real64_3d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    real(real64), pointer, intent(out) :: raw(:,:,:)
  end subroutine extract_var_real64_3d

  module subroutine extract_var_int8_4d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int8), pointer, intent(out) :: raw(:,:,:,:)
  end subroutine extract_var_int8_4d

  module subroutine extract_var_int16_4d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int16), pointer, intent(out) :: raw(:,:,:,:)
  end subroutine extract_var_int16_4d

  module subroutine extract_var_int32_4d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int32), pointer, intent(out) :: raw(:,:,:,:)
  end subroutine extract_var_int32_4d

  module subroutine extract_var_int64_4d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int64), pointer, intent(out) :: raw(:,:,:,:)
  end subroutine extract_var_int64_4d

  module subroutine extract_var_real32_4d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    real(real32), pointer, intent(out) :: raw(:,:,:,:)
  end subroutine extract_var_real32_4d

  module subroutine extract_var_real64_4d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    real(real64), pointer, intent(out) :: raw(:,:,:,:)
  end subroutine extract_var_real64_4d

  module subroutine extract_var_int8_5d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int8), pointer, intent(out) :: raw(:,:,:,:,:)
  end subroutine extract_var_int8_5d

  module subroutine extract_var_int16_5d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int16), pointer, intent(out) :: raw(:,:,:,:,:)
  end subroutine extract_var_int16_5d

  module subroutine extract_var_int32_5d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int32), pointer, intent(out) :: raw(:,:,:,:,:)
  end subroutine extract_var_int32_5d

  module subroutine extract_var_int64_5d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    integer(int64), pointer, intent(out) :: raw(:,:,:,:,:)
  end subroutine extract_var_int64_5d

  module subroutine extract_var_real32_5d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    real(real32), pointer, intent(out) :: raw(:,:,:,:,:)
  end subroutine extract_var_real32_5d

  module subroutine extract_var_real64_5d(var, raw)
    type(netcdf_variable), target, intent(in) :: var
    real(real64), pointer, intent(out) :: raw(:,:,:,:,:)
  end subroutine extract_var_real64_5d


  module function new_file(vars, atts) result(file)
    type(netcdf_variable), intent(in) :: vars(:)
    type(netcdf_attribute), intent(in), optional :: atts(:)
    type(netcdf_file) :: file
  end function new_file

  module function from_netcdf_grp(path) result(file)
    character(len=*), intent(in) :: path
    type(netcdf_file) :: file
  end function from_netcdf_grp

  module subroutine to_netcdf_grp(file)
    type(netcdf_file), target, intent(in) :: file
  end subroutine to_netcdf_grp

  module subroutine write_formatted_dim( &
    & dim, unit, iotype, v_list, iostat, iomsg)
    class(netcdf_dimension), intent(in) :: dim
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
  end subroutine write_formatted_dim

  module subroutine write_formatted_att( &
    & att, unit, iotype, v_list, iostat, iomsg)
    class(netcdf_attribute), intent(in) :: att
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
  end subroutine write_formatted_att

  module subroutine write_formatted_var( &
    & var, unit, iotype, v_list, iostat, iomsg)
    class(netcdf_variable), intent(in) :: var
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
  end subroutine write_formatted_var
end interface

end module module_fortran_interface
