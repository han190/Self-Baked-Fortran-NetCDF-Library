module module_interface

use :: iso_fortran_env
use :: iso_c_binding
use :: module_c_interface
implicit none

public :: attribute_type, dimension_type, variable_type
public :: operator(.dim.), operator(.att.)
public :: dims, atts, data_array
public :: write (formatted)
private

!> NetCDF Data Model
!> -----------------

!> The data model follows the netCDF data model introduced
!> https://docs.unidata.ucar.edu/netcdf-c/current/netcdf_data_model.html

!> Currently supported data types
!> - [x] char
!> - [ ] byte
!> - [x] short
!> - [x] int
!> - [x] int64
!> - [x] float
!> - [x] double
!> - [ ] unsigned byte
!> - [ ] unsigned short
!> - [ ] unsigned int
!> - [ ] unsigned int64
!> - [ ] string

!> Abstract NetCDF type
type, abstract :: netcdf_type
  integer(c_int) :: ID
  character(len=:), allocatable :: name
end type netcdf_type

!> Dimension type
type, extends(netcdf_type) :: dimension_type
  integer(c_size_t) :: len = 0
  logical :: is_unlim = .false.
end type dimension_type

!> Attribute type
type, abstract, extends(netcdf_type) :: abstract_attribute_type
  integer(c_size_t) :: len = 0
  integer(c_int) :: type = 0
end type abstract_attribute_type

!> integer(int8) attribute type
type, extends(abstract_attribute_type) :: attribute_int8_type
  integer(int8), allocatable :: vals(:)
end type attribute_int8_type

!> integer(int16) attribute type
type, extends(abstract_attribute_type) :: attribute_int16_type
  integer(int16), allocatable :: vals(:)
end type attribute_int16_type

!> integer(int32) attribute type
type, extends(abstract_attribute_type) :: attribute_int32_type
  integer(int32), allocatable :: vals(:)
end type attribute_int32_type

!> integer(int64) attribute type
type, extends(abstract_attribute_type) :: attribute_int64_type
  integer(int64), allocatable :: vals(:)
end type attribute_int64_type

!> real(real32) attribute type
type, extends(abstract_attribute_type) :: attribute_real32_type
  real(real32), allocatable :: vals(:)
end type attribute_real32_type

!> real(real64) attribute type
type, extends(abstract_attribute_type) :: attribute_real64_type
  real(real64), allocatable :: vals(:)
end type attribute_real64_type

!> character attribute type
type, extends(abstract_attribute_type) :: attribute_char_type
  character(len=:), allocatable :: vals(:)
end type attribute_char_type

!> Attributes_type
type :: attribute_type
  class(abstract_attribute_type), allocatable :: att
end type attribute_type

!> Abstract variable type
type, abstract, extends(netcdf_type) :: abstract_variable_type
  type(dimension_type), allocatable :: dims(:)
  type(attribute_type), allocatable :: atts(:)
  integer(c_int), pointer  :: grpID => null()
  integer(c_int) :: type = 0
end type abstract_variable_type

!> integer(int8) attribute type
type, extends(abstract_variable_type) :: variable_int8_type
  integer(int8), allocatable :: vals(:)
end type variable_int8_type

!> integer(int16) attribute type
type, extends(abstract_variable_type) :: variable_int16_type
  integer(int16), allocatable :: vals(:)
end type variable_int16_type

!> integer(int32) attribute type
type, extends(abstract_variable_type) :: variable_int32_type
  integer(int32), allocatable :: vals(:)
end type variable_int32_type

!> integer(int64) attribute type
type, extends(abstract_variable_type) :: variable_int64_type
  integer(int64), allocatable :: vals(:)
end type variable_int64_type

!> real(real32) attribute type
type, extends(abstract_variable_type) :: variable_real32_type
  real(real32), allocatable :: vals(:)
end type variable_real32_type

!> real(real64) attribute type
type, extends(abstract_variable_type) :: variable_real64_type
  real(real64), allocatable :: vals(:)
end type variable_real64_type

!> character variable type
type, extends(abstract_variable_type) :: variable_char_type
  character(len=:), allocatable :: vals(:)
end type variable_char_type

!> Variable type
type :: variable_type
  class(abstract_variable_type), allocatable :: var
end type variable_type

!> Group type
type, extends(netcdf_type) :: group_type
  type(dimension_type), allocatable :: dims(:)
  type(attribute_type), allocatable :: atts(:)
  type(variable_type), allocatable :: vars(:)
  type(group_type), allocatable :: grps(:)
  integer(c_int) :: mode = 0
  integer(c_int) :: fmt = 0
end type group_type

!> File type (root group)
type, extends(group_type) :: file_type
  character(len=:), allocatable :: filename
end type file_type

!> Dimension constructor
interface operator(.dim.)
  module procedure :: new_dim
end interface operator(.dim.)

!> Dimensions constructor
interface dims
  module procedure :: new_dims
  module procedure :: new_dims_unlim
end interface dims

!> Attribute constructor
interface operator(.att.)
  module procedure :: new_att_scal_int8
  module procedure :: new_att_scal_int16
  module procedure :: new_att_scal_int32
  module procedure :: new_att_scal_int64
  module procedure :: new_att_scal_real32
  module procedure :: new_att_scal_real64
  module procedure :: new_att_scal_char
  module procedure :: new_att_vec_int8
  module procedure :: new_att_vec_int16
  module procedure :: new_att_vec_int32
  module procedure :: new_att_vec_int64
  module procedure :: new_att_vec_real32
  module procedure :: new_att_vec_real64
end interface operator(.att.)

!> Attributes constructor
interface atts
  module procedure :: new_atts
end interface atts

!> Variable constructor
interface data_array
  module procedure :: new_variable_int8_1d
  module procedure :: new_variable_noatt_int8_1d
  module procedure :: new_variable_int16_1d
  module procedure :: new_variable_noatt_int16_1d
  module procedure :: new_variable_int32_1d
  module procedure :: new_variable_noatt_int32_1d
  module procedure :: new_variable_int64_1d
  module procedure :: new_variable_noatt_int64_1d
  module procedure :: new_variable_real32_1d
  module procedure :: new_variable_noatt_real32_1d
  module procedure :: new_variable_real64_1d
  module procedure :: new_variable_noatt_real64_1d
  module procedure :: new_variable_int8_2d
  module procedure :: new_variable_noatt_int8_2d
  module procedure :: new_variable_int16_2d
  module procedure :: new_variable_noatt_int16_2d
  module procedure :: new_variable_int32_2d
  module procedure :: new_variable_noatt_int32_2d
  module procedure :: new_variable_int64_2d
  module procedure :: new_variable_noatt_int64_2d
  module procedure :: new_variable_real32_2d
  module procedure :: new_variable_noatt_real32_2d
  module procedure :: new_variable_real64_2d
  module procedure :: new_variable_noatt_real64_2d
  module procedure :: new_variable_int8_3d
  module procedure :: new_variable_noatt_int8_3d
  module procedure :: new_variable_int16_3d
  module procedure :: new_variable_noatt_int16_3d
  module procedure :: new_variable_int32_3d
  module procedure :: new_variable_noatt_int32_3d
  module procedure :: new_variable_int64_3d
  module procedure :: new_variable_noatt_int64_3d
  module procedure :: new_variable_real32_3d
  module procedure :: new_variable_noatt_real32_3d
  module procedure :: new_variable_real64_3d
  module procedure :: new_variable_noatt_real64_3d
  module procedure :: new_variable_int8_4d
  module procedure :: new_variable_noatt_int8_4d
  module procedure :: new_variable_int16_4d
  module procedure :: new_variable_noatt_int16_4d
  module procedure :: new_variable_int32_4d
  module procedure :: new_variable_noatt_int32_4d
  module procedure :: new_variable_int64_4d
  module procedure :: new_variable_noatt_int64_4d
  module procedure :: new_variable_real32_4d
  module procedure :: new_variable_noatt_real32_4d
  module procedure :: new_variable_real64_4d
  module procedure :: new_variable_noatt_real64_4d
  module procedure :: new_variable_int8_5d
  module procedure :: new_variable_noatt_int8_5d
  module procedure :: new_variable_int16_5d
  module procedure :: new_variable_noatt_int16_5d
  module procedure :: new_variable_int32_5d
  module procedure :: new_variable_noatt_int32_5d
  module procedure :: new_variable_int64_5d
  module procedure :: new_variable_noatt_int64_5d
  module procedure :: new_variable_real32_5d
  module procedure :: new_variable_noatt_real32_5d
  module procedure :: new_variable_real64_5d
  module procedure :: new_variable_noatt_real64_5d
  module procedure :: new_variable_int8_6d
  module procedure :: new_variable_noatt_int8_6d
  module procedure :: new_variable_int16_6d
  module procedure :: new_variable_noatt_int16_6d
  module procedure :: new_variable_int32_6d
  module procedure :: new_variable_noatt_int32_6d
  module procedure :: new_variable_int64_6d
  module procedure :: new_variable_noatt_int64_6d
  module procedure :: new_variable_real32_6d
  module procedure :: new_variable_noatt_real32_6d
  module procedure :: new_variable_real64_6d
  module procedure :: new_variable_noatt_real64_6d
end interface data_array

!> IO
interface write (formatted)
module procedure :: write_formatted_dim
module procedure :: write_formatted_att
module procedure :: write_formatted_var
end interface write (formatted)

!> Interfaces to submodules
interface
  !> submodule utility
  !> -----------------

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

  !> submodule dimension
  !> -------------------

  module pure function new_dim(name, len) result(dim)
    character(len=*), intent(in) :: name
    integer, intent(in) :: len
    type(dimension_type) :: dim
  end function new_dim

  module function new_dims(dims) result(ret)
    type(dimension_type), intent(in) :: dims(:)
    type(dimension_type), allocatable :: ret(:)
  end function new_dims

  module function new_dims_unlim(dims, unlim_dim) result(ret)
    type(dimension_type), intent(in) :: dims(:)
    integer, intent(in) :: unlim_dim
    type(dimension_type), allocatable :: ret(:)
  end function new_dims_unlim

  module pure function shape_dims(dims) result(ret)
    type(dimension_type), intent(in) :: dims(:)
    integer, allocatable :: ret(:)
  end function shape_dims

  !> submodule attribute
  !> -------------------

  module pure function new_att_vec_int8(name, vals) result(att)
    character(*), intent(in) :: name
    integer(int8), intent(in) :: vals(:)
    type(attribute_type) :: att
  end function new_att_vec_int8

  module pure function new_att_vec_int16(name, vals) result(att)
    character(*), intent(in) :: name
    integer(int16), intent(in) :: vals(:)
    type(attribute_type) :: att
  end function new_att_vec_int16

  module pure function new_att_vec_int32(name, vals) result(att)
    character(*), intent(in) :: name
    integer(int32), intent(in) :: vals(:)
    type(attribute_type) :: att
  end function new_att_vec_int32

  module pure function new_att_vec_int64(name, vals) result(att)
    character(*), intent(in) :: name
    integer(int64), intent(in) :: vals(:)
    type(attribute_type) :: att
  end function new_att_vec_int64

  module pure function new_att_vec_real32(name, vals) result(att)
    character(*), intent(in) :: name
    real(real32), intent(in) :: vals(:)
    type(attribute_type) :: att
  end function new_att_vec_real32

  module pure function new_att_vec_real64(name, vals) result(att)
    character(*), intent(in) :: name
    real(real64), intent(in) :: vals(:)
    type(attribute_type) :: att
  end function new_att_vec_real64

  module pure function new_att_scal_int8(name, val) result(att)
    character(*), intent(in) :: name
    integer(int8), intent(in) :: val
    type(attribute_type) :: att
  end function new_att_scal_int8

  module pure function new_att_scal_int16(name, val) result(att)
    character(*), intent(in) :: name
    integer(int16), intent(in) :: val
    type(attribute_type) :: att
  end function new_att_scal_int16

  module pure function new_att_scal_int32(name, val) result(att)
    character(*), intent(in) :: name
    integer(int32), intent(in) :: val
    type(attribute_type) :: att
  end function new_att_scal_int32

  module pure function new_att_scal_int64(name, val) result(att)
    character(*), intent(in) :: name
    integer(int64), intent(in) :: val
    type(attribute_type) :: att
  end function new_att_scal_int64

  module pure function new_att_scal_real32(name, val) result(att)
    character(*), intent(in) :: name
    real(real32), intent(in) :: val
    type(attribute_type) :: att
  end function new_att_scal_real32

  module pure function new_att_scal_real64(name, val) result(att)
    character(*), intent(in) :: name
    real(real64), intent(in) :: val
    type(attribute_type) :: att
  end function new_att_scal_real64

  module pure function new_att_scal_char(name, val) result(att)
    character(len=*), intent(in) :: name, val
    type(attribute_type) :: att
  end function new_att_scal_char

  module function new_atts(atts) result(ret)
    type(attribute_type), intent(in) :: atts(:)
    type(attribute_type), allocatable :: ret(:)
  end function new_atts

  !> submodule variable
  !> ------------------

  !> variable constructor for integer(int8), rank(0)
  module function new_variable_int8_1d( &
    & data, name, dims, atts) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int8_1d

  !> variable constructor for integer(int8), rank(0)
  module function new_variable_noatt_int8_1d( &
    & data, name, dims) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int8_1d

  !> variable constructor for integer(int16), rank(0)
  module function new_variable_int16_1d( &
    & data, name, dims, atts) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int16_1d

  !> variable constructor for integer(int16), rank(0)
  module function new_variable_noatt_int16_1d( &
    & data, name, dims) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int16_1d

  !> variable constructor for integer(int32), rank(0)
  module function new_variable_int32_1d( &
    & data, name, dims, atts) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int32_1d

  !> variable constructor for integer(int32), rank(0)
  module function new_variable_noatt_int32_1d( &
    & data, name, dims) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int32_1d

  !> variable constructor for integer(int64), rank(0)
  module function new_variable_int64_1d( &
    & data, name, dims, atts) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int64_1d

  !> variable constructor for integer(int64), rank(0)
  module function new_variable_noatt_int64_1d( &
    & data, name, dims) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int64_1d

  !> variable constructor for real(real32), rank(0)
  module function new_variable_real32_1d( &
    & data, name, dims, atts) result(var)
    real(real32), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real32_1d

  !> variable constructor for real(real32), rank(0)
  module function new_variable_noatt_real32_1d( &
    & data, name, dims) result(var)
    real(real32), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real32_1d

  !> variable constructor for real(real64), rank(0)
  module function new_variable_real64_1d( &
    & data, name, dims, atts) result(var)
    real(real64), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real64_1d

  !> variable constructor for real(real64), rank(0)
  module function new_variable_noatt_real64_1d( &
    & data, name, dims) result(var)
    real(real64), target, contiguous, intent(in) :: data(:)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real64_1d

  !> variable constructor for integer(int8), rank(1)
  module function new_variable_int8_2d( &
    & data, name, dims, atts) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int8_2d

  !> variable constructor for integer(int8), rank(1)
  module function new_variable_noatt_int8_2d( &
    & data, name, dims) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int8_2d

  !> variable constructor for integer(int16), rank(1)
  module function new_variable_int16_2d( &
    & data, name, dims, atts) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int16_2d

  !> variable constructor for integer(int16), rank(1)
  module function new_variable_noatt_int16_2d( &
    & data, name, dims) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int16_2d

  !> variable constructor for integer(int32), rank(1)
  module function new_variable_int32_2d( &
    & data, name, dims, atts) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int32_2d

  !> variable constructor for integer(int32), rank(1)
  module function new_variable_noatt_int32_2d( &
    & data, name, dims) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int32_2d

  !> variable constructor for integer(int64), rank(1)
  module function new_variable_int64_2d( &
    & data, name, dims, atts) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int64_2d

  !> variable constructor for integer(int64), rank(1)
  module function new_variable_noatt_int64_2d( &
    & data, name, dims) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int64_2d

  !> variable constructor for real(real32), rank(1)
  module function new_variable_real32_2d( &
    & data, name, dims, atts) result(var)
    real(real32), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real32_2d

  !> variable constructor for real(real32), rank(1)
  module function new_variable_noatt_real32_2d( &
    & data, name, dims) result(var)
    real(real32), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real32_2d

  !> variable constructor for real(real64), rank(1)
  module function new_variable_real64_2d( &
    & data, name, dims, atts) result(var)
    real(real64), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real64_2d

  !> variable constructor for real(real64), rank(1)
  module function new_variable_noatt_real64_2d( &
    & data, name, dims) result(var)
    real(real64), target, contiguous, intent(in) :: data(:, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real64_2d

  !> variable constructor for integer(int8), rank(2)
  module function new_variable_int8_3d( &
    & data, name, dims, atts) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int8_3d

  !> variable constructor for integer(int8), rank(2)
  module function new_variable_noatt_int8_3d( &
    & data, name, dims) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int8_3d

  !> variable constructor for integer(int16), rank(2)
  module function new_variable_int16_3d( &
    & data, name, dims, atts) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int16_3d

  !> variable constructor for integer(int16), rank(2)
  module function new_variable_noatt_int16_3d( &
    & data, name, dims) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int16_3d

  !> variable constructor for integer(int32), rank(2)
  module function new_variable_int32_3d( &
    & data, name, dims, atts) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int32_3d

  !> variable constructor for integer(int32), rank(2)
  module function new_variable_noatt_int32_3d( &
    & data, name, dims) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int32_3d

  !> variable constructor for integer(int64), rank(2)
  module function new_variable_int64_3d( &
    & data, name, dims, atts) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int64_3d

  !> variable constructor for integer(int64), rank(2)
  module function new_variable_noatt_int64_3d( &
    & data, name, dims) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int64_3d

  !> variable constructor for real(real32), rank(2)
  module function new_variable_real32_3d( &
    & data, name, dims, atts) result(var)
    real(real32), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real32_3d

  !> variable constructor for real(real32), rank(2)
  module function new_variable_noatt_real32_3d( &
    & data, name, dims) result(var)
    real(real32), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real32_3d

  !> variable constructor for real(real64), rank(2)
  module function new_variable_real64_3d( &
    & data, name, dims, atts) result(var)
    real(real64), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real64_3d

  !> variable constructor for real(real64), rank(2)
  module function new_variable_noatt_real64_3d( &
    & data, name, dims) result(var)
    real(real64), target, contiguous, intent(in) :: data(:, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real64_3d

  !> variable constructor for integer(int8), rank(3)
  module function new_variable_int8_4d( &
    & data, name, dims, atts) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int8_4d

  !> variable constructor for integer(int8), rank(3)
  module function new_variable_noatt_int8_4d( &
    & data, name, dims) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int8_4d

  !> variable constructor for integer(int16), rank(3)
  module function new_variable_int16_4d( &
    & data, name, dims, atts) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int16_4d

  !> variable constructor for integer(int16), rank(3)
  module function new_variable_noatt_int16_4d( &
    & data, name, dims) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int16_4d

  !> variable constructor for integer(int32), rank(3)
  module function new_variable_int32_4d( &
    & data, name, dims, atts) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int32_4d

  !> variable constructor for integer(int32), rank(3)
  module function new_variable_noatt_int32_4d( &
    & data, name, dims) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int32_4d

  !> variable constructor for integer(int64), rank(3)
  module function new_variable_int64_4d( &
    & data, name, dims, atts) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int64_4d

  !> variable constructor for integer(int64), rank(3)
  module function new_variable_noatt_int64_4d( &
    & data, name, dims) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int64_4d

  !> variable constructor for real(real32), rank(3)
  module function new_variable_real32_4d( &
    & data, name, dims, atts) result(var)
    real(real32), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real32_4d

  !> variable constructor for real(real32), rank(3)
  module function new_variable_noatt_real32_4d( &
    & data, name, dims) result(var)
    real(real32), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real32_4d

  !> variable constructor for real(real64), rank(3)
  module function new_variable_real64_4d( &
    & data, name, dims, atts) result(var)
    real(real64), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real64_4d

  !> variable constructor for real(real64), rank(3)
  module function new_variable_noatt_real64_4d( &
    & data, name, dims) result(var)
    real(real64), target, contiguous, intent(in) :: data(:, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real64_4d

  !> variable constructor for integer(int8), rank(4)
  module function new_variable_int8_5d( &
    & data, name, dims, atts) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int8_5d

  !> variable constructor for integer(int8), rank(4)
  module function new_variable_noatt_int8_5d( &
    & data, name, dims) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int8_5d

  !> variable constructor for integer(int16), rank(4)
  module function new_variable_int16_5d( &
    & data, name, dims, atts) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int16_5d

  !> variable constructor for integer(int16), rank(4)
  module function new_variable_noatt_int16_5d( &
    & data, name, dims) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int16_5d

  !> variable constructor for integer(int32), rank(4)
  module function new_variable_int32_5d( &
    & data, name, dims, atts) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int32_5d

  !> variable constructor for integer(int32), rank(4)
  module function new_variable_noatt_int32_5d( &
    & data, name, dims) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int32_5d

  !> variable constructor for integer(int64), rank(4)
  module function new_variable_int64_5d( &
    & data, name, dims, atts) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int64_5d

  !> variable constructor for integer(int64), rank(4)
  module function new_variable_noatt_int64_5d( &
    & data, name, dims) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int64_5d

  !> variable constructor for real(real32), rank(4)
  module function new_variable_real32_5d( &
    & data, name, dims, atts) result(var)
    real(real32), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real32_5d

  !> variable constructor for real(real32), rank(4)
  module function new_variable_noatt_real32_5d( &
    & data, name, dims) result(var)
    real(real32), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real32_5d

  !> variable constructor for real(real64), rank(4)
  module function new_variable_real64_5d( &
    & data, name, dims, atts) result(var)
    real(real64), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real64_5d

  !> variable constructor for real(real64), rank(4)
  module function new_variable_noatt_real64_5d( &
    & data, name, dims) result(var)
    real(real64), target, contiguous, intent(in) :: data(:, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real64_5d

  !> variable constructor for integer(int8), rank(5)
  module function new_variable_int8_6d( &
    & data, name, dims, atts) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int8_6d

  !> variable constructor for integer(int8), rank(5)
  module function new_variable_noatt_int8_6d( &
    & data, name, dims) result(var)
    integer(int8), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int8_6d

  !> variable constructor for integer(int16), rank(5)
  module function new_variable_int16_6d( &
    & data, name, dims, atts) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int16_6d

  !> variable constructor for integer(int16), rank(5)
  module function new_variable_noatt_int16_6d( &
    & data, name, dims) result(var)
    integer(int16), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int16_6d

  !> variable constructor for integer(int32), rank(5)
  module function new_variable_int32_6d( &
    & data, name, dims, atts) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int32_6d

  !> variable constructor for integer(int32), rank(5)
  module function new_variable_noatt_int32_6d( &
    & data, name, dims) result(var)
    integer(int32), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int32_6d

  !> variable constructor for integer(int64), rank(5)
  module function new_variable_int64_6d( &
    & data, name, dims, atts) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_int64_6d

  !> variable constructor for integer(int64), rank(5)
  module function new_variable_noatt_int64_6d( &
    & data, name, dims) result(var)
    integer(int64), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_int64_6d

  !> variable constructor for real(real32), rank(5)
  module function new_variable_real32_6d( &
    & data, name, dims, atts) result(var)
    real(real32), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real32_6d

  !> variable constructor for real(real32), rank(5)
  module function new_variable_noatt_real32_6d( &
    & data, name, dims) result(var)
    real(real32), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real32_6d

  !> variable constructor for real(real64), rank(5)
  module function new_variable_real64_6d( &
    & data, name, dims, atts) result(var)
    real(real64), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(attribute_type), intent(in) :: atts(:)
    type(variable_type) :: var
  end function new_variable_real64_6d

  !> variable constructor for real(real64), rank(5)
  module function new_variable_noatt_real64_6d( &
    & data, name, dims) result(var)
    real(real64), target, contiguous, intent(in) :: data(:, :, :, :, :, :)
    character(len=*), intent(in) :: name
    type(dimension_type), intent(in) :: dims(:)
    type(variable_type) :: var
  end function new_variable_noatt_real64_6d

  !> submodule I/O
  !> -------------

  module subroutine write_formatted_dim( &
    & dim, unit, iotype, v_list, iostat, iomsg)
    class(dimension_type), intent(in) :: dim
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
  end subroutine write_formatted_dim

  module subroutine write_formatted_att( &
    & att, unit, iotype, v_list, iostat, iomsg)
    class(attribute_type), intent(in) :: att
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
  end subroutine write_formatted_att

  module subroutine write_formatted_var( &
    & var, unit, iotype, v_list, iostat, iomsg)
    class(variable_type), intent(in) :: var
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg
  end subroutine write_formatted_var
end interface

end module module_interface
