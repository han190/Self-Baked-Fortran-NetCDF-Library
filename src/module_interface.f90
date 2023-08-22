module module_interface

use :: iso_fortran_env
use :: iso_c_binding
use :: module_c_interface
implicit none

public :: attribute_type, dimension_type
public :: operator(.dim.), operator(.att.)
public :: dims, atts
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
type, abstract :: abstract_variable_type
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
end interface

end module module_interface
