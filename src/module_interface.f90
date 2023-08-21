module module_interface

use :: iso_fortran_env
use :: iso_c_binding
implicit none

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
  !> ID of file, grp, var, dim, att
  integer(c_int) :: ID
  !> name of file, grp, var, dim, att
  character(len=:), allocatable :: name
end type netcdf_type

!> Dimension type
type, extends(netcdf_type) :: dimension_type
  !> length
  integer(c_size_t) :: len = 0
  !> is unlimited
  logical :: is_unlim = .false.
end type dimension_type

!> Attribute type
type, abstract, extends(netcdf_type) :: abstract_attribute_type
  !> length
  integer(c_size_t) :: len = 0
  !> type (default: not a type)
  integer(c_int) :: type = 0
end type abstract_attribute_type

!> integer(int8) attribute type
type, extends(abstract_attribute_type) :: attribute_int8_type
  !> values
  integer(int8), allocatable :: vals(:)
end type attribute_int8_type

!> integer(int16) attribute type
type, extends(abstract_attribute_type) :: attribute_int16_type
  !> values
  integer(int16), allocatable :: vals(:)
end type attribute_int16_type

!> integer(int32) attribute type
type, extends(abstract_attribute_type) :: attribute_int32_type
  !> values
  integer(int32), allocatable :: vals(:)
end type attribute_int32_type

!> integer(int64) attribute type
type, extends(abstract_attribute_type) :: attribute_int64_type
  !> values
  integer(int64), allocatable :: vals(:)
end type attribute_int64_type

!> real(real32) attribute type
type, extends(abstract_attribute_type) :: attribute_real32_type
  !> values
  real(real32), allocatable :: vals(:)
end type attribute_real32_type

!> real(real64) attribute type
type, extends(abstract_attribute_type) :: attribute_real64_type
  !> values
  real(real64), allocatable :: vals(:)
end type attribute_real64_type

!> character attribute type
type, extends(abstract_attribute_type) :: attribute_char_type
  !> values
  character(len=:), allocatable :: vals(:)
end type attribute_char_type

!> Attributes_type
type :: attribute_type
  !> attribute
  class(abstract_attribute_type), allocatable :: att
end type attribute_type

!> Abstract variable type
type, abstract :: abstract_variable_type
  !> dimensions
  type(dimension_type), allocatable :: dims(:)
  !> attributes
  type(attribute_type), allocatable :: atts(:)
  !> group ID
  integer(c_int), pointer  :: grpID => null()
  !> type
  integer(c_int) :: type = 0
end type abstract_variable_type

!> integer(int8) attribute type
type, extends(abstract_variable_type) :: variable_int8_type
  !> values
  integer(int8), allocatable :: vals(:)
end type variable_int8_type

!> integer(int16) attribute type
type, extends(abstract_variable_type) :: variable_int16_type
  !> values
  integer(int16), allocatable :: vals(:)
end type variable_int16_type

!> integer(int32) attribute type
type, extends(abstract_variable_type) :: variable_int32_type
  !> values
  integer(int32), allocatable :: vals(:)
end type variable_int32_type

!> integer(int64) attribute type
type, extends(abstract_variable_type) :: variable_int64_type
  !> values
  integer(int64), allocatable :: vals(:)
end type variable_int64_type

!> real(real32) attribute type
type, extends(abstract_variable_type) :: variable_real32_type
  !> values
  real(real32), allocatable :: vals(:)
end type variable_real32_type

!> real(real64) attribute type
type, extends(abstract_variable_type) :: variable_real64_type
  !> values
  real(real64), allocatable :: vals(:)
end type variable_real64_type

!> character variable type
type, extends(abstract_variable_type) :: variable_char_type
  !> values
  character(len=:), allocatable :: vals(:)
end type variable_char_type

!> Variable type
type :: variable_type
  class(abstract_variable_type), allocatable :: var
end type variable_type

!> Group type
type, extends(netcdf_type) :: group_type
  !> dimensions
  type(dimension_type), allocatable :: dims(:)
  !> attributes
  type(attribute_type), allocatable :: atts(:)
  !> variables
  type(variable_type), allocatable :: vars(:)
  !> subgroups
  type(group_type), allocatable :: grps(:)
  !> mode (read, write, etc.)
  integer(c_int) :: mode = 0
  !> format (NetCDF3, NetCDF4, etc.)
  integer(c_int) :: fmt = 0
end type group_type

!> File type (root group)
type, extends(group_type) :: file_type
  !> filename
  character(len=:), allocatable :: filename
end type file_type

! !> Dimension constructor
! interface operator(.dim.)
!   module procedure :: new_dimension
! end interface operator(.dim.)

! !> Attribute constructor
! interface operator(.att.)
!   module procedure :: new_attribute
! end interface operator(.att.)

end module module_interface
