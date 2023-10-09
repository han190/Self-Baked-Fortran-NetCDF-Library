# To develop an intermediate NetCDF interface in the pre-Fortran202Y era

## Data structure

```fortran
type, abstract :: netcdf_abstract
  integer(c_int) :: ID
  character(len=:), allocatable :: name
end type netcdf_abastract

type, extends(netcdf_abstract) :: netcdf_dimension
  integer(c_size_t) :: length
  logical :: is_unlimited
end type netcdf_dimension

type, extends(netcdf_abstract), abstract :: netcdf_abstract_attribute
  integer(c_size_t) :: length = 0
  integer(c_int) :: type = 0
end type netcdf_abstract_attribute

type, extends(netcdf_abstract_attribute) :: netcdf_integer_attribute(kind)
  integer, kind :: kind
  integer(kind=kind), allocatable :: values(:)
end type netcdf_integer_attribute

type, extends(netcdf_abstract_attribute) :: netcdf_real_attribute(kind)
  integer, kind :: kind
  real(kind=kind), allocatable :: values(:)
end type netcdf_real_attribute

type, extends(netcdf_abstract_attribute) :: netcdf_character_attribute(kind)
  integer, kind :: kind
  character(kind=kind, len=:), allocatable :: values(:)
end type netcdf_character_attribute

type :: netcdf_attribute
  class(netcdf_abstract_attribute), allocatable :: attribute
end type netcdf_attribute
```