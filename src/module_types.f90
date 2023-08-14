module module_types

  use iso_c_binding, only: c_int, c_size_t
  implicit none
  public

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

  !> Dimension pointer type
  type :: dimension_pointer_type
    type(dimension_type), pointer :: ptr => null()
  end type dimension_pointer_type

  !> Dimensions type
  type :: dimensions_type
    !> Rank
    integer :: length
    !> Pointers
    type(dimension_pointer_type), allocatable :: ptrs(:)
  end type dimensions_type

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
    type(dimensions_type) :: dims
    !> attribute
    type(attribute_type), allocatable :: atts(:)
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
    type(dimension_type), allocatable :: dims(:)
    !> attributes
    type(attribute_type), allocatable :: atts(:)
    !> variables
    type(variable_type), allocatable :: vars(:)
    !> mode (read, write, etc.)
    integer(c_int) :: mode = 0
    !> format (NetCDF3, NetCDF4, etc.)
    integer(c_int) :: format = 0
    !> filename
    character(len=:), allocatable :: filename
  end type group_type

  !> Self-Baked Dictionary
  !> ---------------------

  !> Key-value pair
  type :: pair_type
    !> key (character)
    character(len=:), allocatable :: key
    !> value
    class(netcdf_type), allocatable :: value
  end type pair_type

  !> Node type
  type :: node_type
    !> item
    type(pair_type) :: pair
    !> next
    type(node_type), pointer :: next => null()
  end type node_type

  !> List type
  type :: list_type
    !> length
    integer :: length = 0
    !> head
    type(node_type), pointer :: head => null()
    !> tail
    type(node_type), pointer :: tail => null()
  end type list_type

  !> Dictionary type (PDT)
  type :: dictionary_type(len)
    !> length of buckets
    integer, len :: len
    !> length of dictionary
    integer :: length
    !> buckets
    type(list_type) :: items(len)
  end type dictionary_type

end module module_types